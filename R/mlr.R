# load required R packages
library(data.table)
library(dplyr)
library(mlr)
library(sva)
library(stringr)
library(randomForest)


mlr_pred <- function(lrn.name="classif.randomForest", refdat.path, refdat.name, ext.dat, num.cores, mode){
  # Train a random forest model to predict cell types using one reference dataset
  #
  # Args:
  #   lrn.name: random.forest
  #   train.dat: part of the reference dataset with cell as rows and feature as columns,
  #   the last column is target(celltype)
  #   test.dat: held-out part of the reference dataset for evaluating, cell by feature,
  #   last column is target
  #   ext.dat: query dataset datframe in format of cell by feature

  #
  # Returns:
  #   a list containing trained random forest model, prediction of training part and held-out part of reference dataset,
  #    prediction of query dataset, evaluation result using in trainig
  #   and test parts of the trainng dataset.



  print(paste0("Training a classifier from ", refdat.name))

  # partition the reference dataset into training and held-out part
  # batch correction between reference and query datasets

  dat.folds <- dat_partition(refdat.path, ext.dat)
  train.dat <- dat.folds[['train']]
  test.dat <- dat.folds[['test']]
  ext.dat <- dat.folds[['ext']]

  if(mode == "debug"){ # use a small number of cells to debug the workflow
        print ("Activate debug mode, use 1000 cells for training")
        train.dat <- train.dat[sample(1:nrow(train.dat), 1000), ]
  }


  # train a random forest classifier using the training part of reference dataset
  print (paste("Training a random forest classifier for", refdat.name))
  task <- mlr::makeClassifTask(id = lrn.name, dat = train.dat, target = "target")
  lrn <- mlr::makeLearner(lrn.name, predict.type = "prob")
  mod <- mlr::train(lrn, task)

  # apply the trained model to training, test part of the reference dataset and query dataset
  print (paste("Predict query dataset using trained classifier for ", refdat.name))
  pred.train <- predict(mod, task = task)
  pred.test <- predict(mod, newdat = test.dat)
  pred.ext <- predict(mod, newdat = ext.dat)

  # evaluation of performance within training part and test part of reference dataset
  measure.list <- list(mlr::multiclass.aunp,  mlr::acc, mlr::timepredict)
  perf.train <- mlr::performance(pred.train, measures = measure.list)
  perf.test <- mlr::performance(pred.test, measures = measure.list)

  print("Prediction evaluation using held-out samples in reference dataset")
  print("-----------------------------------------------------------------")
  print (perf.test)

  #return (list(mod, pred.train, pred.test, pred.ext, perf.train, perf.test))
  #return the probabilities of query cells across all cell types in reference dataset

  ref.index <- stringr::str_extract(refdat.name, "Ref[0-9]")
  colnames(pred.ext$data) <- gsub("prob", ref.index,  colnames(pred.ext$data))
  return (pred.ext$data[, !grepl("response", colnames(pred.ext$data))])

}




dat_partition <- function(refdat.path, ext.dat){
  # This function performs:
  # 1. Batch correction between the query dataset and reference dataset
  # 2. partition of reference dataset into training and held-out parts
  #
  # Args:
  #   train.dat.path: reference dat path in RDS format
  #   ext.dat: query dataset datframe in format of cell by feature
  #
  # Returns:
  #   a list containing trained random forest model, prediction of training part
  #   and held-out part of reference dataset, prediction of query dataset, evaluation
  #   result using in training and test parts of the trainng dataset.
  #

    require(dplyr)
    require(sva)


  print (paste("load train/test partition from training set", refdat.path))
  train.test.mat <- readRDS(refdat.path)

  # due to high drop-out rate, only feature genes that are expressed in query dataset will be used
  common.genes <- intersect(colnames(train.test.mat$train), colnames(ext.dat))
  print(paste("Feature reduced from #", ncol(train.test.mat$train), "to", length(common.genes)))
  if(length(common.genes) == 0)
    stop("No genes match the training data set.")

  print ("Extract features")
  ext.feat.mat <- ext.dat[, common.genes]

  # For reference dataset, only immune cells are used.
  # In case that held-out cells contain non-immune cells,
  # non-immune held-out cells are filtered out.

  train.class <- train.test.mat$train %>% count(target) %>% select(target)  %>% unlist
  train.test.mat$test <- train.test.mat$test[train.test.mat$test$target %in% train.class, ]

  print ("Run ComBat to remove batch effect between ref and query datasets")
  # use ComBat to correct the batch effect between ref and query dataset

  uncorrected.matrix <- rbind(data.frame(train.test.mat$train[, common.genes], dataset="ref", stringsAsFactors = F),
                              data.frame(train.test.mat$test[, common.genes], dataset="ref", stringsAsFactors = F),
                              data.frame(ext.feat.mat[, common.genes], dataset = "query", stringsAsFactors = F)
                             )

  M <- t(uncorrected.matrix[, -ncol(uncorrected.matrix)])
  ds <- uncorrected.matrix$dataset
  rm(uncorrected.matrix)   # save memory

  # Count the genes with uniform expression across a single batch.
  # ComBat does not use them for batch correction.
  I.uniform <- c()
  for(batch in unique(ds)) {
    M.b <- M[,ds == batch]
    vars <- apply(M.b, 1, var)
    x <- which(vars < 1E-10)
    I.uniform <- c(I.uniform, x)
    rm(M.b)   # save memory
  }
  I.uniform <- sort(unique(I.uniform))
  num.genes <- nrow(M) - length(I.uniform)   # non-uniform genes

  #I.not.uniform <- setdiff(1:nrow(M), I.uniform)
  #print(rownames(M)[I.not.uniform])
  x1 <- "features have"
  x2 <- "are"
  if(num.genes == 1) {
    x1 <- "feature has"
    x2 <- "is"
  }
  cat(sprintf("%g %s non-uniform expression and %s usable for batch correction.\n", num.genes, x1, x2))
  if(num.genes < 2) {
    cat("Not enough common non-uniform genes for batch corrrection.\n")
    quit(save="no", status=-1)
  }

  corrected.matrix <- t(ComBat(M, ds))
  rm(M)  # save memory

  trainN <- nrow(train.test.mat$train)
  testN <- nrow(train.test.mat$test)
  queryN <- nrow(ext.feat.mat)

  train.corrected <- data.frame(corrected.matrix[1:trainN,],
                                target=train.test.mat$train$target, stringsAsFactors = F)

  test.corrected <- data.frame(corrected.matrix[(trainN+1):(trainN+testN),],
                                     target =train.test.mat$test$targert, stringsAsFactors = F)

  ext.corrected <- data.frame(corrected.matrix[(trainN+testN+1):nrow(corrected.matrix),])

  return(list(train=train.corrected, test=test.corrected, ext=ext.corrected))

}

#' Predict cell type and return probabilities across all cell types within one training dataset
#'
#' @param queryfile.path: string, path of the query dataset in tab-delimited text file with
#'   the rows as gene symbol, and columns as sampleID.
#' @param output.prefix: string, the prefix of query dataset
#' @param mode: string
#' @return a path to the file to be used as input to the deep learning step
#' @examples within_reference_pred(queryfile.path = "test/bulk.logrma.txt", output.prefix = "bulk", mode = "run")
#' @export
within_reference_pred <- function(queryfile.path, output.prefix = "query", num.cores = 1, mode = "run"){
  # This function call mlr_pred to predict for each reference dataset
  #
  #
  #
  # Args:
  #
  #   queryfile.path: string, path of the query dataset in tab-delimited text file with
  #   the rows as gene symbol, and columns as sampleID.
  #   output.prefix: string, the prefix of query dataset

  #
  # Returns:
  #   a list containing trained random forest model, prediction of training part
  #   and held-out part of reference dataset, prediction of query dataset, evaluation
  #   result using in training and test parts of the trainng dataset.
  #

    if(grepl("rds$", queryfile.path)){
        ext.dat <- readRDS(queryfile.path)
        print ("Please make sure to input Cell x Gene for .rds format")
    }else{

        print (paste("Open query file", queryfile.path))
        # read.table is slow and crashed by large input file
        #ext.dat <- read.table(queryfile.path, header = T, sep = "\t", row.names = 1, stringsAsFactors = F, check.names = F)
        #rownames(ext.dat) <- gsub("-|_", ".", toupper(rownames(ext.dat)))
        #ext.dat <- t(ext.dat)
        # use data.table

        ext.dat <- data.table::fread(queryfile.path, sep = "\t")
        if (colnames(ext.dat)[1]!='Gene'){
          cat('Please rename the 1st column as "Gene"\n')
          quit(save="no", status=-1)
        }
        if(any(duplicated(ext.dat$Gene))) {
          cat("Please remove duplicate Genes.\n")
          quit(save="no", status=-1)
        }

        print ("reshape input file")
      
      
        ext.dt2 <- data.table::transpose(ext.dat[, -1])
        ext.dt2 <- data.frame(ext.dt2)
        rownames(ext.dt2) <- colnames(ext.dat)[-1]
        colnames(ext.dt2) <- ext.dat[, Gene]
      
      
        ext.dat <- ext.dt2
        #x <- data.table::melt(ext.dt, id.var='Gene', variable.factor = F, value.factor = F, variable.name = 'Cell')
        #ext.dat <- data.table::dcast(x, Cell ~ Gene, median)
        #print ("convert to data.frame")
        #ext.dat <- data.frame(ext.dat, stringsAsFactors = F, check.names =F)
        #rownames(ext.dat) <- ext.dat$Cell
        #ext.dat<- ext.dat[, -1]
        colnames(ext.dat) <- gsub("-|_", ".", toupper(colnames(ext.dat)))
    }
    print ("Query file input is done")

    reference.paths <- c('Ref1: The Human Cell Atlas bone marrow single-cell interactive web portal' = 'hca-bm',
                         'Ref2: Circulating immune cell phenotype dynamics reflect the strength of tumor-immune cell interactions in patients during immunotherapy' = 'pbmc',
                         'Ref3: Single cell RNA sequencing of human liver reveals distinct intrahepatic macrophage populations' = 'liver-immune',
                         'Ref4: Human bone marrow assessment by single-cell RNA sequencing, mass cytometry, and flow cytometry' = 'jci-bm',
                         'Ref5: Single-Cell Transcriptomics of Human and Mouse Lung Cancers Reveals Conserved Myeloid Populations across Individuals and Species' = 'nsclc-zilionis-tii-minor',
                         'Ref6: Single-cell profiling of breast cancer T cells reveals a tissue-resident memory subset associated with improved prognosis' = 'brcatil',
                         'Ref7: Global characterization of T cells in non-small-cell lung cancer by single-cell sequencing' = 'nsclc-guo')


    lrn.name <- "classif.randomForest"
    res <- mlr_pred(lrn.name,
                    refdat.path = paste0("/ImmClassifier/data/", reference.paths[1], "-train-test-dat.rds"),
                    refdat.name = names(reference.paths)[1],
                    ext.dat, num.cores, mode)


    # concatenate prediction probabilities from all reference datasets
    for (i in 2:length(reference.paths)){

        res <- cbind(res,
                     mlr_pred(lrn.name,
                              refdat.path = paste0("/ImmClassifier/data/", reference.paths[i], "-train-test-dat.rds"),
                              refdat.name = names(reference.paths)[i], ext.dat, num.cores, mode))


    }

   fpath=paste0("/tmp/",output.prefix, ".dnn.input.txt")
   print(paste0("Write concanetated probabilities to ",fpath))
   write.table(data.frame("Cell" = rownames(res), res),
               fpath,
               quote = F, row.names = F, sep = "\t")
  return (fpath)
}
