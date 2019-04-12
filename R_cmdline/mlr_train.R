#' @description Call train function in MLR package
#' @param lrn.name string classifier name default: classif.randomForest
#' @param train.dat dataframe cell x features in training samples
#' @param test.dat dataframe cell x features in held-out samples in training dataset
#' @param ext.dat dataframe cell x features in external dataset
#' @export
mlr_pred <- function(lrn.name, train.dat, test.dat, ext.dat){
    require(randomforest)
    require(mlr)
  task <- mlr::makeClassifTask(id = lrn.name, data = train.dat, target = "target")
  lrn <- makeLearner(lrn.name, predict.type = "prob")
  mod <- train(lrn, task)

  pred.train <- predict(mod, task = task)
  pred.test <- predict(mod, newdata = test.dat)
  pred.ext <- predict(mod, newdata = ext.dat)

  #measure.list <- list(mmce, bac, qsr, multiclass.aunp, multiclass.aunu,
  #                     ber, timepredict, multiclass.brier, ssr, acc, logloss,
  #                     wkappa, multiclass.au1p, multiclass.au1u, kappa)

  measure.list <- list(mlr::multiclass.aunp, mlr::kappa, mlr::timepredict, acc)


  # evaluation of performance
  perf.train <- performance(pred.train, measures = measure.list)
  perf.test <- performance(pred.test, measures = measure.list)

  print("Prediction evaluation using held-out samples in training datasets")
  print("-----------------------------------------------------------------")
  print (perf.test)

  # confusion matrix for held-out samples
  cm.train <- calculateConfusionMatrix(pred.train, relative = TRUE)
  cm.test <- calculateConfusionMatrix(pred.test, relative = TRUE)

  return (list(mod, pred.train, pred.test, pred.ext, perf.train, perf.test, cm.train, cm.test))
}



data_partition <- function(train.dat.path, ext.dat){

  print ("load train/test parition from training set")
  print (train.dat.path)
  train.test.mat <- readRDS(train.dat.path)
  common.genes <- intersect(colnames(train.test.mat$train), colnames(ext.dat))
  print(paste("Feature reduced from #", ncol(train.test.mat$train), "to", length(common.genes)))

  ext.feat.mat <- ext.dat[, common.genes]

  trainCellN <- nrow(train.test.mat$train)
  testCellN <- nrow(train.test.mat$test)
  extCellN <- nrow(ext.feat.mat)

  #combat
  dataset <- c(rep ("TRAIN", trainCellN + testCellN), rep("EXT", extCellN))

  comb.matrix <- rbind(train.test.mat$train[, common.genes],
                       train.test.mat$test[, common.genes],
                       ext.feat.mat)
  clean.matrix <- t(ComBat(t(comb.matrix), dataset))


  train.feat.combat <- data.frame(clean.matrix[1:trainCellN,],
                                      target=train.test.mat$train$target, stringsAsFactors = F)
  test.feat.combat <- data.frame(clean.matrix[(trainCellN+1):(trainCellN+testCellN),],
                                     target =train.test.mat$test$target, stringsAsFactors = F)
  ext.feat.combat <- data.frame(clean.matrix[(trainCellN+testCellN+1):nrow(clean.matrix),])

  return(list(train=train.feat.combat, test=test.feat.combat, ext=ext.feat.combat))

}


#' @description load pre-defined human bone marrow cell x features dataframe for training
#' @param ext.dat dataframe cell x features in external dataset
#' @param training.set path to rds of bone marrow training set
#' @return list BM prediction results

pred_hca_clusters <- function(ext.dat, num.cores = 1, mode,training.set="../data/hca-bm-train-test-dat.rds"){
    require(mlr)
    require(randomForest)
  # clusters from hca bone marrow cells
  data.folds <- data_partition(training.set, ext.dat)
  train.feat.combat <- data.folds[['train']]
  print("training using cells from bone marrow (HCA)")
  subsample.ids <- sample(1:nrow(train.feat.combat), 1000)
  if(mode == "test"){
    train.feat.combat <- train.feat.combat[subsample.ids, ]
  }
  pred.combat <- mlr_pred("classif.randomForest",
                          train.feat.combat,
                          data.folds[['test']],
                          data.folds[['ext']])

  dat.combat <- list(train = train.feat.combat,
                     test =  data.folds[['test']],
                     ext = data.folds[['ext']])

  return(list(pred = pred.combat, dat=dat.combat))
}



#' @description load pre-defined PBMC cell x features dataframe for training
#' @param ext.dat dataframe cell x features in external dataset
#' @param training.set path to rds of pbmc training set
#' @return list PBMC prediction results
pred_pd1_clusters <- function(ext.dat, num.cores = 1, mode,training.set="../data/pbmc-train-test-dat.rds"){
  # clusters from PD1 clinical trial

    require(randomForest)
    require(mlr)
  data.folds <- data_partition(training.set, ext.dat)
  train.feat.combat <- data.folds[['train']]
  print("training using cells from colon cancer PBMC")
  subsample.ids <- sample(1:nrow(train.feat.combat), 1000)
  if(mode == "test"){
    train.feat.combat <- train.feat.combat[subsample.ids, ]
  }
  pred.combat <- mlr_pred("classif.randomForest",
                          train.feat.combat,
                          data.folds[['test']],
                          data.folds[['ext']])

  dat.combat <- list(train = train.feat.combat,
                     test =  data.folds[['test']],
                     ext = data.folds[['ext']])

  return(list(pred = pred.combat, dat=dat.combat))
}

#' @description load pre-defined liver cell x features dataframe for training
#' @param ext.dat dataframe cell x features in external dataset
#' @param training.set path to rds of liver training set
#' @return list liver prediction results

pred_liver_clusters <- function(ext.dat, num.cores = 1, mode,training.set="../data/liver-immune-train-test-dat.rds"){
    require(randomForest)
    require(mlr)
  # clusters from nature communications paper, healthy liver donors
  print("training using cells from liver")
  data.folds <- data_partition(training.set, ext.dat)
  train.feat.combat <- data.folds[['train']]
  # remove inflammatory macs from train and test
  #train.feat.combat <- train.feat.combat[train.feat.combat$target != "4", ]
  test.feat.combat <- data.folds[['test']]
  #test.feat.combat <- test.feat.combat[test.feat.combat$target != "4", ]
  subsample.ids <- sample(1:nrow(train.feat.combat), 1000)
  if(mode == "test"){
    train.feat.combat <- train.feat.combat[subsample.ids, ]
  }
  pred.combat <- mlr_pred("classif.randomForest",
                          train.feat.combat,
                          test.feat.combat,
                          data.folds[['ext']])

  dat.combat <- list(train = train.feat.combat,
                     test = test.feat.combat,
                     ext = data.folds[['ext']])

  return(list(pred = pred.combat, dat=dat.combat))
}


#' @description load pre-defined bone marrow cell x features dataframe for training
#' @param ext.dat dataframe cell x features in external dataset
#' @param training.set path to rds of bm training set
#' @return list bm prediction results

pred_bm_clusters <- function(ext.dat, num.cores = 1, mode,training.set='../data/jci-bm-train-test-dat.rds'){
    require(randomForest)
    require(mlr)
  # clusters from JCI paper, healthy bone marrow doners
  print("training using cells from bone marrow (JCI)")
  data.folds <- data_partition(training.set, ext.dat)
  train.feat.combat <- data.folds[['train']]

  subsample.ids <- sample(1:nrow(train.feat.combat), 1000)
  if(mode == "test"){
    train.feat.combat <- train.feat.combat[subsample.ids, ]
  }
  pred.combat <- mlr_pred("classif.randomForest",
                          train.feat.combat,
                          data.folds[['test']],
                          data.folds[['ext']])

  dat.combat <- list(train = train.feat.combat,
                     test =  data.folds[['test']],
                     ext = data.folds[['ext']])

  return(list(pred = pred.combat, dat=dat.combat))
}

#' @description parse external dataset to BM and PBMC training datasets and gather results
#' @param ext.dat dataframe cell x features in external dataset
#' @param hca.path path to hca training dataset
#' @param bm.path path to JCI bone marrow training set
#' @param liver.path path to liver immune training set
#' @param pbmc.path path to pbmc training set
#' @return res dataframe cell x class_probability
#'
#' #@export
pred_dataset <- function(ext.dat, num.cores = 1, mode,
                         hca.path='../data/hca-bm-train-test-dat.rds',
                         bm.path='../data/jci-bm-train-test-dat.rds',
                         liver.path='../data/liver-immune-train-test-dat.rds',
                         pbmc.path='../data/pbmc-train-test-dat.rds'){

  hca.bm.rf <- pred_hca_clusters(ext.dat, num.cores = num.cores, mode,hca.path)
  jci.bm.rf <- pred_bm_clusters(ext.dat, num.cores = num.cores, mode,bm.path)
  liver.rf <- pred_liver_clusters(ext.dat, num.cores = num.cores, mode,liver.path)
  pbmc.rf <- pred_pd1_clusters(ext.dat, num.cores = num.cores, mode,pbmc.path)

  pbmc.res <- pbmc.rf$pred[[4]]$data
  hca.bm.res <- hca.bm.rf$pred[[4]]$data
  jci.bm.res <- jci.bm.rf$pred[[4]]$data
  liver.res <- liver.rf$pred[[4]]$data


  colnames(pbmc.res) <- gsub("prob", "PBMC", colnames(pbmc.res))
  colnames(pbmc.res)[ncol(pbmc.res)] <- 'PBMC.Response'

  colnames(hca.bm.res) <- gsub("prob", "HCABM", colnames(hca.bm.res))
  colnames(hca.bm.res)[ncol(hca.bm.res)] <- 'HCABM.Response'

  colnames(jci.bm.res) <- gsub("prob", "JCIBM", colnames(jci.bm.res))
  colnames(jci.bm.res)[ncol(jci.bm.res)] <- 'JCIBM.Response'

  colnames(liver.res) <- gsub("prob", "LIVER", colnames(liver.res))
  colnames(liver.res)[ncol(liver.res)] <- 'LIVER.Response'

  res <- cbind(pbmc.res, hca.bm.res, jci.bm.res, liver.res)
  dat <- list(pbmc.rf$dat, hca.bm.rf$dat, jci.bm.rf$dat, liver.rf$dat)
  return (list(res=res, dat=dat))
}
