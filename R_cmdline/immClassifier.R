require(dplyr)
require(mlr)
require(sva)
require(stringr)
require(igraph)
require(randomForest)


source("./mlr_train.R")
source("./assign_cell_identity.R")
source("./visualization.R")
############################################ 
### Extract params from the command line ###
argv <- commandArgs()
if(length(argv) != 13){

  #print("Usage: R CMD BATCH --args <input.path> <prob.unknown> <num.cores> <ml.file> <out.prefix> <mode> immClassifier.R")
  print("Example: R CMD BATCH --args ../test/bulk.logrma.rds 0 1 NULL testrun test immClassifier.R" )
print(argv)  
q()
}


input.path <- argv[8]
prob.unknown <- argv[9]
num.cores <- argv[10]
ml.file <- argv[11]
out.prefix <- argv[12]
mode <- argv[13]

predict_immune_cell_types <- function(input.path='../test/bulk.logrma.rds', 
                                      prob.unknown = 0, 
                                      num.cores = 1, 
                                      ml.file = NULL, 
                                      out.prefix="testrun",  
                                      mode="test"){
  ext.dat <- readRDS(input.path)
  start_time <- Sys.time()
  # replace - as . in gene symbols to match genes in exp.dat to training dat
  colnames(ext.dat) <- gsub("-", ".",colnames(ext.dat))
  print("begin MLR prediction")
  pred.res <- pred_dataset(ext.dat, num.cores = 1, mode)
  print("assign cell identity")
  cell.ident <- assign_cell_type(pred.res$res, prob.unknown)
  print ("writing mlr prediction results to output/")
  
  write.table (cell.ident$known, paste0("../output/",out.prefix,"_celltype_prediction_known.txt"), sep = '\t', quote = F)
  write.table (cell.ident$unknown, paste0("../output/",out.prefix,"_celltype_prediction_unknown.txt"), sep = '\t', quote = F)
  end_time <- Sys.time()
  
  print (paste0(round(end_time-start_time,2),  " mins"))
  #saveRDS(pred.res, paste0("output/",out.prefix,"_pred.data.rds"))
  #print ("writing cell identity cell.ident.rds to output/")
  #saveRDS(cell.ident, paste0("output/",out.prefix,"_cell.ident.rds"))
}



##########################################
# Run the analysis 

predict_immune_cell_types(input.path, prob.unknown, num.cores, ml.file, out.prefix,  mode)





