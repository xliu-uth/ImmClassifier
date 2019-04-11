require(dplyr)
require(mlr)
require(sva)
require(stringr)
require(igraph)
require(randomForest)


#' @author AXL
#' @title Predict immune cell identity in new dataset.
#' @description immClassifier predicts cell/sample identity using single-cell or bulk RNA-Seq sequencing.
#' @param exp.dat A numeric dataframe with logCPM values, rownames as unique cellIDs and colnames as unique gene symbols.
#' @param prob.unknown A number in [0,1]. If the max prob among all predicted cell types is less than prob.unknown, a cell is assigned unknown.
#' @param num.cores A number >=1. The number of cores can be utilized
#' @param ml.file A string as the markup file location. NULL as default.
#' @param mode A string "test": for testing docker which takes minimal time to go through the whole workflow; "real": otherwise
#' @return csv files under output/
#' @export
#' @examples
#' predict_immune_cell_types()
predict_immune_cell_types <- function(input.path='test/bulk.logrma.rds', 
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
  
  write.table (cell.ident$known, paste0("output/",out.prefix,"_celltype_prediction_known.txt"), sep = '\t', quote = F)
  write.table (cell.ident$unknown, paste0("output/",out.prefix,"_celltype_prediction_unknown.txt"), sep = '\t', quote = F)
  end_time <- Sys.time()
  
  print (paste0(round(end_time-start_time,2),  " mins"))
  #saveRDS(pred.res, paste0("output/",out.prefix,"_pred.data.rds"))
  #print ("writing cell identity cell.ident.rds to output/")
  #saveRDS(cell.ident, paste0("output/",out.prefix,"_cell.ident.rds"))
}




