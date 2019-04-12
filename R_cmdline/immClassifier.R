suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(mlr))
suppressPackageStartupMessages(require(sva))
suppressPackageStartupMessages(require(stringr))
suppressPackageStartupMessages(require(igraph))
suppressPackageStartupMessages(require(randomForest))
suppressPackageStartupMessages(require(optparse))
suppressPackageStartupMessages(require(synapser)) ##adding in the synapse requirement here; remove when it's an R package



############################################
### Extract params from the command line ###
getArgs<-function(){
    option_list <- list(
        make_option(c("-i", "--input"), default='../inst_immClassifierTestMatrix.tsv', help="Path to tab-delimited input matrix"),
        make_option(c("-p", "--prob"), default=0, help="Probability the cell types are unknown"),
        make_option(c("-o", "--output"), default="testout", help = "Prefix to add to output files"),
        make_option(c('-c','--cores'), default=1, help="Number of cores"),
        make_option(c('-m','--mlfile'),default=NULL, help="Path to ml file"),
        make_option(c('-t','--testmode'),default=FALSE,action='store_true',help='Run in test mode')
    )

    args=parse_args(OptionParser(option_list = option_list),args=commandArgs(trailingOnly=TRUE))

    return(args)
    }

main<-function(){
    args<-getArgs()
    input.path <- args$input
    prob.unknown <- args$prob
    out.prefix <- args$output
    num.cores <- args$cores
    ml.file <- args$mlfile

    mode=ifelse(args$testmode,'mode','prod')



    predict_immune_cell_types(input.path, prob.unknown, num.cores, ml.file, out.prefix,  mode)

}
###########################################

predict_immune_cell_types <- function(input.path='../inst/immClassifierTestMatrix.tsv',
                                      prob.unknown = 0,
                                      num.cores = 1,
                                      ml.file = NULL,
                                      out.prefix="tesdir",
                                      mode="prod"){
  ext.dat <- t(read.table(input.path,sep='\t')) ## updated this to take a text file and gene by sample matrix
  start_time <- Sys.time()
  # replace - as . in gene symbols to match genes in exp.dat to training dat
  colnames(ext.dat) <- gsub("-", ".",colnames(ext.dat))
  print('getting files from synapse')
  synLogin()
  hca.path=synapser::synGet('syn18496221')$path
  bm.path=synapser::synGet('syn18496223')$path
  liver.path=synapser::synGet('syn18496224')$path
  pbmc.path=synapser::synGet('syn18496225')$path


  print("begin MLR prediction")
  pred.res <- pred_dataset(ext.dat, num.cores = num.cores, mode,hca.path,bm.path,liver.path,pbmc.path)
  print("assign cell identity")
  cell.ident <- assign_cell_type(pred.res$res, prob.unknown)
  print (paste("writing mlr prediction results to",out.prefix))

  if(!dir.exists(out.dir))
      dir.create(out.dir)
  write.table (cell.ident$known, paste0(out.prefix,"_celltype_prediction_known.txt"), sep = '\t', quote = F)
  write.table (cell.ident$unknown, paste0(out.prefix,"_celltype_prediction_unknown.txt"), sep = '\t', quote = F)
  end_time <- Sys.time()

  print (paste0(round(end_time-start_time,2),  " mins"))
  #saveRDS(pred.res, paste0("output/",out.prefix,"_pred.data.rds"))
  #print ("writing cell identity cell.ident.rds to output/")
  #saveRDS(cell.ident, paste0("output/",out.prefix,"_cell.ident.rds"))
}

main()
