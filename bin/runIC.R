library(reticulate)
library(optparse)

#!/usr/bin/env Rscript

############################################
### Extract params from the command line ###
getArgs<-function(){
    require(optparse)
    option_list <- list(
        make_option(c("-i", "--input"), dest='input',default='inst/immClassifierTestMatrix.tsv', help="Path to tab-delimited input matrix"),
        make_option(c("-p", "--prob"), default=0,dest='prob', help="Probability the cell types are unknown"),
        make_option(c("-o", "--output"), default="testout", dest='output',help = "Prefix to add to output files"),
#cores'), default=1, dest='cores',help="Number of cores"),
   #     make_option(c('-m','--mlfile'),default=NULL, dest='mlfile',help="Path to ml file"),
        make_option(c('-t','--testmode'),default=FALSE,dest='testmode',action='store_true',help='Run in test mode')
    )

    args=parse_args(OptionParser(option_list = option_list))

    return(args)
    }

main<-function(){
    args<-getArgs()
    input.path <- args$input
    prob.unknown <- args$prob
    out.prefix <- args$output
#    num.cores <- args$cores

    # Allow the user to specify --input files relative to the current
    # working directory.
    orig.input.path <- input.path
    if(!file.exists(input.path))
      input.path <- sprintf("/tmp/%s", input.path)
    if(!file.exists(input.path))
      stop(sprintf("Cannot find file: %s", orig.input.path))


    mode=ifelse(args$testmode,'mode','prod')

    require(ImmClassifier)
    res.path=within_reference_pred(input.path, out.prefix, mode=mode)
    model.dir='/ImmClassifier/tensorflow/pre-trained-models' ##not writeable, absolute path won't work outside docker
    pred.file<-predictHierarchy(res.path, out.prefix,model.dir)
    cell.types<-assignCellTypes(out.prefix, pred.file)
}



#step2: predict hierarchy
predictHierarchy<-function(res.path,out.prefix,model.dir){
    library(reticulate)
    dnn<-source_python("/ImmClassifier/bin/dnn.py") ## i hate the absolute path but it will work in Docker
    predFile<-dnn_predict(res.path,out.prefix,paste0(model.dir,'/'))
    return(predFile)

}

#step3: assign cell type
assignCellTypes<-function(out.prefix,pred.file){
    fname=assign_dataset(out.prefix,pred.file)
    print(fname)
}


main()
