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
        make_option(c('-c','--cores'), default=1, dest='cores',help="Number of cores"),
        make_option(c('-m','--mlfile'),default=NULL, dest='mlfile',help="Path to ml file"),
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
    num.cores <- args$cores
    ml.file <- args$mlfile

    mode=ifelse(args$testmode,'mode','prod')

    require(ImmClassifier)

    #altered data to write name of file
                                        # res.path=within_reference_pred(input.path, out.prefix,  mode)
    res.path='tensorflow/input/bulk.dnn.input.txt'
    model.dir='tensorflow/intput/pre-trained-models'
    out.prefix='tensorflow/output/bulk.'
    pred.file<-predictHierarchy(res.path,out.prefix,model.dir)

}



#step2: predict hierarchy
predictHierarchy<-function(res.path,out.prefix,model.dir){
    library(reticulate)
    dnn<-source_python("bin/dnn.py") ## i hate the absolute path but it will work in Docker
    predFile<-dnn_predict(res.path,out.prefix,model.dir)
    return(predFile)

}

#step3: assign cell type
assignCellTypes<-function(pred.file){
    fnam=assign_dataset(out.prefix,pred.file)
    print(fname)
}


main()
