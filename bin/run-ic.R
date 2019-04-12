############################################
### Extract params from the command line ###
getArgs<-function(){
    require(optparse)
    option_list <- list(
        make_option(c("-i", "--input"), dest='input',default='../inst_immClassifierTestMatrix.tsv', help="Path to tab-delimited input matrix"),
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

    predict_immune_cell_types(input.path, prob.unknown, num.cores, ml.file, out.prefix,  mode)

}

main()
