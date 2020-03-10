find_parent <- function(node){
    parent <- "Immune_Cell"
    if(grepl(":", node)){
        parent <- gsub(":[^:]+$", "", node)
    }
    return (parent)
}



find_path <- function(node){

    path <- c(node)
    while (grepl(":", node)){
        parent <- find_parent(node)
        path <- c(path, parent)
        node <- parent
    }
    return (path)
}

construct_tree <- function(){

  tree <- list()
  tree[[1]] <- list(c('CD34', 'M', 'L'))
  names(tree[[1]]) <- 'Immune_Cell'

  tree[[2]] <- list(c('CD34:HSC'),
                   c('L:B', 'L:T', 'L:NK', 'L:pDC', 'L:unconvT'),
                   c('M:pDC',  'M:cDC', 'M:Eos', 'M:Ery', 'M:Mac', 'M:Mast', 'M:Mega', 'M:Mono', 'M:Neu'))

  names(tree[[2]]) <- c('CD34', 'L', 'M')

  tree[[3]] <- list(c('L:B:PC'),
                    c('L:T:CD4', 'L:T:CD8'),
                    c('M:Mega:Platelet')
                   )
  names(tree[[3]]) <- c('L:B', 'L:T', 'M:Mega')



  tree[[4]] <- list(c('L:T:CD4:CM', 'L:T:CD4:EM', 'L:T:CD4:Ex', 'L:T:CD4:Naïve', 'L:T:CD4:Tfh', 'L:T:CD4:Treg', 'L:T:CD4:TRM'),
                    c('L:T:CD8:CM', 'L:T:CD8:EM', 'L:T:CD8:Ex', 'L:T:CD8:Mait', 'L:T:CD8:Naïve', 'L:T:CD8:TRM')
                   )
  names(tree[[4]]) <- c('L:T:CD4','L:T:CD8')

  return(tree)
}


get_entropy <- function(p){
    if(p <= 0 | p == 1){return (0)}
    return(-p*log(p)-(1-p)*log(1-p))
}


tie_breaker <- function(candidates){


    if(length(candidates) == 1) {return (candidates[1])}

    # find common ancestors
    paths <- list()
    for(i in 1:length(candidates)){
        paths[[i]] <- c(find_path(candidates[i]), 'Immune_Cell')
    }


    i <- 1

    while(i <= length(paths[[1]]) ){
        cas <- TRUE

        for (j in 2:length(paths)){

            if(! (paths[[1]][i] %in% paths[[j]])){
                cas <- FALSE
                break
            }
        }

        if(cas) return (paths[[1]][i])
        i <- i+1
    }

}

assign_cell_label <- function(vstats){


    tree <- construct_tree()
    probs <- vstats[1:35]
    names(probs) <- gsub(".prob", "", names(probs))
    ent <- sapply(probs, function(x) get_entropy(x))

    entgain <- ent

    i <- length(tree)
    while(i >1){
        for (s in names(tree[[i]])){
            if(length(tree[[i]][[s]])>1){
                for (t in tree[[i]][[s]]){
                      entgain[s] <- entgain[s] - ent[t]#/length(tree[[i]][[s]])
                  }
            }


        }
        i <- i-1
    }

   # deal with ties, choose the lowest std/mean
    candidates <- names(entgain[entgain==max(entgain)])

    return (candidates[order(vstats[paste0(candidates, ".std")]/vstats[paste0(candidates, ".prob")])[1]])
}


#' Assign final cell type label for the whole query dataset
#'
#' @param output.prefix: string, the prefix of query dataset
#'
#' @return a dataframe with column1 as cell id and column2 as the predicted cell type
#' @examples assign_dataset('bulk')
#'
#' @export
assign_dataset <- function(output.prefix,deep.learning.file){
  # This function assign the final cell label for each cell in the query dataset:
  #
  # Args:
  #   output.prefix: string, the prefix of query dataset


  #
  # Returns:
  # 1. return a dataframe with column1 as cell id and column2 as the predicted cell type
  # 2. write the returned dataframe to disk
    require(dplyr)
    ref.nodes <- c("CD34","CD34:HSC","L","L:NK","L:B",
                   "M","M:Mega","M:Mega:Platelet","M:Ery","M:Eos",
                   "M:Neu","M:Mono", "L:pDC","M:pDC","L:B:PC",
                   "M:cDC","L:T","L:T:CD4","L:T:CD4:EM","L:T:CD8",
                   "L:T:CD8:Ex","L:T:CD8:EM","L:T:CD8:CM","L:T:CD4:Tfh","L:T:CD4:Naïve",
                   "L:T:CD4:CM","L:T:CD4:Treg","L:T:CD8:Naïve", "M:Mac","L:unconvT",
                   "M:Mast","L:T:CD4:TRM", "L:T:CD8:TRM","L:T:CD4:Ex","L:T:CD8:Mait")

    norm.method <- "ontotree"
    fpath <- deep.learning.file #paste0('tensorflow/output/', output.prefix,  '.deeplearning.', norm.method, '.stats.txt', sep = "")
    print (paste0('Read ', fpath))
    dnn.stats <- read.table(fpath, header = F, sep = " ",stringsAsFactors=F)

    cnames <- c("Cell",paste0(ref.nodes, ".prob"), paste0(ref.nodes, ".std"))
    colnames(dnn.stats) <- cnames
    rownames(dnn.stats) <- dnn.stats$Cell
    print (paste0('Assign the final labels'))
    assign.labels <- dnn.stats[, -1] %>%
           apply(1, function(x) assign_cell_label(x))
    print (paste0('Finish assigning the final labels'))
    outfile= paste0("/tmp/", output.prefix, ".output.txt")
    write.table(data.frame(Cell=dnn.stats$Cell,
           ImmClassifier_prediction= assign.labels),
               outfile,
           quote = F, sep = "\t", row.names = F)
#    return (data.frame(Cell=dnn.stats$Cell, ImmClassifier_prediction=assign.labels, stringsAsFactors = F))
    return(outfile)


}
