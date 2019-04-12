get_entropy <- function(probs){

  sum <- 0
  for(p in probs){
    if(p>0){
      sum <- sum  -p*log(p)
    }
  }
  return (sum)
}


#' @description Define the immune tree which will used for inferring cell identity
define_tree_structure <- function(){

  tree <- list()
  tree[[1]] <- list(c('Immune_Cell', 'HCABM.33'))
  names(tree[[1]]) <- 'Root'

  tree[[2]] <- list(c('HSPC', 'Myeloid', 'Lymphoid', 'PBMC.17'))
  names(tree[[2]]) <- 'Immune_Cell'

  tree[[3]] <- list(
    c('HCABM.10', 'JCIBM.12', 'HCABM.11', 'HCABM.12'), # HSC, HSPC, HSC-cycle, Multi-lin
    c('Megakaryocyte-erythroid', 'Neutrophil', 'Monocyte', 'DC', 'Other_Gran'),
    c('Lymphoid_Progenitor', 'T', 'B', 'NK_Cell'))
  names(tree[[3]]) <- c('HSPC',
                        'Myeloid',
                        'Lymphoid')

  tree[[4]] <- list(c('HCABM.15', 'Megakaryocyte', 'Erythrocyte'),
                    c('HCABM.31', 'HCABM.30'),
                    c('HCABM.32', 'PBMC.2', 'PBMC.3', 'PBMC.9', 'PBMC.10', 'PBMC.15', 'PBMC.18', 'PBMC.4', 'JCIBM.2', 'JCIBM.3', 'JCIBM.16'),
                    c('HCABM.27', 'PBMC.14', 'JCIBM.9', 'HCABM.34', 'PBMC.19', 'JCIBM.19'),
                    c('HCABM.13', 'HCABM.18', 'HCABM.28', 'HCABM.29', 'HCABM.7', 'HCABM.8'),
                    c('HCABM.9', 'HCABM.3', 'HCABM.2', 'HCABM.5', 'HCABM.6'),
                    c('T:CD4+', 'T:CD8+'),
                    c('HCABM.24', 'HCABM.1', 'HCABM.4', 'PBMC.11', 'JCIBM.1', 'PBMC.13', 'HCABM.22', 'LIVER.16', 'JCIBM.4',
                      'HCABM.35', 'PBMC.20', 'PBMC.25', 'LIVER.7', 'JCIBM.18'),
                    c('PBMC.1', 'PBMC.12', 'HCABM.19', 'LIVER.8', 'JCIBM.17')
                    )
  names(tree[[4]]) <- c('Megakaryocyte-erythroid',
                        'Neutrophil',
                        'Monocyte',
                        'DC',
                        'Other_Gran',
                        'Lymphoid_Progenitor',
                        'T',
                        'B',
                        'NK_Cell')


  tree[[5]] <- list(c('HCABM.14', 'PBMC.22', 'HCABM.23', 'JCIBM.15'),
                    c('HCABM.17', 'JCIBM.14', 'HCABM.16', 'JCIBM.11', 'HCABM.26', 'JCIBM.10', 'HCABM.25', 'LIVER.19', 'JCIBM.13', 'PBMC.24'),
                    c('PBMC.T1', 'JCIBM.6', 'PBMC.T4', 'PBMC.T0', 'JCIBM.5', 'PBMC.T8', 'PBMC.T15'),
                    c('PBMC.T9', 'JCIBM.8', 'PBMC.T7', 'PBMC.T14', 'PBMC.T11', 'PBMC.T5', 'PBMC.T12','PBMC.T13', 'JCIBM.7', 'PBMC.T10', 'PBMC.T3', 'PBMC.T2')
                    )
  names(tree[[5]]) <- c('Megakaryocyte',
                        'Erythrocyte',
                        'T:CD4+',
                        'T:CD8+'
                        )



  tree
}


#' @description initialize and update node probability of immune tree from bottom to up
#' @param probs vector probability for leaf nodes in immune tree
infer_identity <- function(probs){

  tree <- define_tree_structure()
  height <- length(tree)

  vertex.names <- c('Root')
  for (i in 1:height){
    vertex.names <- c(vertex.names, unlist(tree[[i]]))
  }

  vertex.probs <- rep(0, length(vertex.names))
  names(vertex.probs) <- vertex.names

  split.classes <- c('LIVER.2', 'LIVER.9', 'LIVER.18', 'HCABM.20', 'PBMC.23', 'HCABM.21', 'LIVER.4', 'LIVER.10', 'LIVER.7')
  valid.leaves <- names(probs)[!names(probs) %in% split.classes]
  # init leaf prob
  vertex.probs[valid.leaves] <- probs[valid.leaves]

  # pass the CD8+ population prob from HCABM.20 and PBMC.23 to the rest CD8+ subpopulations

  for (t in tree[[5]][['T:CD8+']]){

    vertex.probs[t] = probs[t] + (probs['HCABM.20'] + probs['PBMC.23'])/length(tree[[5]][['T:CD8+']])

  }

  # pass the Naive T prob to CD4+ and CD8+ Naive population
  vertex.probs['PBMC.T1'] <- vertex.probs['PBMC.T1'] + probs['HCABM.21']/2
  vertex.probs['PBMC.T9'] <- vertex.probs['PBMC.T9'] + probs['HCABM.21']/2

  for (t in c(tree[[5]][['T:CD8+']], tree[[5]][['T:CD4+']])){

    vertex.probs[t] = probs[t] + (probs['LIVER.2'] + probs['LIVER.9'] + probs['LIVER.18'])/length(c(tree[[5]][['T:CD8+']], tree[[5]][['T:CD4+']]))

  }
  for (t in c(tree[[4]][['Monocyte']], tree[[4]][['DC']])){

    vertex.probs[t] = probs[t] + (probs['LIVER.4'] + probs['LIVER.10'])/length(c(tree[[4]][['Monocyte']], tree[[4]][['DC']]))

  }

  entropy <- rep(0, length(vertex.names))
  names(entropy) <- vertex.names

  entropy.gain <- rep(0, length(vertex.names))
  names(entropy.gain) <- vertex.names

  # initialize leaf entropies and entropy gains

  for (leaf in valid.leaves){

    entropy[leaf] <- get_entropy(probs[leaf])
    entropy.gain[leaf] <- entropy[leaf]
  }


  i <- height
  while(i >0){
    for (s in names(tree[[i]])){
      for (t in tree[[i]][[s]]){
        vertex.probs[s] <- vertex.probs[s] + vertex.probs[t]
      }

      entropy[s] <- get_entropy(vertex.probs[s])
      entropy.gain[s] <- entropy[s]
      for (t in tree[[i]][[s]]){
        if(vertex.probs[s] > 0){
          entropy.gain[s] <- entropy.gain[s] - entropy[t]
        }
      }

    }
    i <- i-1
  }


  gain.max <- entropy.gain[order(-entropy.gain)][1]
  enforce.probs <- vertex.probs[valid.leaves]
  enforce.max <- enforce.probs[enforce.probs == max(enforce.probs)]

  return (list(names(gain.max)[sample(1:length(gain.max), 1)],
               names(enforce.max)[sample(1:length(enforce.max), 1)]))
}


#' @description Infer cell identity using auto, enforce and general mode

assign_cell_type <- function(pred.res, p=0){
  integrated.cluster.names <- readRDS('../inst/integrated.cluster.names.rds') #update this when we have proper package
  # normalize prediction probability
  pred.res[, !grepl('Response', colnames(pred.res))] <- pred.res[, !grepl('Response', colnames(pred.res))]/4
  maxP <- pred.res[, !grepl('Response', colnames(pred.res))] %>% apply(1, max)

  pred.unknown <- pred.res[maxP < p, ]
  pred.known <- pred.res[maxP >= p, ]

  infer.res <- matrix('NA', nrow(pred.known), 2)

  for (i in 1:nrow(pred.known)){
    ident <- infer_identity(pred.known[i, !grepl('Response', colnames(pred.known))] %>% unlist)

    if(grepl("\\.", ident[[1]])) ident[[1]] <- integrated.cluster.names[ident[[1]],'ClusterIDName']
    if(grepl("\\.", ident[[2]])) ident[[2]] <- integrated.cluster.names[ident[[2]],'ClusterIDName']
    infer.res[i, ] <- c(ident[[1]], ident[[2]])
  }



  tree <- define_tree_structure()
  child.parent.map <- data.frame(child=character(), parent=character(),stringsAsFactors=FALSE)

  i <- length(tree)
  while(i>0){
      for(s in names(tree[[i]])){
        for (t in tree[[i]][[s]]){
          if(grepl("\\.", t)){
            child.parent.map <- rbind(child.parent.map, data.frame(child=t, parent=s, stringsAsFactors = F))
          }
        }
      }
    i <- i-1
  }

  rownames(child.parent.map) <- child.parent.map$child

  pred.known <- data.frame(pred.known,
                           enforce = infer.res[,2] %>% unlist,
                           auto = infer.res[,1] %>% unlist,
                           general = ifelse(grepl("\\.", infer.res[,1]) , child.parent.map[gsub(":[^:]+$", "",infer.res[,1]), 2], infer.res[,1])
  )
  return (list(known=pred.known, unknown=pred.unknown))
}
