source("./assign_cell_identity.R")


#' @author AXL
#' @title Visualize defined immune tree structure
#' @description plot immune tree
#' @export
#' @examples
#' plot_immune_tree()
plot_immune_tree <- function(){

  integrated.cluster.names <- readRDS('../inst/integrated.cluster.names.rds') #update this when we have a proper package
  tree <- define_tree_structure()
  tree.edges <- data.frame(source=character(), target=character(),stringsAsFactors=FALSE)
  i <- length(tree)
  while(i>0){
    for(s in names(tree[[i]])){
      for (t in tree[[i]][[s]]){

        tree.edges <- rbind(tree.edges, data.frame(source=ifelse(grepl("\\.", s ),
                                                                 integrated.cluster.names[s, 'ClusterIDName'], s),
                                                   target=ifelse(grepl("\\.", t ),
                                                                 integrated.cluster.names[t, 'ClusterIDName'], t),
                                                   stringsAsFactors=FALSE))
      }
    }
    i <- i-1
  }

  g <- graph_from_data_frame(tree.edges, directed = T)
  layout= layout_as_tree(g)
  g %>% plot (layout = layout[, 2:1], vertex.shape = "circle",
          vertex.size = 1, vertex.label.cex = 0.5,vertex.label.family = "Helvetica",
          vertex.label.color = "black",edge.arrow.size=0,edge.color="purple",edge.width = .4)
}
