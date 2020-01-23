
plot_heatmap <- function(data, title="",  ncol = 2, ord1 = NULL, ord2=NULL,
                         size = 10,size2=2, palette = 'Purples',
                         minpct = 0, minshow=25, measures = c('f1', 'Precision', 'Recall'),
                         legend.pos='none') {

  TP <- data
  FN <- apply(data, 1, sum) - data
  FP <- t(apply(data, 2, sum) - t(data))
  TN <- sum(data) - TP -FN - FP

  accuracy <- (TP+TN)/(FN+FP+TP+TN)
  recall <- TP/(TP+FN)
  specificity <- TN/(TN+FP)
  precision <- TP/(TP+FP) # PPV

  f1 <- 2*(precision*recall)/(precision+recall)


  data <- rbind(data.frame(recall %>% melt, measure = 'Recall'),
                data.frame(precision %>% melt, measure = 'Precision'),
                data.frame(f1  %>% melt, measure = 'f1')
  ) %>% mutate(value = round(value,2)*100)


  if(!is.null(ord1)){
    data <- data %>%
      mutate(annot = as.character(annot)) %>%
      mutate(annot = ifelse(annot %in% ord1, annot, 'Other')) %>%
      mutate(annot = factor(annot, levels = c(ord1, 'Other')))

  }

  if(!is.null(ord2)){
    data <- data %>%
      mutate(Known = as.character(Known)) %>%
      mutate(Known = ifelse(Known %in% ord2, Known, 'Other')) %>%
      mutate(Known =  factor(Known, levels = c(ord2, 'Other')))
  }



  #colfunc <- colorRampPalette(c("white", "purple"))
  colfunc <- colorRampPalette(brewer.pal(9, palette))
  #zcut <- c(-1, 25, 50, 75, 100)
  zcut <- c(-1, 19, 40, 60, 80, 100)
  #zcut <- c(-1, seq(10, 100, 10))

  data2 <- data %>% filter(measure %in% measures)
  data2[is.na(data2$value), 'value'] <- 0

  data2 %>%
    ggplot(aes(Known, annot,  fill = cut(value, zcut))) +
    geom_tile(color = "black") +
    #  scale_fill_brewer(palette = palette,  direction = 1) +
    theme_minimal()+
    theme(
      axis.text.x = element_text(angle = 45, size = size, hjust = 1,color= "black"),
      axis.text.y = element_text(size = size, hjust = 1,color = "black"),
      axis.title.x = element_text(size = size+4, hjust = 1),
      axis.title.y = element_text(size = size+4, hjust = 1),
      legend.text = element_text(size = size, angle = 0, hjust = 1),
      legend.title = element_text(size = size, hjust = 1),
      plot.title = element_text(size = 14, hjust = 1),
      strip.text.x = element_text(size = 14, colour = "black", angle = 0),
      legend.position=legend.pos) +
    coord_equal()+
    labs(title = title, x ="known", y = "predicted") +

    facet_wrap(~measure, ncol =  ncol)   +
    scale_fill_manual(values = c('white', colfunc(9)[c(3,5,7,9)])) +
    scale_colour_manual(values=c(rep("black", 3), rep("white",2))) +
    # scale_fill_manual(values = colfunc(20)) +
    geom_text(aes(label=ifelse(value>minshow, value, ""), color = cut(value, zcut)), size = size2) +
    guides(fill=guide_legend(title="percentage of row (recall), col(precision)"))

}





# match clusters between original and unsupervised umap

getNearestCentroid <- function(query, original.centroids){
  return(original.centroids[order(apply(original.centroids[, -1], 1, function(x) (x[1]-query[1])^2 + (x[2]-query[2])^2))[1], original_annotation])
}
