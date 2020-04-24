#####################################################################
#                             brca3p                                #
#####################################################################

brca3p_dt[1:3,]

brca3p_L1_celltypes <- c('L', 'M')

brca3p_L1 <- brca3p_dt[!grepl("NKT", original_annotation), ]
brca3p_L1[, original_annotation:=ifelse(grepl("^M|^m|NEUTRO", original_annotation), 'M', original_annotation)]
brca3p_L1[, original_annotation:=ifelse(grepl("^T|NK|B|pDC", original_annotation), 'L', original_annotation)]


brca3p_L1[, ImmC:=ifelse(grepl("^L:", ImmC), 'L', ImmC)]
brca3p_L1[, ImmC:=ifelse(grepl("^M:", ImmC), 'M', ImmC)]
brca3p_L1[, ImmC:=ifelse(grepl("CD34", ImmC), 'CD34', ImmC)]

brca3p_L1[, SingleR:=ifelse(grepl("B_cell|T_cell|NK", SingleR), 'L', SingleR)]
brca3p_L1[, SingleR:=ifelse(grepl("DC:m|Mono|Neutro|Macro", SingleR), 'M', SingleR)]
brca3p_L1[, SingleR:=ifelse(SingleR %in% c('L', 'M'), SingleR, 'Other')]

brca3p_L1[, garnett:=ifelse(grepl("B cells|T cells|NK", garnett), 'L', garnett)]
brca3p_L1[, garnett:=ifelse(grepl("Mono|Den", garnett), 'M', garnett)]
brca3p_L1[, garnett:=ifelse(garnett %in% c('L', 'M'), garnett, 'Other')]

brca3p_L1[, ctrl:=ifelse(grepl("^L:", ctrl), 'L', ctrl)]
brca3p_L1[, ctrl:=ifelse(grepl("^M:", ctrl), 'M', ctrl)]
brca3p_L1[, ctrl:=ifelse(grepl("CD34", ctrl), 'CD34', ctrl)]

table(data.table(Known=brca3p_L1$original_annotation, annot=brca3p_L1$ImmC)) %>%
plot_heatmap(title = "ImmC", palette= "RdPu",
             minshow = 19, size =10, size2= 3, minpct=0, legend.pos = "none",
             ord1 = c('L', 'M'),
             ord2 = c( 'L', 'M'),
             measures = c('Recall', 'Precision')
)

brca3p.ImmC.L1.recall <- c(97, 94)
brca3p.ImmC.L1.ppv <- c(99, 94)


table(data.table(Known=brca3p_L1$original_annotation, annot=brca3p_L1$SingleR)) %>%
  plot_heatmap(title = "SingleR", palette= "RdPu",
               minshow = 0, size =10, size2= 3, minpct=0, legend.pos = "none",
               ord1 = c('L', 'M'),
               ord2 = c( 'L', 'M'),
               measures = c('Recall', 'Precision')
  )



brca3p.SingleR.L1.recall <- c(95, 90)
brca3p.SingleR.L1.ppv <- c(99, 97)


table(data.table(Known=brca3p_L1$original_annotation, annot=brca3p_L1$garnett)) %>%
  plot_heatmap(title = "Garnett", palette= "RdPu",
               minshow = 0, size =10, size2= 3, minpct=0, legend.pos = "none",
               ord1 = c('L', 'M'),
               ord2 = c( 'L', 'M'),
               measures = c('Recall', 'Precision')
  )


brca3p.Garnett.L1.recall <- c(87, 54)
brca3p.Garnett.L1.ppv <- c(97, 92)


table(data.table(Known=brca3p_L1$original_annotation, annot=brca3p_L1$ctrl)) %>%
  plot_heatmap(title = "Ctrl", palette= "RdPu",
               minshow = 0, size =10, size2= 3, minpct=0, legend.pos = "none",
               ord1 = c('L', 'M'),
               ord2 = c( 'L', 'M'),
               measures = c('Recall', 'Precision')
  )


brca3p.ctrl.L1.recall <- c(99, 87)
brca3p.ctrl.L1.ppv <- c(97, 97)


########################## End of brca3p L1 #########################

brca3p_dt[1:3,]

brca3p_L2_celltypes <- c('B', 'T', 'NK', 'Dendritic', 'Monocyte', 'Macrophage', 'Neutrophil', 'Mast')




brca3p_L2 <- brca3p_dt[!grepl("NKT", original_annotation), ]

brca3p_L2[, original_annotation:=ifelse(grepl("B:", original_annotation), 'B', original_annotation)]
brca3p_L2[, original_annotation:=ifelse(grepl("MACROPHAGE", original_annotation), 'Macrophage', original_annotation)]
brca3p_L2[, original_annotation:=ifelse(grepl("MAST", original_annotation), 'Mast', original_annotation)]
brca3p_L2[, original_annotation:=ifelse(grepl("DC", original_annotation), 'Dendritic', original_annotation)]
brca3p_L2[, original_annotation:=ifelse(grepl("MONO", original_annotation), 'Monocyte', original_annotation)]
brca3p_L2[, original_annotation:=ifelse(grepl("NEUTR", original_annotation), 'Neutrophil', original_annotation)]
brca3p_L2[, original_annotation:=ifelse(grepl("NK", original_annotation), 'NK', original_annotation)]
brca3p_L2[, original_annotation:=ifelse(grepl("T:", original_annotation), 'T', original_annotation)]


brca3p_L2[, ImmC:=ifelse(grepl("L:T|convT", ImmC), 'T', ImmC)]
brca3p_L2[, ImmC:=ifelse(grepl("L:NK", ImmC), 'NK', ImmC)]
brca3p_L2[, ImmC:=ifelse(grepl("L:B", ImmC), 'B', ImmC)]
brca3p_L2[, ImmC:=ifelse(grepl("M:Mono", ImmC), 'Monocyte', ImmC)]
brca3p_L2[, ImmC:=ifelse(grepl("M:Mac", ImmC), 'Macrophage', ImmC)]
brca3p_L2[, ImmC:=ifelse(grepl("M:Neu", ImmC), 'Neutrophil', ImmC)]
brca3p_L2[, ImmC:=ifelse(grepl("M:Mast", ImmC), 'Mast', ImmC)]
brca3p_L2[, ImmC:=ifelse(grepl("DC", ImmC), 'Dendritic', ImmC)]
brca3p_L2[, ImmC:=ifelse(ImmC %in% c('B','T','NK', 'Macrophage', 'Mast', 'Dendritic', 'Neutrophil', 'Monocyte'), ImmC, 'Other')]


brca3p_L2[, SingleR:=ifelse(grepl("B_cell",SingleR),"B", SingleR)]
brca3p_L2[, SingleR:=ifelse(grepl("T_cell",SingleR),"T", SingleR)]
brca3p_L2[, SingleR:=ifelse(grepl("NK",SingleR),"NK", SingleR)]
brca3p_L2[, SingleR:=ifelse(grepl("DC",SingleR),"Dendritic", SingleR)]
brca3p_L2[, SingleR:=ifelse(grepl("Monocyte",SingleR),"Monocyte", SingleR)]
brca3p_L2[, SingleR:=ifelse(grepl("Macrophage",SingleR),"Macrophage", SingleR)]
brca3p_L2[, SingleR:=ifelse(grepl("Mast",SingleR),"ast", SingleR)]
brca3p_L2[, SingleR:=ifelse(grepl("Monocyte",SingleR),"Monocyte", SingleR)]
brca3p_L2[, SingleR:=ifelse(grepl("Neutrophil",SingleR),"Neutrophil", SingleR)]
brca3p_L2[, SingleR:=ifelse(SingleR %in% c('B','T','NK', 'Macrophage', 'Mast', 'Dendritic', 'Neutrophil', 'Monocyte'), SingleR, 'Other')]

brca3p_L2[, garnett:=ifelse(grepl("Neutrophil",garnett),"Neutrophil", garnett)]
brca3p_L2[, garnett:=ifelse(grepl("B cells",garnett),"B", garnett)]
brca3p_L2[, garnett:=ifelse(grepl("T cells",garnett),"T", garnett)]
brca3p_L2[, garnett:=ifelse(grepl("NK",garnett),"NK", garnett)]
brca3p_L2[, garnett:=ifelse(grepl("Mono",garnett),"Monocyte", garnett)]
brca3p_L2[, garnett:=ifelse(grepl("Dend",garnett),"Dendritic", garnett)]
brca3p_L2[, garnett:=ifelse(garnett %in% c('B','T','NK', 'Macrophage', 'Mast', 'Dendritic', 'Neutrophil', 'Monocyte'), garnett, 'Other')]


brca3p_L2[, ctrl:=ifelse(grepl("L:T|convT", ctrl), 'T', ctrl)]
brca3p_L2[, ctrl:=ifelse(grepl("L:NK", ctrl), 'NK', ctrl)]
brca3p_L2[, ctrl:=ifelse(grepl("L:B", ctrl), 'B', ctrl)]
brca3p_L2[, ctrl:=ifelse(grepl("M:Mono", ctrl), 'Monocyte', ctrl)]
brca3p_L2[, ctrl:=ifelse(grepl("M:Mac", ctrl), 'Macrophage', ctrl)]
brca3p_L2[, ctrl:=ifelse(grepl("M:Neu", ctrl), 'Neutrophil', ctrl)]
brca3p_L2[, ctrl:=ifelse(grepl("M:Mast", ctrl), 'Mast', ctrl)]
brca3p_L2[, ctrl:=ifelse(grepl("DC", ctrl), 'Dendritic', ctrl)]
brca3p_L2[, ctrl:=ifelse(ctrl %in% c('B','T','NK', 'Macrophage', 'Mast', 'Dendritic', 'Neutrophil', 'Monocyte'), ctrl, 'Other')]




table(data.table(Known=brca3p_L2$original_annotation, annot=brca3p_L2$ctrl)) %>%
  plot_heatmap(title = "Ctrl", palette= "RdPu",
               minshow = 0, size =10, size2= 3, minpct=0, legend.pos = "none",
               ord1 = brca3p_L2_celltypes,
               ord2 = brca3p_L2_celltypes,
               measures = c('Recall', 'Precision')
  )




brca3p.ctrl.L2.recall <- c(94, 97, 68, 54, 15, 95, 74, 71)
brca3p.ctrl.L2.ppv <- c(87, 98,58, 50, 83, 61, 51, 91)




table(data.table(Known=brca3p_L2$original_annotation, annot=brca3p_L2$ImmC)) %>%
  plot_heatmap(title = "ImmC", palette= "RdPu",
               minshow = 0, size =10, size2= 3, minpct=0, legend.pos = "none",
               ord1 = brca3p_L2_celltypes,
               ord2 = brca3p_L2_celltypes,
               measures = c('Recall', 'Precision')
  )



brca3p.ImmC.L2.recall <- c(82,96, 70,49, 33,95,74,72)
brca3p.ImmC.L2.ppv <- c(84,99,62,52,71,58,52,86)


table(data.table(Known=brca3p_L2$original_annotation, annot=brca3p_L2$SingleR)) %>%
  plot_heatmap(title = "SingleR", palette= "RdPu",
               minshow = 0, size =10, size2= 3, minpct=0, legend.pos = "none",
               ord1 = brca3p_L2_celltypes,
               ord2 = brca3p_L2_celltypes,
               measures = c('Recall', 'Precision')
  )



brca3p.SinleR.L2.recall <- c(86,92,59,3,70, 76,74,NA)
brca3p.SingleR.L2.ppv <- c(95,97,31,22,50,70,80, NA)



table(data.table(Known=brca3p_L2$original_annotation, annot=brca3p_L2$garnett)) %>%
  plot_heatmap(title = "Garnett", palette= "RdPu",
               minshow = 0, size =10, size2= 3, minpct=0, legend.pos = "none",
               ord1 = brca3p_L2_celltypes,
               ord2 = brca3p_L2_celltypes,
               measures = c('Recall', 'Precision')
  )


brca3p.Garnett.L2.recall <- c(94, 86, 52, 19, 42, NA, NA, NA)
brca3p.Garnett.L2.ppv <- c(97, 99, 35, 49, 29, NA, NA, NA)


brca3p_L3_celltypes <- c('CD4+T', 'CD8+T', 'mDC', 'pDC')
brca3p_L3 <- brca3p_dt[!grepl("NKT", original_annotation), ]

brca3p_L3[, original_annotation:=ifelse(grepl("mDC", original_annotation), 'mDC', original_annotation)]
brca3p_L3[, original_annotation:=ifelse(grepl("pDC", original_annotation), 'pDC', original_annotation)]
brca3p_L3[, original_annotation:=ifelse(grepl("CD4|Treg", original_annotation), 'CD4+T', original_annotation)]
brca3p_L3[, original_annotation:=ifelse(grepl("CD8", original_annotation), 'CD8+T', original_annotation)]
brca3p_L3[, original_annotation:=ifelse(original_annotation %in% c('CD4+T', 'CD8+T', 'mDC', 'pDC'), original_annotation, 'Other')]


brca3p_L3[, ImmC:=ifelse(grepl("L:T:CD4", ImmC), 'CD4+T', ImmC)]
brca3p_L3[, ImmC:=ifelse(grepl("L:T:CD8", ImmC), 'CD8+T', ImmC)]
brca3p_L3[, ImmC:=ifelse(grepl("cDC", ImmC), 'mDC', ImmC)]
brca3p_L3[, ImmC:=ifelse(grepl("pDC", ImmC), 'pDC', ImmC)]
brca3p_L3[, ImmC:=ifelse(ImmC %in% c('CD4+T', 'CD8+T', 'mDC', 'pDC'), ImmC, 'Other')]

brca3p_L3[, SingleR:=ifelse(grepl("T_cell:CD4", SingleR), 'CD4+T', SingleR)]
brca3p_L3[, SingleR:=ifelse(grepl("T_cell:CD8", SingleR), 'CD8+T', SingleR)]
brca3p_L3[, SingleR:=ifelse(grepl("DC:mono", SingleR), 'mDC', SingleR)]
brca3p_L3[, SingleR:=ifelse(grepl("DC:pl", SingleR), 'pDC', SingleR)]
brca3p_L3[, SingleR:=ifelse(SingleR %in% c('CD4+T', 'CD8+T', 'mDC', 'pDC'), SingleR, 'Other')]

brca3p_L3[, garnett:=ifelse(grepl("CD4", garnett), 'CD4+T', garnett)]
brca3p_L3[, garnett:=ifelse(grepl("CD8", garnett), 'CD8+T', garnett)]
brca3p_L3[, garnett:=ifelse(garnett %in% c('CD4+T', 'CD8+T', 'mDC', 'pDC'), garnett, 'Other')]


table(data.table(Known=brca3p_L3$original_annotation, annot=brca3p_L3$ImmC)) %>%
  plot_heatmap(title = "ImmC", palette= "RdPu",
               minshow = 0, size =10, size2= 3, minpct=0, legend.pos = "none",
               ord1 = brca3p_L3_celltypes,
               ord2 = brca3p_L3_celltypes,
               measures = c('Recall', 'Precision')
  )

brca3p.ImmC.L3.recall <- c(85,46,23,67)
brca3p.ImmC.L3.ppv <- c(73,66,60,12)


table(data.table(Known=brca3p_L3$original_annotation, annot=brca3p_L3$SingleR)) %>%
  plot_heatmap(title = "SingleR", palette= "RdPu",
               minshow = 0, size =10, size2= 3, minpct=0, legend.pos = "none",
               ord1 = brca3p_L3_celltypes,
               ord2 = brca3p_L3_celltypes,
               measures = c('Recall', 'Precision')
  )


brca3p.SingleR.L3.recall <- c(96,24,3,NA)
brca3p.SingleR.L3.ppv <- c(71,73,18,NA)


table(data.table(Known=brca3p_L3$original_annotation, annot=brca3p_L3$garnett)) %>%
  plot_heatmap(title = "Garnett", palette= "RdPu",
               minshow = 0, size =10, size2= 3, minpct=0, legend.pos = "none",
               ord1 = brca3p_L3_celltypes,
               ord2 = brca3p_L3_celltypes,
               measures = c('Recall', 'Precision')
  )


brca3p.Garnett.L3.recall <- c(31,7,NA,NA)
brca3p.Garnett.L3.ppv <- c(72,94,NA,NA)






