# Get performance per granularity level

##############################################################################
#                                brca3p_level 1                              #
##############################################################################

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



##############################################################################
#                                brca3p_level 2                              #
##############################################################################


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



##############################################################################
#                                brca3p_level 3                              #
##############################################################################


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


brca3p_L3[, ctrl:=ifelse(grepl("L:T:CD4", ctrl), 'CD4+T', ctrl)]
brca3p_L3[, ctrl:=ifelse(grepl("L:T:CD8", ctrl), 'CD8+T', ctrl)]
brca3p_L3[, ctrl:=ifelse(grepl("cDC", ctrl), 'mDC', ctrl)]
brca3p_L3[, ctrl:=ifelse(grepl("pDC", ctrl), 'pDC', ctrl)]
brca3p_L3[, ctrl:=ifelse(ctrl %in% c('CD4+T', 'CD8+T', 'mDC', 'pDC'), ctrl, 'Other')]


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

table(data.table(Known=brca3p_L3$original_annotation, annot=brca3p_L3$ctrl)) %>%
  plot_heatmap(title = "Ctrl", palette= "RdPu",
               minshow = 0, size =10, size2= 3, minpct=0, legend.pos = "none",
               ord1 = brca3p_L3_celltypes,
               ord2 = brca3p_L3_celltypes,
               measures = c('Recall', 'Precision')
  )

brca3p.ctrl.L3.recall <- c(87,43,32, 81)
brca3p.ctrl.L3.ppv <- c(73,66,56,15)




##############################################################################
#                                brca3p_level 4                              #
##############################################################################

brca3p_L4_celltypes <-  c('CD4+ TNaive', 'CD4+ TCM','CD4+ TEM','CD8+ TNaive', 'CD8+ TCM', 'CD8+ TEM', 'Treg' )
brca3p_L4 <- brca3p_dt[!grepl("NKT", original_annotation), ]


brca3p_L4[, original_annotation:=ifelse(grepl("CD4\\+CM", original_annotation), 'CD4+ TCM', original_annotation)]
brca3p_L4[, original_annotation:=ifelse(grepl("CD4\\+NAIVE", original_annotation), 'CD4+ TNaive', original_annotation)]
brca3p_L4[, original_annotation:=ifelse(grepl("CD4\\+EM", original_annotation), 'CD4+ TEM', original_annotation)]
brca3p_L4[, original_annotation:=ifelse(grepl("CD8\\+CM", original_annotation), 'CD8+ TCM', original_annotation)]
brca3p_L4[, original_annotation:=ifelse(grepl("CD8\\+NAIVE", original_annotation), 'CD8+ TNaive', original_annotation)]
brca3p_L4[, original_annotation:=ifelse(grepl("CD8\\+EM", original_annotation), 'CD8+ TEM', original_annotation)]
brca3p_L4[, original_annotation:=ifelse(grepl("T:Reg", original_annotation), 'Treg', original_annotation)]



brca3p_L4[, original_annotation:=ifelse(original_annotation %in%c( "CD4+ TCM","CD4+ TNaive", "CD4+ TEM",
                                                                   "Treg", "CD8+ TCM","CD8+ TNaive", "CD8+ TEM"), original_annotation, 'Other')]

brca3p_L4[, ImmC:=ifelse(grepl("L:T:CD4:CM", ImmC), 'CD4+ TCM', ImmC)]
brca3p_L4[, ImmC:=ifelse(grepl("L:T:CD4:Na", ImmC), 'CD4+ TNaive', ImmC)]
brca3p_L4[, ImmC:=ifelse(grepl("L:T:CD4:EM", ImmC), 'CD4+ TEM', ImmC)]
brca3p_L4[, ImmC:=ifelse(grepl("L:T:CD4:CM", ImmC), 'CD4+ TCM', ImmC)]
brca3p_L4[, ImmC:=ifelse(grepl("L:T:CD4:Treg", ImmC), 'Treg', ImmC)]
brca3p_L4[, ImmC:=ifelse(grepl("L:T:CD8:CM", ImmC), 'CD8+ TCM', ImmC)]
brca3p_L4[, ImmC:=ifelse(grepl("L:T:CD8:Na", ImmC), 'CD8+ TNaive', ImmC)]
brca3p_L4[, ImmC:=ifelse(grepl("L:T:CD8:EM", ImmC), 'CD8+ TEM', ImmC)]

brca3p_L4[, ImmC:=ifelse(ImmC %in%c( "CD4+ TCM","CD4+ TNaive", "CD4+ TEM",
                                     "Treg", "CD8+ TCM","CD8+ TNaive", "CD8+ TEM"), ImmC, 'Other')]




brca3p_L4[, SingleR:=ifelse(grepl("T_cell:CD4\\+_central_memory", SingleR), 'CD4+ TCM', SingleR)]
brca3p_L4[, SingleR:=ifelse(grepl("T_cell:CD4\\+_effector_memory", SingleR), 'CD4+ TEM', SingleR)]
brca3p_L4[, SingleR:=ifelse(grepl("T_cell:CD4\\+_Naive", SingleR), 'CD4+ TNaive', SingleR)]
brca3p_L4[, SingleR:=ifelse(grepl("T_cell:CD8\\+_naive", SingleR), 'CD8+ TNaive', SingleR)]
brca3p_L4[, SingleR:=ifelse(grepl("T_cell:CD8\\+_Central_memory", SingleR), 'CD8+ TCM', SingleR)]
brca3p_L4[, SingleR:=ifelse(grepl("T_cell:CD8\\+_effector_memory", SingleR), 'CD8+ TEM', SingleR)]
brca3p_L4[, SingleR:=ifelse(grepl("Treg", SingleR), 'Treg', SingleR)]

brca3p_L4[, SingleR:=ifelse(SingleR %in%c( "CD4+ TCM","CD4+ TNaive", "CD4+ TEM",
                                           "Treg", "CD8+ TCM","CD8+ TNaive", "CD8+ TEM"), SingleR, 'Other')]



brca3p_L4[, garnett:=ifelse(grepl("CD4", garnett), 'CD4+ T', garnett)]
brca3p_L4[, garnett:=ifelse(grepl("CD8", garnett), 'CD8+ T', garnett)]

brca3p_L4[, garnett:=ifelse(garnett %in%c( "CD4+ TCM","CD4+ TNaive", "CD4+ TEM",
                                           "Treg", "CD8+ TCM","CD8+ TNaive", "CD8+ TEM"), garnett, 'Other')]



brca3p_L4[, ctrl:=ifelse(grepl("L:T:CD4:CM", ctrl), 'CD4+ TCM', ctrl)]
brca3p_L4[, ctrl:=ifelse(grepl("L:T:CD4:Na", ctrl), 'CD4+ TNaive', ctrl)]
brca3p_L4[, ctrl:=ifelse(grepl("L:T:CD4:EM", ctrl), 'CD4+ TEM', ctrl)]
brca3p_L4[, ctrl:=ifelse(grepl("L:T:CD4:CM", ctrl), 'CD4+ TCM', ctrl)]
brca3p_L4[, ctrl:=ifelse(grepl("L:T:CD4:Treg", ctrl), 'Treg', ctrl)]
brca3p_L4[, ctrl:=ifelse(grepl("L:T:CD8:CM", ctrl), 'CD8+ TCM', ctrl)]
brca3p_L4[, ctrl:=ifelse(grepl("L:T:CD8:Na", ctrl), 'CD8+ TNaive', ctrl)]
brca3p_L4[, ctrl:=ifelse(grepl("L:T:CD8:EM", ctrl), 'CD8+ TEM', ctrl)]

brca3p_L4[, ctrl:=ifelse(ctrl %in%c( "CD4+ TCM","CD4+ TNaive", "CD4+ TEM",
                                     "Treg", "CD8+ TCM","CD8+ TNaive", "CD8+ TEM"), ctrl, 'Other')]



table(data.table(Known=brca3p_L4$original_annotation, annot=brca3p_L4$ImmC)) %>%
  plot_heatmap(title = "ImmC", palette= "RdPu",
               minshow = 0, size =10, size2= 3, minpct=0, legend.pos = "none",
               ord1 = brca3p_L4_celltypes,
               ord2 = brca3p_L4_celltypes,
               measures = c('Recall', 'Precision')
  )


brca3p.ImmC.L4.recall <- c(37,14,15,8, 2, 25,2)
brca3p.ImmC.L4.ppv <- c(90,3, 1, 29 ,NA, 24,9)




table(data.table(Known=brca3p_L4$original_annotation, annot=brca3p_L4$SingleR)) %>%
  plot_heatmap(title = "SingleR", palette= "RdPu",
               minshow = 0, size =10, size2= 3, minpct=0, legend.pos = "none",
               ord1 = brca3p_L4_celltypes,
               ord2 = brca3p_L4_celltypes,
               measures = c('Recall', 'Precision')
  )

brca3p.SingleR.L4.recall <- c(41,50,37,4,7,2,NA)
brca3p.SingleR.L4.ppv <- c(98,8,1,67,1,16,33)


table(data.table(Known=brca3p_L4$original_annotation, annot=brca3p_L4$garnett)) %>%
  plot_heatmap(title = "Garnett", palette= "RdPu",
               minshow = 0, size =10, size2= 3, minpct=0, legend.pos = "none",
               ord1 = brca3p_L4_celltypes,
               ord2 = brca3p_L4_celltypes,
               measures = c('Recall', 'Precision')
  )

brca3p.Garnett.L4.recall <- rep(NA, 7)
brca3p.Garnett.L4.ppv <- rep(NA, 7)




table(data.table(Known=brca3p_L4$original_annotation, annot=brca3p_L4$ctrl)) %>%
  plot_heatmap(title = "Ctrl", palette= "RdPu",
               minshow = 0, size =10, size2= 3, minpct=0, legend.pos = "none",
               ord1 = brca3p_L4_celltypes,
               ord2 = brca3p_L4_celltypes,
               measures = c('Recall', 'Precision')
  )


brca3p.ctrl.L4.recall <- c(44,6,7,3,1,23,66)
brca3p.ctrl.L4.ppv <- c(86,2,NA, 16, NA, 21, 12)





##############################################################################
#                                pbmc68k_level 1                             #
##############################################################################

pbmc68k_dt[1:3,]
pbmc68k_L1 <- pbmc68k_dt
pbmc68k_L1[, original_annotation:=ifelse(grepl("Mono|Dendritic", original_annotation), 'M', original_annotation)]
pbmc68k_L1[, original_annotation:=ifelse(grepl("T|NK|CD4|CD8|B", original_annotation), 'L', original_annotation)]
pbmc68k_L1[, original_annotation:=ifelse(grepl("CD34", original_annotation), 'CD34+', original_annotation)]

pbmc68k_L1[, ImmC:=ifelse(grepl("^L:", ImmC), 'L', ImmC)]
pbmc68k_L1[, ImmC:=ifelse(grepl("^M:", ImmC), 'M', ImmC)]
pbmc68k_L1[, ImmC:=ifelse(grepl("CD34", ImmC), 'CD34+', ImmC)]
pbmc68k_L1[, ImmC:=ifelse(ImmC %in% c('L', 'M', 'CD34+'), ImmC, 'Other')]

pbmc68k_L1[, SingleR:=ifelse(grepl("B_cell|T_cell|NK", SingleR), 'L', SingleR)]
pbmc68k_L1[, SingleR:=ifelse(grepl("Mono|Neutro|Macro", SingleR), 'M', SingleR)]
pbmc68k_L1[, SingleR:=ifelse(grepl("HSC|CD34\\+|GMP|CMP|MEP", SingleR), 'CD34+', SingleR)]
pbmc68k_L1[, SingleR:=ifelse(SingleR %in% c('L', 'M', 'CD34+'), SingleR, 'Other')]

pbmc68k_L1[, garnett:=ifelse(grepl("B cells|T cells|NK", garnett), 'L', garnett)]
pbmc68k_L1[, garnett:=ifelse(grepl("Mono|Den", garnett), 'M', garnett)]
pbmc68k_L1[, garnett:=ifelse(garnett %in% c('L', 'M', 'CD34+'), garnett, 'Other')]


pbmc68k_L1[, ctrl:=ifelse(grepl("^L:", ctrl), 'L', ctrl)]
pbmc68k_L1[, ctrl:=ifelse(grepl("^M:", ctrl), 'M', ctrl)]
pbmc68k_L1[, ctrl:=ifelse(grepl("CD34", ctrl), 'CD34+', ctrl)]
pbmc68k_L1[, ctrl:=ifelse(ctrl %in% c('L', 'M', 'CD34+'), ctrl, 'Other')]


pbmc68k_L1_celltypes <- c('L', 'M', 'CD34+')

table(data.table(Known=pbmc68k_L1$original_annotation, annot=pbmc68k_L1$ImmC)) %>%
  plot_heatmap(title = "ImmC", palette= "RdPu",
               minshow = 0, size =10, size2= 3, minpct=0, legend.pos = "none",
               ord1 = pbmc68k_L1_celltypes,
               ord2 = pbmc68k_L1_celltypes,
               measures = c('Recall', 'Precision')
  )


pbmc68k.ImmC.L1.recall <- c(100,94,17)
pbmc68k.ImmC.L1.ppv <- c(100,94,30)


table(data.table(Known=pbmc68k_L1$original_annotation, annot=pbmc68k_L1$SingleR)) %>%
  plot_heatmap(title = "SingleR", palette= "RdPu",
               minshow = 0, size =10, size2= 3, minpct=0, legend.pos = "none",
               ord1 = pbmc68k_L1_celltypes,
               ord2 = pbmc68k_L1_celltypes,
               measures = c('Recall', 'Precision')
  )

pbmc68k.SingleR.L1.recall <- c(100, 88,19)
pbmc68k.SingleR.L1.ppv <- c(99,100,78)


table(data.table(Known=pbmc68k_L1$original_annotation, annot=pbmc68k_L1$garnett)) %>%
  plot_heatmap(title = "Garnett", palette= "RdPu",
               minshow = 0, size =10, size2= 3, minpct=0, legend.pos = "none",
               ord1 = pbmc68k_L1_celltypes,
               ord2 = pbmc68k_L1_celltypes,
               measures = c('Recall', 'Precision')
  )

pbmc68k.Garnett.L1.recall <- c(100,39,7)
pbmc68k.Garnett.L1.ppv <- c(98,94,37)


table(data.table(Known=pbmc68k_L1$original_annotation, annot=pbmc68k_L1$ctrl)) %>%
  plot_heatmap(title = "Ctrl", palette= "RdPu",
               minshow = 0, size =10, size2= 3, minpct=0, legend.pos = "none",
               ord1 = pbmc68k_L1_celltypes,
               ord2 = pbmc68k_L1_celltypes,
               measures = c('Recall', 'Precision')
  )

pbmc68k.ctrl.L1.recall <- c(100,91,14)
pbmc68k.ctrl.L1.ppv <- c(99,95,38)



##############################################################################
#                                pbmc68k_level 2                             #
##############################################################################

pbmc68k_dt[1:3,]
pbmc68k_L2 <- copy(pbmc68k_dt)
pbmc68k_L2[, original_annotation:=ifelse(grepl("B", original_annotation), 'B', original_annotation)]
pbmc68k_L2[, original_annotation:=ifelse(grepl("T|CD4|CD8", original_annotation), 'T', original_annotation)]
pbmc68k_L2[, original_annotation:=ifelse(grepl("NK", original_annotation), 'NK', original_annotation)]
pbmc68k_L2[, original_annotation:=ifelse(grepl("Dendritic", original_annotation), 'Dendritic', original_annotation)]
pbmc68k_L2[, original_annotation:=ifelse(grepl("Monocyte", original_annotation), 'Monocyte', original_annotation)]




pbmc68k_L2[, ImmC:=ifelse(grepl("L:T|convT", ImmC), 'T', ImmC)]
pbmc68k_L2[, ImmC:=ifelse(grepl("L:NK", ImmC), 'NK', ImmC)]
pbmc68k_L2[, ImmC:=ifelse(grepl("L:B", ImmC), 'B', ImmC)]
pbmc68k_L2[, ImmC:=ifelse(grepl("M:Mono", ImmC), 'Monocyte', ImmC)]
pbmc68k_L2[, ImmC:=ifelse(grepl("DC", ImmC), 'Dendritic', ImmC)]
pbmc68k_L2[, ImmC:=ifelse(ImmC %in% c('B','T','NK', 'Dendritic', 'Monocyte'), ImmC, 'Other')]


pbmc68k_L2[, SingleR:=ifelse(grepl("B_cell", SingleR), 'B', SingleR)]
pbmc68k_L2[, SingleR:=ifelse(grepl("T_cell", SingleR), 'T', SingleR)]
pbmc68k_L2[, SingleR:=ifelse(grepl("NK", SingleR), 'NK', SingleR)]
pbmc68k_L2[, SingleR:=ifelse(grepl("DC", SingleR), 'Dendritic', SingleR)]
pbmc68k_L2[, SingleR:=ifelse(grepl("Monocyte", SingleR), 'Monocyte', SingleR)]
pbmc68k_L2[, SingleR:=ifelse(SingleR %in% c('B','T','NK', 'Dendritic', 'Monocyte'), SingleR, 'Other')]

pbmc68k_L2[, garnett:=ifelse(grepl("B cells", garnett), 'B', garnett)]
pbmc68k_L2[, garnett:=ifelse(grepl("T cells", garnett), 'T', garnett)]
pbmc68k_L2[, garnett:=ifelse(grepl("NK", garnett), 'NK', garnett)]
pbmc68k_L2[, garnett:=ifelse(grepl("Mono", garnett), 'Monocyte', garnett)]
pbmc68k_L2[, garnett:=ifelse(grepl("Dend", garnett), 'Dendritic', garnett)]
pbmc68k_L2[, garnett:=ifelse(garnett %in% c('B','T','NK', 'Dendritic', 'Monocyte'), garnett, 'Other')]


pbmc68k_L2[, ctrl:=ifelse(grepl("L:T|convT", ctrl), 'T', ctrl)]
pbmc68k_L2[, ctrl:=ifelse(grepl("L:NK", ctrl), 'NK', ctrl)]
pbmc68k_L2[, ctrl:=ifelse(grepl("L:B", ctrl), 'B', ctrl)]
pbmc68k_L2[, ctrl:=ifelse(grepl("M:Mono", ctrl), 'Monocyte', ctrl)]
pbmc68k_L2[, ctrl:=ifelse(grepl("DC", ctrl), 'Dendritic', ctrl)]
pbmc68k_L2[, ctrl:=ifelse(ctrl %in% c('B','T','NK', 'Dendritic', 'Monocyte'), ctrl, 'Other')]



pbmc68k_L2_celltypes <- c('B', 'T', 'NK', 'Dendritic', 'Monocyte')


table(data.table(Known=pbmc68k_L2$original_annotation, annot=pbmc68k_L2$ImmC)) %>%
  plot_heatmap(title = "ImmC", palette= "RdPu",
               minshow = 0, size =10, size2= 3, minpct=0, legend.pos = "none",
               ord1 = pbmc68k_L2_celltypes,
               ord2 = pbmc68k_L2_celltypes,
               measures = c('Recall', 'Precision')
  )


pbmc68k.ImmC.L2.recall <- c(71,94,82,32,67)
pbmc68k.ImmC.L2.ppv <- c(69,93,90,85,62)


table(data.table(Known=pbmc68k_L2$original_annotation, annot=pbmc68k_L2$SingleR)) %>%
  plot_heatmap(title = "SingleR", palette= "RdPu",
               minshow = 0, size =10, size2= 3, minpct=0, legend.pos = "none",
               ord1 = pbmc68k_L2_celltypes,
               ord2 = pbmc68k_L2_celltypes,
               measures = c('Recall', 'Precision')
  )



pbmc68k.SingleR.L2.recall <- c(65,99,49,NA,93)
pbmc68k.SingleR.L2.ppv <- c(88,88,95,NA,60)


table(data.table(Known=pbmc68k_L2$original_annotation, annot=pbmc68k_L2$garnett)) %>%
  plot_heatmap(title = "Garnett", palette= "RdPu",
               minshow = 0, size =10, size2= 3, minpct=0, legend.pos = "none",
               ord1 = pbmc68k_L2_celltypes,
               ord2 = pbmc68k_L2_celltypes,
               measures = c('Recall', 'Precision')
  )


pbmc68k.Garnett.L2.recall <- c(65, 99,51,21,38)
pbmc68k.Garnett.L2.ppv <- c(97, 88,80,81,73)



table(data.table(Known=pbmc68k_L2$original_annotation, annot=pbmc68k_L2$ctrl)) %>%
  plot_heatmap(title = "Ctrl", palette= "RdPu",
               minshow = 0, size =10, size2= 3, minpct=0, legend.pos = "none",
               ord1 = pbmc68k_L2_celltypes,
               ord2 = pbmc68k_L2_celltypes,
               measures = c('Recall', 'Precision')
  )


pbmc68k.Garnett.L2.recall <- c(69, 95,93,37,18)
pbmc68k.Garnett.L2.ppv <- c(83, 95, 83, 85, 62)




