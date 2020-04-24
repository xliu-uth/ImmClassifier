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
               minshow = 19, size =10, size2= 3, minpct=0, legend.pos = "none",
               ord1 = c('L', 'M'),
               ord2 = c( 'L', 'M'),
               measures = c('Recall', 'Precision')
  )



brca3p.SingleR.L1.recall <- c(95, 90)
brca3p.SingleR.L1.ppv <- c(99, 97)


table(data.table(Known=brca3p_L1$original_annotation, annot=brca3p_L1$garnett)) %>%
  plot_heatmap(title = "Garnett", palette= "RdPu",
               minshow = 19, size =10, size2= 3, minpct=0, legend.pos = "none",
               ord1 = c('L', 'M'),
               ord2 = c( 'L', 'M'),
               measures = c('Recall', 'Precision')
  )


brca3p.Garnett.L1.recall <- c(87, 54)
brca3p.Garnett.L1.ppv <- c(97, 92)


table(data.table(Known=brca3p_L1$original_annotation, annot=brca3p_L1$ctrl)) %>%
  plot_heatmap(title = "Ctrl", palette= "RdPu",
               minshow = 19, size =10, size2= 3, minpct=0, legend.pos = "none",
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








  mutate(ImmClassifier = ifelse(grepl("L:T|convT",ImmClassifier),   "T", ImmClassifier)) %>%
  mutate(ImmClassifier = ifelse(grepl("L:NK",ImmClassifier),   "NK", ImmClassifier)) %>%
  mutate(ImmClassifier = ifelse(grepl("L:B",ImmClassifier),   "B", ImmClassifier)) %>%
  mutate(ImmClassifier = ifelse(grepl("M:Mono",ImmClassifier),   "Monocyte", ImmClassifier)) %>%
  mutate(ImmClassifier = ifelse(grepl("M:Mac",ImmClassifier),   "Macrophage", ImmClassifier)) %>%
  mutate(ImmClassifier = ifelse(grepl("M:Neu",ImmClassifier),   "Neutrophil", ImmClassifier)) %>%
  mutate(ImmClassifier = ifelse(grepl("M:Mast",ImmClassifier),   "Mast", ImmClassifier)) %>%
  mutate(ImmClassifier = ifelse(grepl("DC",ImmClassifier),   "Dendritic", ImmClassifier)) %>%
  mutate(ImmClassifier = ifelse(ImmClassifier %in% c('B','T','NK', 'Macrophage', 'Mast', 'Dendritic', 'Neutrophil', 'Monocyte'), ImmClassifier, 'Other')) %>%

  mutate(SingleR = ifelse(grepl("B_cell",SingleR),   "B", SingleR)) %>%
  mutate(SingleR = ifelse(grepl("T_cell",SingleR),   "T", SingleR)) %>%
  mutate(SingleR = ifelse(grepl("NK",SingleR),   "NK", SingleR)) %>%
  mutate(SingleR = ifelse(grepl("DC",SingleR),   "Dendritic", SingleR)) %>%
  mutate(SingleR = ifelse(grepl("Monocyte",SingleR),   "Monocyte", SingleR)) %>%
  mutate(SingleR = ifelse(grepl("Macrophage",SingleR),   "Macrophage", SingleR)) %>%
  mutate(SingleR = ifelse(grepl("Mast",SingleR),   "Mast", SingleR)) %>%
  mutate(SingleR = ifelse(grepl("Neutrophil",SingleR),   "Neutrophil", SingleR)) %>%
  mutate(SingleR = ifelse(SingleR %in% c('B','T','NK', 'Macrophage', 'Mast', 'Dendritic', 'Neutrophil', 'Monocyte'), SingleR, 'Other')) %>%

  mutate(Garnett = ifelse(grepl("B cells",Garnett),   "B", Garnett)) %>%
  mutate(Garnett = ifelse(grepl("T cells",Garnett),   "T", Garnett)) %>%
  mutate(Garnett = ifelse(grepl("NK",Garnett),   "NK", Garnett)) %>%
  mutate(Garnett = ifelse(grepl("Mono",Garnett),   "Monocyte", Garnett)) %>%
  mutate(Garnett = ifelse(grepl("Dend",Garnett),   "Dendritic", Garnett)) %>%

  mutate(Garnett = ifelse(Garnett %in% c('B','T','NK', 'Macrophage', 'Mast', 'Dendritic', 'Neutrophil', 'Monocyte'), Garnett, 'Other'))













