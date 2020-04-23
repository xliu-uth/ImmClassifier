# data located at unicron:/home/xuanliu/data/Projects/ImmClassifier-Final-Version

# on unicron
# .libPaths( "/data/xuanliu/R-3.6.1/library")
library(data.table)


# ctrl_1: cell normalized expr (log2RMA, log2CPM and log2TPM) -> neural network -> multi- classification
# ctrl_2: cell normalized expr -> combat -> neural network -> multi-classification

# need to generate a cell-known_celltype table


celltype_map <- fread('annotation_data/dataset.txt')
setkey(celltype_map, ClusterID)


# dnn input


