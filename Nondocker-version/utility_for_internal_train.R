

# generate mixed bits



cover_set <- function(leaves){
  cset <- c()

  for(leaf in leaves){


    for (node in strsplit(leaf, ";")[[1]]){

      elements <- strsplit(node, ":")[[1]]
      i <- length(elements)

      current <- elements[1]

      cset <- c(cset, current)
      while (i > 1){
        current <- elements[1]
        for(j in 2:i){

          current <- paste(current, elements[j], sep = ":")

          cset <- c(cset, current)
        }
        i <- i-1
      }




    }

  }
  return (cset)
}




# create child-parent-link

convert_to_bits <- function(nodes, ref.nodes){

  target.bits <- rep(0, length(ref.nodes))
  names(target.bits) <- ref.nodes


  for (node in strsplit(nodes, ";")[[1]]){

    elements <- strsplit(node, ":")[[1]]

    i <- 1
    current <- ""
    while(i <= length(elements)){

      if(i == 1){
        current <- paste0(current, elements[i])
      }else{
        current <- paste(current, elements[i], sep = ":")
      }
      target.bits[current] <- 1
      i <- i+1
    }

  }

  #print (paste0("return ",paste(target.bits, collapse = ",")))
  return(paste(target.bits, collapse = ","))
}



# prepare for deep learning training input


# generate training datasets for deep learning
annotation <- read.table('annotation_data/dataset.txt', header = T, sep = "\t", stringsAsFactors=F)
annotation <- annotation %>% filter(!Hierarchy == "X")
rownames(annotation) <- annotation$ClusterID
valid.classes <- annotation %>% count(ClusterID) %>% select(ClusterID) %>% unlist
annotation %>% select(Hierarchy, CellOntology) %>% table %>% apply(1, function(x) names(x[x>0]))


ref.nodes <-  annotation %>% select(Hierarchy) %>% unlist %>% unique %>% cover_set %>% unique
deeplearn.train.sample <- deeplearn.train.sample %>% mutate(Evopath = convert_to_bits(Hierarchy, ref.nodes))

write.table(deeplearn.train.sample, 'tensorflow/input/deeplearn.train.balance.input.txt', quote = F, sep = "\t", row.names = F)


deeplearn.train <- readRDS('tensorflow/input/deeplearn.train.rds')
#238,213 cells
#deeplearn.train <- deeplearn.train %>% mutate(cell = rownames(deeplearn.train))
deeplearn.train <- deeplearn.train %>% filter(clusterID %in% valid.classes)

deeplearn.train <- deeplearn.train %>%
  mutate(Hierarchy=annotation[clusterID, 'Hierarchy']) %>%
  filter(!is.na(Hierarchy))



ref.nodes <-  annotation %>% select(Hierarchy) %>% unlist %>% unique %>% cover_set %>% unique
deeplearn.train.sample <- deeplearn.train.sample %>% mutate(Evopath = convert_to_bits(Hierarchy, ref.nodes))

write.table(deeplearn.train.sample, 'tensorflow/input/deeplearn.train.balance.input.txt', quote = F, sep = "\t", row.names = F)


# generate pre-trained dnn models

# fix random seed for reproducibility
seed = 100
np.random.seed(seed)
input_file = pd.read_csv("./tensorflow/input/deeplearn.train.balance.input.txt", sep = "\t")
data =input_file.values


norm_method = "ontotree"

X_train = data[:, 0:171].astype(float)
y_train = np.array([[int(x) for x in str.split(y, sep = ",")] for y in data[:, 174]])
X_train  = logit(X_train, norm_method)

# Train the models from the begining


num_model = 10
stochastic_models = [multilabel_model_thread(input_dim = X_train.shape[1], output_dim = y_train.shape[1]) for i in range(0,num_model)]
y_train_stochastic = y_train

for i in range(0, num_model):
  stochastic_models[i].fit(X_train, y_train_stochastic, epochs = 5, batch_size = 2048)

# serialize weights to HDF5
[stochastic_models[i].save("./tensorflow/pre-trained-models/model_%s_%d_n10.h5" % (norm_method, i) ) for i in range(0, 10)]
print("Saved trained model to disk")




