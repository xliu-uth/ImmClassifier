# data located at unicron:/home/xuanliu/data/Projects/ImmClassifier-Final-Version

# on unicron

conda activate PyCPU


# fix random seed for reproducibility
seed = 100
np.random.seed(seed)
input_file = pd.read_csv("./tensorflow/input/deeplearn.train.balance.input.txt", sep = "\t")
data =input_file.values


norm_method = "ontotree"


X_train = data[:, 0:171].astype(float)
y_train = np.array(data[:, 173])
X_train  = logit(X_train, norm_method)

# Train the models from the begining

from sklearn.preprocessing import LabelBinarizer
encoder = LabelBinarizer()
encoder.fit(y_train)
transformed_label = encoder.transform(y_train)


num_model = 10
y_train_stochastic = transformed_label
stochastic_models = [multilabel_model_thread(input_dim = X_train.shape[1], output_dim = y_train_stochastic.shape[1]) for i in range(0,num_model)]


for i in range(0, num_model):
  stochastic_models[i].fit(X_train, y_train_stochastic, epochs = 5, batch_size = 2048)

# serialize weights to HDF5
[stochastic_models[i].save("./tensorflow/baseline-trained-models/model_%s_%d_n10.h5" % (norm_method, i) ) for i in range(0, 10)]
print("Saved trained model to disk")

np.savetxt("./tensorflow/baseline-trained-models/char_encoder.txt", encoder.classes_, fmt="%s")





def dnn_predict(output_prefix):
    np.random.seed(100)
    qfpath = "./tensorflow/input/%s.dnn.input.txt" % (output_prefix)
    print ("load query file %s" % qfpath)
    norm_method = "ontotree"
    query_file = pd.read_csv(qfpath, sep = "\t")
    qdata =query_file.values
    print ("perform normalization")
    # 1st column as cell id
    X_new = qdata[:, 1:172].astype(float)
    X_new = logit(X_new, norm_method)
    num_model = 10
    print ("load pre-trained dnn models")
    stochastic_models = [keras.models.load_model("./tensorflow/baseline-trained-models/model_%s_%d_n10.h5" % (norm_method, i)) for i in range(0,num_model)]
    print ("predict query dataset using dnn models")
    Y_new_pred_stochastic = np.array([m.predict(X_new) for m in stochastic_models])
    Y_new_pred_stochastic_mean = np.mean(Y_new_pred_stochastic, axis = 0)
    Y_new_pred_stochastic_std = np.std(Y_new_pred_stochastic, axis = 0)
    print ("save results to disk")
    np.savetxt('./tensorflow/baseline-trained-models/%s.deeplearning.%s.stats.txt' % (output_prefix, norm_method), \
               np.append(np.append(qdata[:, 0].reshape(qdata.shape[0], 1), \
                                   Y_new_pred_stochastic_mean,  1), \
                         Y_new_pred_stochastic_std, 1), fmt="%s")




dnn_predict('bulk')
dnn_predict('brca3p')
dnn_predict('brca5p')
dnn_predict('hcc')
dnn_predict('pbmc68k')





assign_cell_label <- function(vstats){
  probs <- vstats[1:35]
  names(probs) <- gsub(".prob", "", names(probs))
  # deal with ties, choose the lowest std/mean
  candidates <- names(probs[probs==max(probs)])

  return (candidates[order(vstats[paste0(candidates, ".std")]/vstats[paste0(candidates, ".prob")])[1]])
}



assign_dataset <- function(output.prefix,deep.learning.file){

  ref.nodes <- fread('tensorflow/baseline-trained-models/char_encoder.txt', header = F)

  norm.method <- "ontotree"
  fpath <- deep.learning.file #paste0('tensorflow/output/', output.prefix,  '.deeplearning.', norm.method, '.stats.txt', sep = "")
  print (paste0('Read ', fpath))
  dnn.stats <- read.table(fpath, header = F, sep = " ",stringsAsFactors=F)

  cnames <- c("Cell",paste0(ref.nodes$V1, ".prob"), paste0(ref.nodes$V1, ".std"))
  colnames(dnn.stats) <- cnames
  rownames(dnn.stats) <- dnn.stats$Cell
  print (paste0('Assign the final labels'))
  assign.labels <- dnn.stats[, -1] %>%
    apply(1, function(x) assign_cell_label(x))
  print (paste0('Finish assigning the final labels'))
  outfile= paste0("tensorflow/baseline-trained-models/", output.prefix, ".output.txt")
  write.table(data.frame(Cell=dnn.stats$Cell,
                         ImmClassifier_prediction= assign.labels),
              outfile,
              quote = F, sep = "\t", row.names = F)
  #    return (data.frame(Cell=dnn.stats$Cell, ImmClassifier_prediction=assign.labels, stringsAsFactors = F))
  return(outfile)


}

assign_dataset('bulk', 'tensorflow/baseline-trained-models/bulk.deeplearning.ontotree.stats.txt')
assign_dataset('brca3p', 'tensorflow/baseline-trained-models/brca3p.deeplearning.ontotree.stats.txt')
assign_dataset('brca5p', 'tensorflow/baseline-trained-models/brca5p.deeplearning.ontotree.stats.txt')

assign_dataset('hcc', 'tensorflow/baseline-trained-models/hcc.deeplearning.ontotree.stats.txt')

assign_dataset('pbmc68k', 'tensorflow/baseline-trained-models/pbmc68k.deeplearning.ontotree.stats.txt')




