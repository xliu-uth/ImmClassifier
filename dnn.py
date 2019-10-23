import pandas as pd
from keras.models import Sequential
from keras.layers import Dense
from sklearn.model_selection import train_test_split
import numpy as np
import random
from sklearn import metrics
from keras.layers import Dropout
import keras.models

def logit(x, norm):
    if norm=='original':
    # hca
        x[:, 0:35] = x[:, 0:35] * np.sqrt(35) # by total number of cell types in annotation    
    # pbmc
        x[:, 35:69] = x[:, 35:69] * np.sqrt(34) # by total number of cell types in annotation    
    # liver
        x[:, 69:78] = x[:, 69:78] * np.sqrt(9) # by total number of cell types in annotation    
    # jcibm
        x[:, 78:97] = x[:, 78:97] * np.sqrt(19) # by total number of cell types in annotation   
    # nsclctii
        x[:, 97:139] = x[:, 97:139] * np.sqrt(42) # by total number of cell types in annotation    
    # brcat t only
        x[:, 139:148] = x[:, 139:148] * np.sqrt(9)/2 # by total number of cell types in annotation    
    # nsclc t only
        x[:, 148:171] = x[:, 148:171] * np.sqrt(23)/2 # by total number of cell types in annotation
    if norm == 'ontotree':
        x[:, 0:35] = x[:, 0:35] * np.sqrt(12) # by total number of cell types in cell ontology hierarchy
        x[:, 35:69] = x[:, 35:69] * np.sqrt(17) # by total number of cell types in cell ontology hierarchy
        x[:, 69:78] = x[:, 69:78] * np.sqrt(5) # by total number of cell types in cell ontology hierarchy
        x[:, 78:97] = x[:, 78:97] * np.sqrt(13) # by total number of cell types in cell ontology hierarchy
        x[:, 97:139] = x[:, 97:139] * np.sqrt(10) # by total number of cell types in cell ontology hierarchy
        x[:, 139:148] = x[:, 139:148] * np.sqrt(7) # by total number of cell types in cell ontology hierarchy
        x[:, 148:171] = x[:, 148:171] * np.sqrt(11) # by total number of cell types in cell ontology hierarchy
    x = x/x.sum(axis = 1)[:, np.newaxis]
    p = 0.001+x*0.998
    logit = np.log(p/(1-p))
    return logit


def dnn_predict(output_prefix):
    """
    Args:
        output_prefix (str): prefix for file output

    Returns:
    """
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
    stochastic_models = [keras.models.load_model("./tensorflow/pre-trained-models/model_%s_%d_n10.h5" % (norm_method, i)) for i in range(0,num_model)]
    print ("predict query dataset using dnn models")
    Y_new_pred_stochastic = np.array([m.predict(X_new) for m in stochastic_models])
    Y_new_pred_stochastic_mean = np.mean(Y_new_pred_stochastic, axis = 0)
    Y_new_pred_stochastic_std = np.std(Y_new_pred_stochastic, axis = 0)   
    print ("save results to disk")
    np.savetxt('./tensorflow/output/%s.deeplearning.%s.stats.txt' % (output_prefix, norm_method), \
              np.append(np.append(qdata[:, 0].reshape(qdata.shape[0], 1), \
                                  Y_new_pred_stochastic_mean,  1), \
                        Y_new_pred_stochastic_std, 1), fmt="%s")

                                      
def multilabel_model_thread(input_dim, output_dim):
 # create model
    model = Sequential()
    #model.add(Dropout(0.2))
    model.add(Dense(200, input_dim=input_dim, activation='relu'))
    model.add(Dropout(0.2))
    model.add(Dense(400, input_dim=200, activation='relu'))
    model.add(Dropout(0.2))
    model.add(Dense(200, input_dim=200, activation='relu'))
    model.add(Dropout(0.2))
    model.add(Dense(output_dim,  activation='sigmoid'))
    model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])
    return model                          

# examples
# dnn_predict("bulk")