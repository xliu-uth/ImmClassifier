ImmClassifier
Update: Oct 23, 2019

Knowledge-based and lineage-driven immune cell classification in single-cell RNA-Seq data

Maintainer: Xuan Liu <xuan.liu.1@uth.tmc.edu>

ImmClassifier (Immune cell classifier), a knowledge-based and lineage-driven immune cell  classification algorithm with fine annotation granularity yet high prediction accuracy. ImmClassifer seamlessly integrates the biology of immune cell differentiation, the strength of heterogeneous reference datasets and the state-of-art machine learning models. ImmClassifier cascades a machine learning module and a deep learning module. 

Install

Docker version on the way!

Prerequisites:

R packages: dplyr,mlr,sva,stringr,randomForest

Python modules:
              import pandas as pd
              
              from keras.models import Sequential
              
              from keras.layers import Dense
              
              from sklearn.model_selection import train_test_split
              
              import numpy as np
              
              import random
              
              from sklearn import metrics
              
              from keras.layers import Dropout
              
              import keras.models
              

Usage Examples

Input: tab-delimited gene by cell gene expression matrix in text format. The gene expression values can be logCPM,
logTPM or logRMA (microarray dataset).


ImmClassifier is implemented by a combination of R and python. 

ImmClassifier workflow contains three steps:

1. Predict the probabilities across original cell types for each reference dataset for one query dataset. 
This step is implemented using R. The R code is written in mlr.R. 

For example,  

Input: 
       queryfile.path, a string, path of query dataset (gene by cell matrix) eg: test/bulk.logrma.txt.

       output.prefix, a string, the prefix of query dataset
       
       mode, a string (default: run), "debug" or "run". "debug" mode is for time-efficient debug run
       
        
Output: output file written in "tensorflow/input/bulk.dnn.input.txt"

R Command: within_reference_pred(queryfile.path = "test/bulk.logrma.txt", output.prefix = "bulk", mode = "run")

2. Predict the probailities across cell types on cell ontology hierarchy. 

This step is implemented using Python. The python code is written in dnn.py.

Input: the prefix of query dataset

Output: output file written in tensorflow/output/bulk.deeplearning.ontotree.stats.txt

Python Command: dnn_predict("bulk")


3. Assign the cell type labels for query dataset

This step is implemented using R. The R code is written in assign_cell.R. 

For example,  

Input: the prefix of query dataset

Output: output file written in output/bulk.output.txt. The first column is cell ID and the second column is predicted cell type label by ImmClassifier

R Command: assign_dataset('bulk')

