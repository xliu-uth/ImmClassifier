# ImmClassifier

**Update: September 9, 2020**

Knowledge-based and lineage-driven immune cell classification in single-cell RNA-Seq data

Maintainer: Xuan Liu <xuan.liu.1@uth.tmc.edu>
            Sara Gosline <sara.gosline@sagebase.org>

# Description

ImmClassifier (Immune cell classifier), a knowledge-based and lineage-driven immune cell classification algorithm with fine annotation granularity yet high prediction accuracy. ImmClassifer seamlessly integrates the biology of immune cell differentiation, the strength of heterogeneous reference datasets and the state-of-art machine learning models. ImmClassifier cascades a machine learning module and a deep learning module.

# Install

We have provided a Docker container that includes ImmClassifier and 
all its dependencies.  To get the container, you must have Docker installed
and use this command:
```
docker pull sgosline/imm-classifier
```


Alternatively, you can also build locally and install from the GitHub source.
To do this, you must have both git and Docker installed and then
run the commands:

```
git clone https://github.com/sgosline/ImmClassifier.git

cd ImmClassifier

docker build . -t imm-classifier
```


# Prerequisites

All pre-requisites are in the Docker image. We recommend using this image instead of installing locally.


# Usage Examples

ImmClassifier runs a series of three individual commands using both R and Python. It requires two parameters:
- the input file
- the output prefix


### Input File

This file is a matrix of gene expression values where the rows represent gene names with the HUGO identifiers and the columns represent individual cells.  
For the gene expression values, we have tested log_2 CPM (counts per million), log_2 TPM (transcripts per million), and microarray RMA values with good results.

An example file is at:
https://github.com/xliu-uth/ImmClassifier/blob/master/test/bulk.logrma.txt


### Output File

This file is a matrix of cell type predictions for each cell.

The output for the exmple above is at:
https://github.com/xliu-uth/ImmClassifier/blob/master/output/bulk.output.txt


### To run

To run the command you must provide the path to your input file, the output prefix and mount the `/tmp` directory to get the output files.
```
docker run --volume $PWD:/tmp -ti sgosline/imm-classifier --input `input_file` --output `output_prefix`
```

`input_file` is the name of input file.  It needs to be in the local 
directory for docker to find it.  If you are a docker expert, you may specify files in other directories, but you will need to bind the directory using the docker --volume command.  

The output files will be written to your local directory with the prefix specified by the `output_prefix` argument.   ImmClassifier will generate three
files:
* `output_prefix`.dnn.input.txt
* `output_prefix`.deeplearning.ontotree.stats.txt
* `output_prefix`.output.txt

The `output_prefix`.output.txt file contains the final predictions.
The other files contain intermediate results.

# Versions

* Version March 10, 2020, the first official release
* Version September 9, 2020, the cell hierarchy is updated
