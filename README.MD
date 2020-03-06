# ImmClassifier

**Update: January 2, 2020**

Knowledge-based and lineage-driven immune cell classification in single-cell RNA-Seq data

Maintainer: Xuan Liu <xuan.liu.1@uth.tmc.edu>
            Sara Gosline <sara.gosline@sagebase.org>

# Description

ImmClassifier (Immune cell classifier), a knowledge-based and lineage-driven immune cell  classification algorithm with fine annotation granularity yet high prediction accuracy. ImmClassifer seamlessly integrates the biology of immune cell differentiation, the strength of heterogeneous reference datasets and the state-of-art machine learning models. ImmClassifier cascades a machine learning module and a deep learning module.

# Install


THIS IS GOOD, BUT YOU WILL ALSO NEED TO PROVIDE STEP-BY-STEP INSTRUCTIONS ON HOW TO CLONE THIS REPOSITORY, AND WHICH DIRECTORY TO RUN THIS COMMAND FROM.
The docker image can be built locally from this repository using the following command:
```
docker build . -t imm-classifier
```


MAKE THIS THE MAIN INSTALLATION PROCEDURE.
Alternatively you can pull a fully working docker image using this command:
```
docker pull sgosline/imm-classifier
```

# Prerequisites

All pre-requisites are in the Docker image. We recommend using this image instead of installing locally.

# Usage Examples

ImmClassifier runs a series of three individual commands using both R and Python. It requires two parameters:
- the input file
- the output prefix

### Input File

This file is a matrix of gene counts where the rows represent gene names with the HUGO identifiers and the columns represent individual cells.

PROVIDE LINK TO THE SAMPLE MATRIX.

### Output File

This file is a matrix of cell type predictions for each cell. 

PROVIDE LINK TO SOME PRE-COMPUTED OUTPUT FROM THE SAMPLE MATRIX, SO PEOPLE CAN CHECK TO MAKE SURE THEIR DOCKER IS RUNNING CORRECTLY.

### To run
To run the command you must provide the path to your input file, the output prefix and mount the `/tmp` directory to get the output files.
```
docker run --volume $PWD:/tmp -ti sgosline/imm-classifier Rscript /ImmClassifier/bin/runIC.R --input test/bulk.logrma.txt --output testOut
```

I HAVE MADE SOME CHANGES TO SIMPLIFY THIS A BIT.  TO RUN TEST FILE:
docker run --volume $PWD:/tmp -ti sgosline/imm-classifier --input test/bulk.logrma.txt --output testOut

docker run --volume $PWD:/tmp -ti sgosline/imm-classifier --input <your_file> --output testOut

where <your_file> is a file in your local directory.

The output files will be written to your local directory with the prefix specified by the `output` argument.
