# ImmClassifier

**Update: January 2, 2020**

Knowledge-based and lineage-driven immune cell classification in single-cell RNA-Seq data

Maintainer: Xuan Liu <xuan.liu.1@uth.tmc.edu>
            Sara Gosling

# Description

ImmClassifier (Immune cell classifier), a knowledge-based and lineage-driven immune cell  classification algorithm with fine annotation granularity yet high prediction accuracy. ImmClassifer seamlessly integrates the biology of immune cell differentiation, the strength of heterogeneous reference datasets and the state-of-art machine learning models. ImmClassifier cascades a machine learning module and a deep learning module.

# Install

The docker image can be built locally from this repository using the following command:
```
docker build . -t imm-classifier
```

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

### Output File

This file is a matrix of cell type predictions for each cell. 

### To run
To run the command you must provide the path to your input file, the output prefix and mount the `/tmp` directory to get the output files.
```
docker run --volume $PWD:/tmp -ti sgosline/imm-classifier Rscript /ImmClassifier/bin/runIC.R --input test/bulk.logrma.txt --output testOut
```

The output files will be written to your local directory with the prefix specified by the `output` argument.
