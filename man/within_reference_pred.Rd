% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mlr.R
\name{within_reference_pred}
\alias{within_reference_pred}
\title{Predict cell type and return probabilities across all cell types within one training dataset}
\usage{
within_reference_pred(queryfile.path, output.prefix = "query",
  num.cores = 1, mode = "run")
}
\arguments{
\item{mat}{queryfile.path: string, path of the query dataset in tab-delimited text file with
the rows as gene symbol, and columns as sampleID.}

\item{output.prefix:}{string, the prefix of query dataset}
}
\value{
a list containing trained random forest model, prediction of training part
  and held-out part of reference dataset, prediction of query dataset, evaluation
  result using in training and test parts of the trainng dataset.
}
\description{
Predict cell type and return probabilities across all cell types within one training dataset
}
\examples{
within_reference_pred(queryfile.path = "test/bulk.logrma.txt", output.prefix = "bulk", mode = "run")
}
