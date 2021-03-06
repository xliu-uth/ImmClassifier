% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assign_cell.R
\name{assign_dataset}
\alias{assign_dataset}
\title{Assign final cell type label for the whole query dataset}
\usage{
assign_dataset(output.prefix)
}
\arguments{
\item{output.prefix:}{string, the prefix of query dataset}
}
\value{
a dataframe with column1 as cell id and column2 as the predicted cell type
}
\description{
Assign final cell type label for the whole query dataset
}
\examples{
assign_dataset('bulk')
}
