% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sparse_matrix.R
\name{sparse.matrix}
\alias{sparse.matrix}
\title{Sparse matrix and its operations}
\usage{
sparse.matrix(i, j, x, dims = c(max(i), max(j)))
}
\arguments{
\item{i}{row indices of non-zero elements}

\item{j}{column indices of non-zero elements}

\item{x}{values of non-zero elements}

\item{dims}{dimension of the sparse matrix}
}
\value{
a sparse.matrix object
}
\description{
This function constructs a sparse.matrix object.
}
