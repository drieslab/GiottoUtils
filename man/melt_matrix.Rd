% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reshape.R
\name{melt_matrix}
\alias{melt_matrix}
\title{Melt a matrix}
\usage{
melt_matrix(data, varnames = NULL, ..., as.is = FALSE, value.name = "value")
}
\arguments{
\item{data}{\code{matrix} to melt}

\item{varnames}{variable names to use in molten \code{data.table}}

\item{\dots}{further arguments passed to or from other methods}

\item{as.is}{if \code{FALSE}, dimnames will be converted to factor or integer etc
as appropriate. If \code{TRUE}, they will be left as strings.}

\item{value.name}{name of variable used to store values (default = "value")}
}
\value{
\code{data.table} in long format
}
\description{
Simple implementation of melt for matrices to produces results
similar to that of reshape2's now that it is deprecated. \cr
The param \code{na.rm} is not implemented.
}
\examples{
set.seed(9)
Data <- matrix(round(rnorm(12, 10, 4)), nrow = 4, ncol = 3)
melt_matrix(Data)
}
