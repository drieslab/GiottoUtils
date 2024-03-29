% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assert.R
\name{g_assert}
\alias{g_assert}
\alias{assert_giotto}
\alias{assert_dt}
\alias{assert_file}
\alias{assert_numeric}
\title{Assertion framework}
\usage{
g_assert(x, test, msg = NULL, n = 2L, ...)

assert_giotto(gobject, n = 1L, ...)

assert_dt(x)

assert_file(x)

assert_numeric(x)
}
\arguments{
\item{x}{object to test}

\item{test}{test to perform that should have a TRUE or FALSE outcome}

\item{msg}{error message provided as character vector. Using the token .name
will send the name of object being tested in the stack frame referenced by
\code{n}}

\item{n}{stack frames back in which to evaluate the param}

\item{...}{additional params to pass}

\item{gobject}{giotto object}
}
\description{
Framework to perform assertions and provide informative errors
about both the input object and the test that was performed. Functions for
specific checks and messages can be built using this base function.
}
\section{Functions}{
\itemize{
\item \code{assert_giotto()}: Test for whether supplied object is a \code{giotto} object

\item \code{assert_dt()}: Test whether input is a data.table object

\item \code{assert_file()}: Test whether input is an existing file

\item \code{assert_numeric()}: Test whether input is of class numeric

}}
\examples{
\dontrun{
g_assert(
    x,
    test = inherits(x, "data.table"),
    msg = c(.name, "must be of class data.table, not", class(x))
)
}
}
\keyword{internal}
