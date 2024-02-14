# Python functionalities to make available across Giotto packages.
# Not restricted to giotto_env conda env, but will use whichever environment
# is currently active.
#
# Note, however that running any python function for the first time in the R
# session will likely lock it to a specific python env (either set on purpose or
# it picks a default)



#' @name to_scipy_sparse
#' @title Convert R sparse matrix to scipy sparse representation
#' @description
#' Convert an R \pkg{Matrix} sparse matrix representation to python scipy
#' sparse format. Inspired by the implementation in \pkg{nmslibR}. Currently
#' works only for `matrix`, `dgCMatrix`, `dgRMatrix`, and `dgTMatrix`
#' @param x matrix-like object to convert
#' @param format character. Either "C" for Compressed Sparse Column or "R"
#' for Compressed Sparse Row matrix representations
#' @param transpose whether to transpose the matrix. default is FALSE
#' @param \dots additional params to pass to `scipy.sparse.cs*_matrix()`
#' @importFrom methods as
#' @export
to_scipy_sparse <- function(x, format = c("C", "R"), transpose = FALSE, ...) {
    format <- match.arg(toupper(format), choices = c("C", "R"))

    if (inherits(x, "matrix")) {
        return(.to_scipy_sparse_matrix(x, format, transpose, ...))
    }
    if (inherits(x, "dgCMatrix")) {
        return(.to_scipy_sparse_dgc(x, format, transpose, ...))
    }
    if (inherits(x, "dgRMatrix")) {
        return(.to_scipy_sparse_dgr(x, format, transpose, ...))
    }
    if (inherits(x, "dgTMatrix")) {
        return(.to_scipy_sparse_dgt(x, format, transpose, ...))
    }

    .gstop("No to_scipy_sparse method found for class", class(x))
}


# internals ####

.to_scipy_sparse_matrix <- function(
        x, format, transpose = FALSE, ...) {
    SCP <- reticulate::import("scipy", convert = FALSE)
    if (transpose) x <- t(x)
    switch(format,
        "C" = SCP$sparse$csc_matrix(x, ...),
        "R" = SCP$sparse$csr_matrix(x, ...)
    )
}

.to_scipy_sparse_dgc <- function(
        x, format = c("C", "R"), transpose = FALSE, ...) {
    SCP <- reticulate::import("scipy", convert = FALSE)
    if (transpose) x <- Matrix::t(x)
    if (format == "R") {
        x2 <- as(x, "RsparseMatrix")
        return(to_scipy_sparse(x2, format = "R", transpose = FALSE, ...))
    }

    SCP$sparse$csc_matrix(
        arg1 = reticulate::tuple(x@x, x@i, x@p),
        shape = reticulate::tuple(x@Dim[1L], x@Dim[2L])
    )
}

.to_scipy_sparse_dgr <- function(
        x, format = c("C", "R"), transpose = FALSE, ...) {
    SCP <- reticulate::import("scipy", convert = FALSE)
    if (transpose) x <- Matrix::t(x)
    if (format == "C") {
        x2 <- as(x, "CsparseMatrix")
        return(to_scipy_sparse(x2, format = "C", transpose = FALSE, ...))
    }

    SCP$sparse$csr_matrix(
        arg1 = reticulate::tuple(x@x, x@j, x@p),
        shape = reticulate::tuple(x@Dim[1L], x@Dim[2L])
    )
}

.to_scipy_sparse_dgt <- function(
        x, format = c("C", "R"), transpose = FALSE, ...) {
    if (transpose) x <- Matrix::t(x)

    switch(format,
        "C" = x <- as(x, "CsparseMatrix"),
        "R" = x <- as(x, "RsparseMatrix")
    )

    to_scipy_sparse(x, format = format, transpose = FALSE, ...)
}
