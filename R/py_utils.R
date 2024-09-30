# Python functionalities to make available across Giotto packages.
# Not restricted to giotto_env conda env, but will use whichever environment
# is currently active.
#
# Note, however that running any python function for the first time in the R
# session will likely lock it to a specific python env (either set on purpose or
# it picks a default)



#' @name scipy_sparse
#' @title Convert between Matrix and scipy sparse representations
#' @description
#' Convert between R \pkg{Matrix} sparse matrix representation and python scipy
#' sparse formats. Inspired by the implementation in \pkg{nmslibR}. Currently
#' compatible classes:\cr
#' * **`to_scipy_sparse()`**: `matrix`, `dgCMatrix`, `dgRMatrix`, `dgTMatrix`
#' * **`from_scipy_sparse()`**: `scipy.sparse._csr.csr_matrix`,
#' `scipy.sparse._csc.csc_matrix`
#' @param x matrix-like object to convert
#' @param format character. Either "C" for Compressed Sparse Column or "R"
#' for Compressed Sparse Row matrix representations
#' @param transpose whether to transpose the matrix. default is FALSE
#' @param \dots additional params to pass to `scipy.sparse.cs*_matrix()` or
#' `Matrix::sparseMatrix` depending on which output is desired.
#' @importFrom methods as
#' @returns scipy or Matrix sparse representation depending on 'to' or 'from'
#' respectively
#' @examples
#' # example data
#' m <- matrix(data = 0L, nrow = 400, ncol = 300)
#' m[sample(400, 100), sample(300, 100)] <- runif(100, max = 10)
#' dgc <- Matrix::rsparsematrix(nrow = 400L, ncol = 300L, density = 0.1)
#'
#' # some conversions
#' py_m_c <- to_scipy_sparse(m, format = "C")
#' py_m_r_t <- to_scipy_sparse(m, format = "R", transpose = TRUE)
#'
#' py_dgc_c <- to_scipy_sparse(dgc, format = "C")
#' py_dgc_r <- to_scipy_sparse(dgc, format = "R")
#'
#' dgc_revert <- from_scipy_sparse(py_dgc_c, format = "C")
#' dgr_revert <- from_scipy_sparse(py_dgc_c, format = "R")
#' identical(dgc, dgc_revert)
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

    stop("GiottoUtils: No to_scipy_sparse method found for class", class(x))
}

#' @rdname scipy_sparse
#' @export
from_scipy_sparse <- function(x, format = c("C", "R"), transpose = FALSE, ...) {
    format <- match.arg(toupper(format), choices = c("C", "R"))

    if (!inherits(x, "python.builtin.object")) {
        stop("sparse input must be in a python native format.\n")
    }
    if (inherits(x, "scipy.sparse._csr.csr_matrix")) {
        return(.from_scipy_sparse_csr(x, format, transpose, ...))
    }
    if (inherits(x, "scipy.sparse._csc.csc_matrix")) {
        return(.from_scipy_sparse_csc(x, format, transpose, ...))
    }

    stop("GiottoUtils: No from_scipy_sparse method found for class", class(x))
}


#' @title Active python environment
#' @name py_active_env
#' @description Check for an active python environment without initialization.
#' If none initialized, `FALSE` is returned. If an initialized environment
#' is found, the env name based on [reticulate::conda_list()] will be returned
#' @returns boolean
#' @export
py_active_env <- function() {
    # declare data.table variables to avoid code check NOTE
    name <- python <- NULL

    if (!reticulate::py_available()) {
        options("giotto.py_active_env" = FALSE)
        return(FALSE)
    }

    env_cache <- getOption("giotto.py_active_env", FALSE)
    if (is.character(env_cache)) {
        return(env_cache)
    }

    py_conf <- reticulate::py_config()
    py_path <- py_conf$python
    py_ver <- py_conf$version
    py_tab <- data.table::setDT(reticulate::conda_list())
    py_name <- py_tab[dirname(python) == dirname(py_path), name]

    # if no name found (this is not a miniconda), return path instead
    if (length(py_name) == 0) py_name <- py_path

    options("giotto.py_active_env" = py_name)
    options("giotto.py_active_ver" = py_ver)
    return(py_name)
}


# internals ####

.get_scipy <- function() {
    SCP <- getOption("giotto.scipy", NULL)
    SCP <- SCP %null% reticulate::import("scipy", convert = FALSE)
    options("giotto.scipy" = SCP)
    return(SCP)
}

## sparse matrices ####

.to_scipy_sparse_matrix <- function(x, format, transpose = FALSE, ...) {
    SCP <- .get_scipy()
    if (transpose) x <- t(x)
    switch(format,
        "C" = SCP$sparse$csc_matrix(x, ...),
        "R" = SCP$sparse$csr_matrix(x, ...)
    )
}

.to_scipy_sparse_dgc <- function(
        x, format = c("C", "R"),
        transpose = FALSE, ...) {
    SCP <- .get_scipy()
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
        x, format = c("C", "R"),
        transpose = FALSE, ...) {
    SCP <- .get_scipy()
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
        x, format = c("C", "R"),
        transpose = FALSE, ...) {
    if (transpose) x <- Matrix::t(x)

    switch(format,
        "C" = x <- as(x, "CsparseMatrix"),
        "R" = x <- as(x, "RsparseMatrix")
    )

    to_scipy_sparse(x, format = format, transpose = FALSE, ...)
}

.from_scipy_sparse_csr <- function(
        x, format = c("C", "R"),
        transpose = FALSE, ...) {
    if (transpose) {
        x <- x$transpose()
        # call again since transpose is accomplished via csr -> csc conversion
        return(from_scipy_sparse(x, format, transpose = FALSE, ...))
    }

    Matrix::sparseMatrix(
        x = as.numeric(x$data),
        j = as.numeric(x$indices),
        p = as.numeric(x$indptr),
        index1 = FALSE, # 0 indexed values
        dims = dim(x),
        repr = format,
        ...
    )
}



.from_scipy_sparse_csc <- function(
        x, format = c("C", "R"),
        transpose = FALSE, ...) {
    if (transpose) {
        x <- x$transpose()
        # call again since transpose is accomplished via csc -> csr conversion
        return(from_scipy_sparse(x, format, transpose = FALSE, ...))
    }

    Matrix::sparseMatrix(
        x = as.numeric(x$data),
        i = as.numeric(x$indices),
        p = as.numeric(x$indptr),
        index1 = FALSE, # 0 indexed values
        dims = dim(x),
        repr = format,
        ...
    )
}
