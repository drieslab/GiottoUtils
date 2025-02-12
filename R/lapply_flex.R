#' @title lapply_flex
#' @name lapply_flex
#' @param X list to use
#' @param FUN function to be performed
#' @param method character. Either `"future"` or `"biocparallel"`, determining
#' which parallelization framework to use.
#' @param future.seed whether to set a seed when using `method = "future"`
#' @param BPPARAM a BiocParallel parameter class deciding how to perform
#' parallelized or sequential (default) evaluation. Used with
#' `method = "biocparallel"`
#' @param fun deprecated. Backwards compatibility for FUN
#' @param cores deprecated
#' @param ... other arguments to pass
#' @keywords internal
#' @returns list
#' @examples
#' lapply_flex(list(x = 1, y = 2), FUN = log)
#'
#' # suppress warnings
#' options("giotto.warn_sequential" = FALSE)
#'
#' lapply_flex(list(x = 1, y = 2), FUN = log, method = "future")
#'
#' lapply_flex(list(x = 1, y = 2), FUN = log, method = "biocparallel")
#' @export
lapply_flex <- function(
        X,
        FUN,
        method = c("future", "biocparallel"),
        cores = NA,
        future.seed = TRUE,
        BPPARAM = NULL,
        fun = NULL,
        ...) {
    # backwards compatible with previous version
    if (!is.null(fun)) {
        FUN <- fun
    }

    method <- match.arg(tolower(method), c("future", "biocparallel"))

    # check if parallel, warn if not
    save_list <- switch(method,
        "future" = {
            package_check("future", repository = "CRAN")
            .check_future_parallel_plan()
            future.apply::future_lapply(
                X = X, FUN = FUN,
                future.seed = future.seed, ...
            )
        },
        "biocparallel" = {
            package_check("BiocParallel", repository = "Bioc")
            if (!is.null(BPPARAM)) {
                bpparam <- BPPARAM
            } else {
                bpparam <- giotto_bpparam()
            }
            BiocParallel::bplapply(
                X = X, FUN = FUN, BPPARAM = bpparam, ...
            )
        }
    )

    return(save_list)

    # set number of cores automatically, but with limit of 10
    # cores <- determine_cores(cores)

    # get type of os
    # os <- .Platform$OS.type
    # if(os == 'unix') {
    #  save_list = parallel::mclapply(X = X, mc.cores = cores,
    #                                 FUN = fun, ...)
    # } else if(os == 'windows') {
    #  save_list = parallel::mclapply(X = X, mc.cores = 1,
    #                                 FUN = fun, ...)
    # }
}

#' @title Giotto Default BiocParallel Param
#' @name giotto_bpparam
#' @description
#' Get and set a cached BiocParallel BPPARAM setting. Cached settings can
#' either be retrieved using `giotto_bpparam()` or directly with
#' `getOption("giotto.bpparam")`
#' @param BPPARAM set a BiocParallel parameter class deciding how to perform
#' parallelized or (or sequential) evaluation
#' @returns If a `BiocParallelParam` is passed to `BPPARAM`, it will be cached
#' and invisibly returned. If `BPPARAM` is `NULL` then the cached param will be
#' invisibly returned (`SerialParam`) is default.
#' @export
giotto_bpparam <- function(BPPARAM = NULL) {
    package_check("BiocParallel", repository = "Bioc")
    if (is.null(BPPARAM)) {
        # no input: use default
        BPPARAM <- getOption("giotto.bpparam", BiocParallel::SerialParam())
    } else {
        # input supplied: set new default + use
        checkmate::assert_class(BPPARAM, "BiocParallelParam")
        options("giotto.bpparam" = BPPARAM) # cache param
    }
    .check_bpparam(BPPARAM)
    return(invisible(BPPARAM))
}


# internals ####

.check_future_parallel_plan <- function() {
    if (!getOption("giotto.warn_sequential", TRUE)) {
        return(invisible())
    }
    if (inherits(future::plan(), "uniprocess")) {
        "Your code is running sequentially. For better performance, consider using a parallel plan like:
        future::plan(future::multisession)
        \nTo silence this warning, set options(\"giotto.warn_sequential\" = FALSE)" |>
            wrap_txt() |>
            warning(call. = FALSE)
    }
}


.check_bpparam <- function(bpparam) {
    if (!getOption("giotto.warn_sequential", TRUE)) {
        return(invisible())
    }
    if (inherits(bpparam, "SerialParam")) {
        "Your code is running sequentially. For better performance, consider using a parallel plan like:
        GiottoUtils::giotto_bpparam(BiocParallel::SnowParam())
        \nTo silence this warning, set options(\"giotto.warn_sequential\" = FALSE)" |>
            wrap_txt() |>
            warning(call. = FALSE)
    }
}
