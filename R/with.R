#' @name with
#' @title With utilities
#' @description
#' Simple _with_ functions. Similar to or from \pkg{withr} implementations.
#' @param code R code to execute with temporary settings
#' @examples
#' # options ###################################
#' gwith_options(list(gutils.temp = "found"), {
#'     print(getOption("gutils.temp", default = "not_found"))
#' })
#' getOption("gutils.temp", default = "not_found")
#'
#' # packages ##################################
#' # temporarily attach at end of search (right before "base")
#' gwith_package("data.table", print(search()), pos = length(search()))
#' search()
#'
#' # seed  #####################################
#' start_seed <- .Random.seed
#'
#' # identical generation
#' a <- gwith_seed(runif(10), seed = 1234)
#' b <- gwith_seed(runif(10), seed = 1234)
#' identical(a, b)
#'
#' # does not alter pre-existing seed
#' end_seed <- .Random.seed
#' identical(start_seed, end_seed)
NULL

#' @describeIn with Eval with temporary option setting
#' @param new new option to set and its value
#' @export
gwith_options <- function(new, code) {
    old <- set_options(new_options = new)
    on.exit(reset_options(old))
    force(code)
}

#' @describeIn with Eval with temporarily attached package
#' @param package character. The name of a package
#' @param pos integer. position in search to attach at. Default = 2
#' @export
gwith_package <- function(package, code, pos = 2L) {
    pname <- sprintf("package:%s", package)
    is_loaded <- pname %in% search()
    if (!is_loaded) {
        suppressPackageStartupMessages(attachNamespace(package, pos = pos))
        on.exit(detach(pname, character.only = TRUE), add = TRUE)
    }
    force(code)
}

#' @describeIn with Eval with temporary specifiable seed
#' @inheritParams R.utils::withSeed
#' @param seed numeric. seed to set
#' @param \dots additional params to pass. See details.
#' @details
#' `gwith_seed()` : `...` passes to `set.seed()`
#' @export
gwith_seed <- function(
        seed = 1234,
        code,
        ...,
        substitute = TRUE,
        envir = parent.frame()) {
    if (substitute) {
        code <- substitute(code)
    }

    R.utils::withSeed(
        expr = code,
        seed = as.integer(seed),
        ...,
        substitute = FALSE, # already done
        envir = envir
    )
}

# internals ####

set_options <- function(new_options) {
    do.call(options, as.list(new_options))
}

reset_options <- function(old_options) {
    options(old_options)
}
