#' @name with
#' @title With utilities
#' @description
#' Simple _with_ functions. Similar to or from \pkg{withr} implementations.
#' @param code code to execute with temporary settings
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

# internals ####

set_options <- function(new_options) {
    do.call(options, as.list(new_options))
}

reset_options <- function(old_options) {
    options(old_options)
}
