# lifecycle deprecation functions
# nocov start

#' @importFrom lifecycle deprecated
#' @family lifecycle
#' @returns argument marked as deprecated
#' @export
lifecycle::deprecated

#' @importFrom lifecycle is_present
#' @returns boolean
#' @export
lifecycle::is_present

#' @importFrom lifecycle deprecate_soft
#' @returns NULL, invisibly.
#' @export
lifecycle::deprecate_soft

#' @importFrom lifecycle deprecate_warn
#' @returns NULL, invisibly.
#' @export
lifecycle::deprecate_warn

#' @importFrom lifecycle deprecate_stop
#' @returns NULL, invisibly.
#' @export
lifecycle::deprecate_stop

# nocov end

#' @name lifecycle_badge
#' @title lifecycle badge
#' @description
#' Generate markdown needed for lifecycle badges that help communicate stability
#' of functions in documentation when added in backticks as shown below.
#' ```
#'  #' `r lifecycle::badge("experimental")`
#'  #' `r lifecycle::badge("deprecated")`
#'  #' `r lifecycle::badge("superseded")`
#' ```
#' @param stage character. 'stable', 'experimental', 'deprecated', or 'superseded'
#' @importFrom lifecycle badge
#' @family lifecycle
#' @returns markdown
#' @export
lifecycle_badge <- function(stage = "stable") {
    lifecycle::badge(stage)
}


#' @name deprecate_param
#' @title Deprecate a parameter
#' @description
#' Accepts the directly passed deprecated and superceding params and
#' Sends a deprecation message if the deprecated param was used.
#' This function wraps `lifecycle::deprecate_warn()` to make a
#' standard param deprecation message. It then outputs the final value
#' to be used.
#' @param x name. Deprecated param
#' @param y name. Superceding param
#' @param fun character. Name of function
#' @param when character. Version number in which the deprecation happened.
#' @param check character. Method to check if deprecated param was used
#' @param always If FALSE, the default, will warn every 8 hours. If TRUE, will
#'  always warn in direct usages. Indirect usages keep warning every 8 hours to
#'  avoid disrupting users who can't fix the issue. Only use always = TRUE
#'  after at least one release with the default.
#' @returns final value to be used
#' @examples
#' foo <- function(dep = deprecated(), sup = 10) {
#'     sup <- deprecate_param(
#'         dep, sup,
#'         fun = "foo", when = "0.0.1"
#'     )
#'     return(sup)
#' }
#'
#' foo() # following defaults, no deprecation message
#' foo(sup = 3) # no deprecation message triggered
#' foo(dep = 3) # deprecation message triggered
#'
#' # convenient nested function when deprecating multiple params
#' bar <- function(dep1 = deprecated(),
#'     dep2 = deprecated(),
#'     sup1 = 10,
#'     sup2 = 20) {
#'     # internally defined function that streamlines downstream deprecations
#'     .dep <- function(...) {
#'         deprecate_param(..., fun = "bar", when = "0.0.2")
#'     }
#'
#'     sup1 <- .dep(dep1, sup1)
#'     sup2 <- .dep(dep2, sup2)
#'
#'     return(list(sup1, sup2))
#' }
#'
#' bar(sup1 = 100)
#' bar(dep1 = 100, dep2 = "hello")
#'
#' @export
deprecate_param <- function(x, y, fun, when, check = c("deprecated", "null"), always = FALSE) {
    # checkmate::assert_character(from)
    check <- match.arg(check, choices = c("deprecated", "null"))
    xchar <- as.character(substitute(x))

    used <- switch(check,
        "null" = !is.null(x),
        "deprecated" = is_present(x)
    )

    if (!used) {
        return(y)
    }

    ychar <- as.character(substitute(y))
    deprecate_warn(
        when = when,
        what = sprintf("%s(%s)", fun, xchar),
        with = sprintf("%s(%s)", fun, ychar),
        env = parent.frame(2),
        user_env = parent.frame(3),
        always = always
    )
    return(x)
}
