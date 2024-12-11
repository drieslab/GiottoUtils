#' @name handle_warnings
#' @title Warning handling
#' @description
#' Utility to gracefully handle expected warnings
#' @param expr expression to evaluate
#' @returns `list` of "result" which is the output of `expr` and "warnings"
#' which are any warnings that were generated during eval.
#' @examples
#' f <- function() {
#'     warning("this is a warning")
#'     return(TRUE)
#' }
#' handle_warnings(f())
#' @export
handle_warnings <- function(expr) {
    warnings <- character(0)

    # Create a warning handler that captures warnings
    result <- withCallingHandlers(
        expr,
        warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
        }
    )

    # Return both result and warnings
    list(
        result = result,
        warnings = warnings
    )
}

#' @name handle_errors
#' @title Error handling
#' @description
#' Some errors may lock up the console for much longer than they should. This
#' can often be because R has difficulty figuring out the callstack to generate
#' the error message. This function is a simple wrapper to set up a tryCatch
#' evaluation that will return all enclosed errors with the `stop()` arg 
#' `call. = FALSE` setting.
#' @param expr expression to evaluate 
#' @param prefix string. An optional prefix to add before the error to help
#' report where it was from.
#' @returns Throws an error without processing stop calls.
#' @examples
#' x <- function() {handle_errors(stop("this is an error"))}
#' if (FALSE) {
#'     x()
#' }
#' 
#' y <- function() {
#'     handle_errors(
#'         stop("this is an error"), 
#'         prefix = "error location:"
#'     )
#' }
#' #' if (FALSE) {
#'     y()
#' }
#' @export
handle_errors <- function(expr, prefix = "") {
    res <- tryCatch(
        expr,
        error = function(e) {
            stop(wrap_txtf("%s\n%s", prefix, e$message), call. = FALSE)
        }
    )
}


