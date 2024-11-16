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
