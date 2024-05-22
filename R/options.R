

#' @name init_option
#' @title Initialize an option
#' @description Set an option only if it does not exist yet.
#' @param x character. Option to initialize
#' @param value value to set
#' @examples
#' init_option("dummy_option", 2)
#' init_option("dummy_option", 5)
#' getOption("dummy_option") # still 2
#' @returns NULL invisibly
#' @export
init_option <- function(x, value) {
    if (x %in% names(options())) {
        return(invisible())
    }

    setting <- list(value)
    names(setting) <- x

    options(setting)

    return(invisible())
}



