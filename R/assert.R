# ---------- #
# Assertions #
# ---------- #

# Assert something is true. If an assertion is false, then a (preferably 
# informative) error should be thrown. 
# These can help guide developers and users about the
# proper usage and limitations of your functions. No values are returned.
# Use these for guard clauses.

# global for g_assert
.name <- NULL

# Note that assertions built upon g_assert require that the object asserted 
# again
# MUST be supplied as param from the parent frame

#' @title Assertion framework
#' @name g_assert
#' @description Framework to perform assertions and provide informative errors
#' about both the input object and the test that was performed. Functions for
#' specific checks and messages can be built using this base function.
#' @param x object to test
#' @param test test to perform that should have a TRUE or FALSE outcome
#' @param msg error message provided as character vector. Using the token .name
#' will send the name of object being tested in the stack frame referenced by
#' \code{n}
#' @param n stack frames back in which to evaluate the param
#' @param ... additional params to pass
#' @returns A character message
#' @keywords internal
#' @examples
#' x <- data.frame(a = 1:3, b = 5:7)
#' g_assert(
#'     x,
#'     test = inherits(x, "data.table"),
#'     msg = c(.name, "must be of class data.table, not", class(x)),
#'     n = 0
#' )
#' @export
g_assert <- function(x, test, msg = NULL, n = 2L, ...) {
    if (!test) {
        # get name of function where test failed
        fn_name <- deparse(sys.calls()[[sys.nframe() - n]])
        # get name of object that failed test
        .name <- deparse(eval(call("substitute", as.name(substitute(x)), 
                                parent.frame(n = 1L))))
        .name <- paste0('\"\'', .name, '\'\"')

        # compose message
        msg <- gsub(pattern = "\\.name", replacement = .name, 
                    x = deparse(substitute(msg)))
        msg <- parse(text = msg)

        # send error
        stop(wrap_txt(fn_name, ":\n", eval(msg, envir = parent.frame(n = 1L)), 
                    errWidth = TRUE),
            call. = FALSE
        )
    }
}



#' @describeIn g_assert Test for whether supplied object is a \code{giotto} 
#' object
#' @param gobject giotto object
#' @keywords internal
#' @examples
#' x <- GiottoData::loadGiottoMini("visium")
#' assert_giotto(x, n = 0)
#' 
#' x <- data.frame(a = 1:3, b = 5:7) 
#' assert_giotto(x, n = 0)
#' 
#' @export
assert_giotto <- function(gobject, n = 1L, ...) {
    fn_name <- deparse(sys.calls()[[sys.nframe() - n]])
    orig_name <- deparse(eval(call("substitute", as.name(substitute(gobject)), 
                                parent.frame())))
    if (!methods::hasArg(gobject)) {
        stop(
            wrap_txt(fn_name, ":\ngiotto object must be given",
                errWidth = TRUE
            ),
            call. = FALSE
        )
    }
    if (!inherits(gobject, "giotto")) {
        stop(
            wrap_txt(fn_name, ":\n", orig_name, "is not a giotto object",
                errWidth = TRUE
            ),
            call. = FALSE
        )
    }
}



#' @describeIn g_assert Test whether input is a data.table object
#' @examples
#' x = data.table::data.table(x = 1:3, y = 1:3)
#' assert_dt(x, n = 0)
#' 
#' @export
assert_dt <- function(x, n = 2L) {
    g_assert(
        x,
        test = inherits(x, "data.table"),
        msg = c(.name, "must be of class data.table, not", class(x)),
        n = n
    )
}


# NOTE: this currently overrides the checkmate function of the same name
#' @describeIn g_assert Test whether input is an existing file
#' @examples
#' x <- "my_file.txt"
#' assert_file(x, n = 0)
#' 
#' @export
assert_file <- function(x, n = 2L) {
    g_assert(
        x,
        test = is.character(x),
        msg = c(.name, "must be a character vector filepath"),
        n = n
    )
    g_assert(
        x,
        test = file.exists(x),
        msg = c(.name, "is not an existing file"),
        n = n
    )
}

# NOTE: this currently overrides the checkmate function of the same name
#' @describeIn g_assert Test whether input is of class numeric
#' @examples
#' x <- 1
#' assert_numeric(x, n = 0)
#' 
#' @export
assert_numeric <- function(x, n = 2L) {
    g_assert(
        x,
        test = is.numeric(x),
        msg = c(.name, "must be of class numeric, not", class(x)),
        n = n
    )
}
