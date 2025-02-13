#' @name prev_call
#' @title Previous calls
#' @description Functions to check the call stack and get aspects of previous
#' calls. Used in putting together object histories and error handling.
#' @param toplevel integer. Relative stack where the function call was made.
#' @param verbose be verbose
#' @examples
#' get_prev_call()
#'
#' get_prev_fname(1)
#' 
#' # preserve params
#' foo <- function(a, b) get_args(toplevel = 1)
#' foo(a = 1, b = 2)
#' 
#' bar <- function() get_args() # default toplevel = 2
#' baz <- function(x, y) bar()
#' baz("a", "b")
NULL


#' @describeIn prev_call Get previous call
#' @export
get_prev_call <- function(toplevel = 1L) {
    deparse(sys.call(-toplevel))
}


# Determine the name of the function n levels above the current evaluation
# frame, where n is toplevel - 1
#' @describeIn prev_call Get previous call function name
#' @returns character
#' @export
get_prev_fname <- function(toplevel = 3L) {
    as.character(sys.call(-toplevel)[[1]])
}



#' @describeIn prev_call Get previous call args as named character vector
#' @export
get_args <- function(toplevel = 2L, verbose = FALSE) {
    nframes <- sys.nframe()

    if (isTRUE(verbose)) {
        message("number of frames: ", nframes)
    }


    cl <- sys.call(-toplevel)

    if (isTRUE(verbose)) {
        message("system call: ", cl)
    }


    # function name
    fname <- as.character(cl[[1]])

    if (length(fname) > 1) {
        fname <- fname[[3]]
    }

    if (isTRUE(verbose)) {
        message("function name: ", fname)
    }


    # function
    # f = get(x = fname, mode = "function", pos = 'package:Giotto')
    f <- get(x = fname, mode = "function", pos = sys.frame(-2))

    # get used arguments
    cl <- match.call(definition = f, call = cl)
    user_args <- as.list(cl)[-1]

    # all fun arguments
    fun_args <- formals(fun = fname)
    fun_args[names(user_args)] <- user_args

    unl_args <- unlist(fun_args)
    final_args <- as.character(unl_args)
    names(final_args) <- names(unl_args)

    # select first from vector
    bool_det <- grepl("c\\(", final_args)
    if (any(bool_det) == TRUE) {
        for (bool_name in names(final_args[bool_det])) {
            bool_vec <- final_args[bool_name]
            new_vec <- strsplit(bool_vec, split = "\"")[[1]][2]

            final_args[bool_name] <- new_vec
        }
    }

    return(final_args)
}

#' @describeIn prev_call Get call args as named list
#' @param keep character. When NULL, all params are captured. If not NULL,
#' specifies which params to capture.
#' @param \dots additional params to capture
#' @examples
#' a <- function(x = 1, y = 2, ...) {
#'     get_args_list(...)
#' }
#'
#' a(z = 3, keep = "y")
#' @export
get_args_list <- function(toplevel = 1L, keep = NULL, ...) {
    a <- as.list(as.environment(parent.frame(toplevel)))

    if (!is.null(keep)) {
        a <- a[names(a) %in% keep]
    }

    c(a, list(...))
}
