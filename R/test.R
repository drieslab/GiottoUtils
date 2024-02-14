# ------ #
# Checks #
# ------ #

# Check something is TRUE. If FALSE then a (preferably informative)
# string is returned OR simply the value FALSE.
# If TRUE then the value TRUE will be returned.
# Use these when a non-disruptive check is desired.

# General check-type functions should be placed here. Check-type functions that
# are specific to a particular purpose should be next to the functions or
# pipelines in which they are used.


#' @title Test if list element exists
#' @name list_element_exists
#' @description Test if nth element of list exists
#' @param x list
#' @param index element index
#' @keywords internal
#' @return boolean
#' @export
list_element_exists <- function(x, index) {
    tryCatch(
        {
            if (length(x[[index]]) > -1) {
                return(TRUE)
            }
        },
        error = function(e) {
            return(FALSE)
        }
    )
}


# Test if character is missing, NULL or an empty string.
# Returns TRUE for all cases
#' @name is_empty_char
#' @title Test if missing or empty character
#' @description Convenient function to test if a character input is NULL, missing,
#' or has a length of 0 (empty)
#' @keywords internal
#' @export
is_empty_char <- function(x) {
    if (is.null(x)) {
        return(TRUE)
    }
    if (is.character(x) && length(x) == 0L) {
        return(TRUE)
    }
    if (any(sapply(x, is.na))) {
        return(sapply(x, is.na))
    }
    if (any(sapply(x, function(x) {
        x == ""
    }))) {
        return(sapply(x, function(x) {
            x == ""
        }))
    }

    FALSE
}
