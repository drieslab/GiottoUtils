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
#' @examples
#' a <- list()
#' length(a) <- 4
#' list_element_exists(a, 5)
#' list_element_exists(a, 4)
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
#' @description Convenient function to test if a character input is NULL,
#' missing, or has a length of 0 (empty)
#' @param x character vector to test (may be NULL or contain NA values)
#' @keywords internal
#' @returns boolean
#' @examples
#' is_empty_char(NULL)
#' is_empty_char(rep("test", 5))
#' is_empty_char(c(character(4L), "non-empty")) # example with empty
#' is_empty_char(c(NA_character_, "non-na")) # example with NA
#' @export
is_empty_char <- function(x) {
    x %null% return(TRUE)
    if (is.character(x) && length(x) == 0L) {
        return(TRUE)
    }

    nas <- vapply(x, is.na, FUN.VALUE = logical(1L))
    if (any(nas)) {
        return(nas)
    }

    nones <- vapply(x, function(x) x == "", FUN.VALUE = logical(1L))
    if (any(nones)) {
        return(nones)
    }

    FALSE
}
