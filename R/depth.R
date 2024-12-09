# list nesting depth ####

#' @title Find depth of subnesting
#' @name depth
#' @param this object to evaluate
#' @param method max (default) or min nesting to detect
#' @param sig signature or class to check for. Default is 'data.frame'
#' @description Recursively determines how many max or min layers of subnesting
#' there is, with the end object (defined by param sig or a list of length 0)
#' being layer 0
#' @details https://stackoverflow.com/questions/13432863/
#' @keywords internal
#' @returns numeric
#' @examples
#' x <- list(a = 1)
#' depth(x)
#'
#' @export
depth <- function(this,
    method = c("max", "min"),
    sig = "data.frame") {
    method <- match.arg(arg = method, choices = c("max", "min"))

    # Stop conditions:

    # Stop if matches signature to search for
    if (inherits(this, sig)) {
        return(0L)
    }
    # Stop if an empty list is discovered
    if (inherits(this, "list") && length(this) == 0L) {
        return(0L)
    }
    # Stop if object is not a list AND recurse if it is.
    # Report minimum or maximum depth depending on method
    if (method == "max") {
        ifelse(
            inherits(this, "list"),
            1L + max(vapply(this,
                function(x) depth(x, method = method, sig = sig),
                FUN.VALUE = integer(1L)
            )),
            0L
        )
    } else if (method == "min") {
        ifelse(
            inherits(this, "list"),
            1L + min(vapply(this,
                function(x) depth(x, method = method, sig = sig),
                FUN.VALUE = integer(1L)
            )),
            0L
        )
    }
}


# param order should not be changed
#' @name require_depth
#' @title Coerce to AT LEAST specified nesting depth
#' @param x object to evaluate
#' @param dnames character. vector of names to apply per depth level if
#' not already named the name entries should match the depth they are
#' intended for. NULL values
#' @param min_depth required minimum nesting depth
#' are ignored.
#' @param count do not use
#' @returns list
#' @import checkmate
#' @examples
#' x <- list(a = 1)
#' require_depth(x, min_depth = 2L)
#'
#' @export
require_depth <- function(x, dnames = NULL, min_depth = 1L, count = 1L) {
    x_depth <- depth(x)

    # if not null, dnames must be a character vector with length that covers
    # the min_depth
    if (!is.null(dnames)) {
        checkmate::assert_character(dnames,
            len = min_depth + count - x_depth - 1L
        )
    }

    if (x_depth < min_depth) {
        depth_name <- dnames[[count]]
        x <- list(x)
        names(x) <- depth_name
        x <- require_depth(x, dnames, min_depth, count = count + 1L)
    }
    x
}
