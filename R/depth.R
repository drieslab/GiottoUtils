# list nesting depth ####

#' @title Find depth of subnesting
#' @name depth
#' @param this object to evaluate
#' @param method max (default) or min nesting to detect
#' @param sig signature or class to check for. Default is 'data.frame'
#' @description Recursively determines how many max or min layers of subnesting
#' there is, with the end object (defined by param sig or a list of length 0)
#' being layer 0
#' @details https://stackoverflow.com/questions/13432863/determine-level-of-nesting-in-r
#' @keywords internal
#' @export
depth <- function(this,
                  method = c('max', 'min'),
                  sig = 'data.frame') {

  method = match.arg(arg = method, choices = c('max', 'min'))

  # Stop conditions:

  # Stop if matches signature to search for
  if(inherits(this, sig)) {
    return(0L)
  }
  # Stop if an empty list is discovered
  if(inherits(this, 'list') && length(this) == 0L) {
    return(0L)
  }
  # Stop if object is not a list AND recurse if it is.
  # Report minimum or maximum depth depending on method
  if(method == 'max') {
    ifelse(inherits(this, 'list'), 1L + max(sapply(this, function(x) depth(x, method = method, sig = sig))), 0L)
  } else if(method == 'min') {
    ifelse(inherits(this, 'list'), 1L + min(sapply(this, function(x) depth(x, method = method, sig = sig))), 0L)
  }

}
