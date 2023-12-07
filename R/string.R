# string manipulation functions

#' @title Find location of match in string
#' @name str_locate2
#' @return integer matrix with two columns and one row for each element of
#' string. The first column, start, gives the position at the start of the
#' match, and the second column, end, gives the position of the end.
#' @description
#' Implementation of \pkg{stringr}'s `str_locate` with base R.
#' @param string Input vector. Either a character vector, or something coercible to one.
#' @param pattern Pattern to look for.
#' @export
str_locate2 <- function(string, pattern) {
  res <- regexpr(pattern = pattern, text = string)
  .start <- res[1]
  .end <- attr(res, "match.length") - 1L + .start
  array(data = c(.start, .end), dim = c(1,2), dimnames = list(NULL, c('start', 'end')))
}
