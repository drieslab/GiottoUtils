

#' @title Set if
#' @name set_if
#' @aliases `%null%`, `%none%`, `%na%`
#' @description
#' Set a default value if a test is true.
#' @param x object to test
#' @param y default value to set
#' @returns
#' if `x` tests `TRUE`, `y` is returned
#' if `x` tests `FALSE` `x` is returned
#' @examples
#' NULL %null% 1
#' 2 %null% 1
#'
#' "test" %na% 10
#' NA_character_ %na% "missing"
#'
#' c() %none% 20
#' list() %none% "a"
#' logical(5) %none% 10
#'
NULL


#' @rdname set_if
#' @export
`%null%` <- function(x, y) {
  ifelse(is.null(x), y, x)
}

#' @rdname set_if
#' @export
`%na%` <- function(x, y) {
  ifelse(is.na(x), y, x)
}

#' @rdname set_if
#' @export
`%none%` <- function(x, y) {
  ifelse(length(x) == 0L, y, x)
}
