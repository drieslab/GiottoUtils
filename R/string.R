# string manipulation functions

#' @title Find location of match in string
#' @name str_locate2
#' @return integer matrix with two columns and one row for each element of
#' string. The first column, start, gives the position at the start of the
#' match, and the second column, end, gives the position of the end.
#' @description
#' Implementation of \pkg{stringr}'s `str_locate` with base R.
#' @param string Input vector. Either a character vector, or something 
#' coercible to one.
#' @param pattern Pattern to look for.
#' @examples
#' fruit <- c("apple", "banana", "pear", "pineapple")
#' str_locate2(fruit, "$")
#' str_locate2(fruit, "a")
#' str_locate2(fruit, "e")
#' str_locate2(fruit, c("a", "b", "p", "p"))
#' @export
str_locate2 <- function(string, pattern) {
    if (length(pattern) != 1L && length(string) != length(pattern)) {
        .gstop(
    sprintf("Can't recycle `string` (size %d) to match `pattern` (size %d)"),
            length(string), length(pattern)
        )
    }

    # recycle
    if (length(pattern) != length(string)) pattern <- rep(pattern, 
                                                        length(string))

    out <- lapply(seq_along(string), function(i) {
        res <- regexpr(pattern = pattern[[i]], text = string[[i]])
        .start <- res[1]
        .end <- attr(res, "match.length") - 1L + .start
        matrix(c(.start, .end), ncol = 2, dimnames = list(NULL, 
                                                        c("start", "end")))
    })

    out <- Reduce(rbind, out)

    out[out < 0L] <- NA_integer_

    return(out)
}
