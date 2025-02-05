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
    if (length(pattern) != length(string)) {
        pattern <- rep(
            pattern,
            length(string)
        )
    }

    out <- lapply(seq_along(string), function(i) {
        res <- regexpr(pattern = pattern[[i]], text = string[[i]])
        .start <- res[1]
        .end <- attr(res, "match.length") - 1L + .start
        matrix(c(.start, .end), ncol = 2, dimnames = list(
            NULL,
            c("start", "end")
        ))
    })

    out <- Reduce(rbind, out)

    out[out < 0L] <- NA_integer_

    return(out)
}

#' @name str_abbreviate
#' @title Abbreviate a string
#' @description Abbreviates a string in the format of `head`\[...\]`tail`
#' when it exceeds the length specified by `width`. Useful for shortening
#' how filepaths are displayed.
#' @param string character. Input string
#' @param width numeric. Strings longer than this many characters will be
#' abbreviated
#' @param head numeric. Number of characters to include before abbreviated
#' section
#' @param tail numeric. Number of characters to include after abbreviated
#' section
#' @returns character
#' @examples
#' a <- "/short/file/path/"
#' b <- "/much/longer/foooooooooooooooooooo/baaaaaaaaaaaaaaaaaaaar/file/path/"
#' str_abbreviate(c(a, b))
#' str_abbreviate(c(a, b), width = 10, head = 3, tail = 3)
#' @export
str_abbreviate <- function(string, width = 60L, head = 15L, tail = 35L) {
    head <- as.integer(head)
    tail <- as.integer(tail)
    min_width <- head + tail
    if (min_width > width) {
        "str_abbreviate: `head` + `tail` is greater than `width`.
        Using `width` =" |>
            wrap_txt(min_width) |>
            warning(call. = FALSE)
    }
    width <- as.integer(max(min_width, width))

    res <- vapply(string, function(str) {
        nch <- nchar(str)
        if (nch > width) {
            p1 <- substring(str, first = 0L, last = head)
            p2 <- substring(str, first = nch - tail, last = nch)
            sprintf("%s[...]%s", p1, p2)
        } else {
            str
        }
    }, FUN.VALUE = character(1L), USE.NAMES = FALSE)

    return(res)
}
