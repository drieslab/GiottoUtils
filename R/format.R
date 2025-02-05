#' @include symbols.R
NULL

# Print ####

# nocov start
# simple wrapper around wrap_txt
#' @title Wrap message
#' @name wrap_msg
#' @param ... additional strings and/or elements to pass to wrap_txt
#' @param sep how to join elements of string (default is one space)
#' @returns character
#' @examples
#' wrap_msg("A message")
#'
#' @export
wrap_msg <- function(..., sep = " ") {
    message(wrap_txt(..., sep = sep))
}
# nocov end

#' @title Wrap text
#' @name wrap_txt
#' @param ... additional params to pass
#' @param sep how to join elements of string (default is one space)
#' @param strWidth externally set wrapping width. (default value of 100
#' is not effected)
#' @param errWidth default = FALSE. Set strWidth to be compatible
#' with error printout
#' @param .initial character. prefix for first line
#' @param .prefix character. prefix for later lines
#' @import utils
#' @returns character
#' @examples
#' cat(wrap_txt("A text"))
#' cat(wrap_txt(
#'     "Newlines are obeyed.
#'     The first line is not indented by default.
#'     later lines are indented by default.
#'
#'     The text is also wrapped to either a default max width of 100 char
#'     or the width of the console, whichever is smaller.
#'
#'     More than one item passed will be concatenated in the same way
#'     that cat() does."
#' ))
#'
#' @export
wrap_txt <- function(...,
    sep = " ",
    strWidth = 100,
    errWidth = FALSE,
    .prefix = " ",
    .initial = "") {
    custom_width <- ifelse(is.null(match.call()$strWidth),
        yes = FALSE, no = TRUE
    )
    if (!isTRUE(custom_width)) {
        if (isTRUE(errWidth)) strWidth <- getOption("width") - 6
    }

    cat(..., sep = sep) %>%
        capture.output() %>%
        strwrap(
            prefix = .prefix, initial = .initial, # indent later lines,
            # no indent first line
            width = min(80, getOption("width"), strWidth)
        ) %>%
        paste(collapse = "\n")
}

#' @rdname wrap_txt
#' @examples
#' cat(wrap_txtf(
#'     "This function works the same way as %s, but instead
#'     of concatenating all elements in the way that %s usually
#'     does, it uses %s formatting.",
#'     "wrap_txt()", "cat()", "sprintf()"
#' ))
#'
#' @export
wrap_txtf <- function(...,
    sep = " ",
    strWidth = 100,
    errWidth = FALSE,
    .prefix = " ",
    .initial = "") {
    custom_width <- ifelse(is.null(match.call()$strWidth),
        yes = FALSE, no = TRUE
    )
    if (!isTRUE(custom_width)) {
        if (isTRUE(errWidth)) strWidth <- getOption("width") - 6
    }

    cat(sprintf(...), sep = sep) %>%
        capture.output() %>%
        strwrap(
            prefix = .prefix, initial = .initial, # indent later lines,
            # no indent first line
            width = min(80, getOption("width"), strWidth)
        ) %>%
        paste(collapse = "\n")
}


#' @title Verbose message handler
#' @name vmsg
#' @param ... additional strings and/or elements to pass to wrap_msg
#' @param .v verbose flag to pass. Will check options through .vopt
#' if NULL (default). This param is intended for passing function-level
#' verbose flags
#' @param .is_debug flag as a debug print
#' (only prints when .v or .vopt is 'debug')
#' @param .vopt global verbosity option to pull from
#' @returns character
#' @examples
#' # common usage (.v is logical)
#' vmsg("print me", .v = TRUE)
#' vmsg("dont print me", .v = FALSE)
#'
#' # debug messages (.v == "debug")
#' # flag as a debug message using .is_debug = TRUE
#' vmsg("I am a debug message", .is_debug = TRUE, .v = TRUE) # no print
#' vmsg("I am a debug message", .is_debug = TRUE, .v = "debug") # prints
#'
#' vmsg("print me", .v = "debug") # also prints non-debug messages
#'
#' # with global option
#' options("giotto.verbose" = TRUE)
#' vmsg("Print by default")
#'
#' options("giotto.verbose" = FALSE)
#' vmsg("Do not print by default")
#' vmsg("Do not print by default", .v = TRUE)
#' # function level input overrides global option
#' @export
vmsg <- function(
        ..., .v = NULL, .is_debug = FALSE,
        .vopt = getOption("giotto.verbose", TRUE)) {
    # if function-level flag is provided, override global option
    if (!is.null(.v)) {
        .vopt <- .v
    }

    if (isTRUE(.vopt)) .vopt <- "yes"
    if (isFALSE(.vopt)) .vopt <- "no"

    .vopt <- tolower(.vopt)
    vflags <- c("yes", "no", "debug", "log", "debug_log")
    if (!.vopt %in% vflags) {
        .vopt <- "no" # default behavior with no match is to be nonverbose
    }

    .vopt <- match.arg(
        arg = .vopt,
        choices = c(
            "yes",
            "no",
            "debug",
            "log",
            "debug_log"
        )
    )

    # if debug type print, ignore if .vopt is not related to debug
    if (isTRUE(.is_debug)) {
        if (!(.vopt == "debug" || .vopt == "debug_log")) .vopt <- "no"
    }

    # debug overrides
    if (.vopt == "debug") .vopt <- "yes"
    if (.vopt == "debug_log") .vopt <- "log"

    switch(.vopt,
        "yes" = wrap_msg(...),
        "no" = return(invisible(NULL)),
        "log" = log_write(x = wrap_txt(...))
    )
}






#' @name gstop
#' @title Module-specific error message
#' @description
#' Send an error message formatted with `wrap_txt()`. Prepends the Giotto
#' module from which the error was triggered. This function should not be used
#' directly, but instead an internal `.gstop()` function should be created using
#' this framework for each module using it. The `.n` param should be incremented
#' to 2L for this wrapper function
#' @param ... additional strings and/or elements to pass to wrap_msg
#' @param sep how to join elements of string (default is one space)
#' @param strWidth externally set wrapping width.
#' (default value of 100 is not effected)
#' @param errWidth default = FALSE. Set strWidth to be
#' compatible with error printout
#' @param .module character. Giotto module to send the error from
#' @param .initial character. prefix for first line
#' @param .prefix character. prefix for later lines
#' @param .n stack frames back where the error happened
#' @param .call logical, whether to include the call selected through .n as the
#' location where the error was. Default is TRUE
#' @param .warn_nstack logical. whether to warn when there are insufficient
#' stackframes for requested .n (default = FALSE)
#' @returns character message
#' @examples
#' try(
#'     gstop("My stop message", .module = "GiottoUtils"),
#'     silent = TRUE
#' )
#' @export
gstop <- function(
        ...,
        sep = " ",
        strWidth = 100,
        errWidth = FALSE,
        .module,
        .prefix = " ",
        .initial = "",
        .n = 1L,
        .call = TRUE,
        .warn_nstack = getOption("giotto.warn_gstop_nstack", FALSE)) {
    nf <- sys.nframe()
    if (.n > nf) {
        # send message and automatically limit to max nframes
        if (.warn_nstack) {
            warning("[gstop] .n of ", .n,
                " is greater than number of stackframes ", nf,
                call. = FALSE
            )
        }
        .n <- nf
    }

    # determine specific call that triggered this stop
    if (nf %in% c(1L, 2L)) { # call from gstop or .gstop has no specific call
        sc <- NULL
    } else {
        .n <- as.integer(.n)
        sc <- get_prev_call(toplevel = .n + 1) # + 1 because of else statement
    }

    # format
    sc <- paste0(sc, ":")

    # if .call is not TRUE then set sc as NULL
    if (!isTRUE(.call) ||
        identical(sc, "NULL")) {
        sc <- NULL
    }

    emsg <- wrap_txt(
        paste(str_bracket(.module), sc, "\n"),
        ...,
        errWidth = TRUE
    )

    stop(emsg, call. = FALSE)
}


# Use this function internal to this package
.gstop <- function(...,
    sep = " ",
    strWidth = 100,
    errWidth = FALSE,
    .prefix = " ",
    .initial = "",
    .n = 1L,
    .call = TRUE) {
    gstop(...,
        sep = sep,
        strWidth = strWidth,
        errWidth = errWidth,
        .module = "GiottoUtils",
        .prefix = .prefix,
        .initial = .initial,
        .n = .n + 1L,
        .call = .call
    )
}







#' @title String convenience functions
#' @name str_convenience
#' @param x string item(s) to format
#' @returns character
#' @examples
#' x <- "test"
#' cat(str_bracket(x), "\n")
#' cat(str_parenth(x), "\n")
#' cat(str_double_quote(x), "\n")
#' cat(str_quote(x), "\n")
#'
#' vec <- c("item1", "item2", "item3")
#' cat(str_vector(vec), "\n")
#' cat(str_vector(vec, qchar = "double"))
NULL

#' @rdname str_convenience
#' @param qchar quote character to use. Either 'single' or "double"
#' @export
str_vector <- function(x, qchar = c("single", "double")) {
    qchar <- match.arg(qchar, choices = c("single", "double"))
    switch(qchar,
        "single" = return(toString(sprintf("'%s'", x))),
        "double" = return(toString(sprintf("\"%s\"", x)))
    )
}

#' @rdname str_convenience
#' @export
str_bracket <- function(x) {
    paste0("[", x, "]")
}

#' @rdname str_convenience
#' @export
str_parenth <- function(x) {
    paste0("(", x, ")")
}

#' @rdname str_convenience
#' @export
str_double_quote <- function(x) {
    paste0("\"", x, "\"")
}

#' @rdname str_convenience
#' @export
str_quote <- function(x) {
    paste0("\'", x, "\'")
}



#' @name print_list
#' @title Pretty print formatting for lists and vectors
#' @param x list of items to print. All entries must be named and have
#' `as.character()` methods
#' @param pre character. Optional characters to place at the head of each line
#' @returns lists and vectors
#' @examples
#' print_list(list())
#' print_list(c())
#'
#' testvec <- seq(3)
#' names(testvec) <- LETTERS[seq(3)]
#' print_list(testvec)
#'
#' test <- list(
#'     name1 = "1",
#'     longername2 = "test_char",
#'     thirdname = factor("this will be converted with as.character()")
#' )
#' print_list(test)
#' print_list(test, pre = "* ")
#' @export
print_list <- function(x, pre = "") {
    if (length(x) == 0) {
        cat("<empty>\n")
    }
    ns <- names(x)
    if (length(ns) != length(x)) {
        stop("all elements must be named")
    }
    x <- lapply(x, as.character)
    cat(sprintf("%s%s : %s", pre, format(ns), x), sep = "\n")
    invisible(x)
}





# Color text (8 colors) ####

# nocov start

#' @title Colorize print text
#' @name color_tag
#' @description
#' Return a list of 8 formatting tags for ansi colored text
#' @details supported colors checking is modified from \pkg{cli}
#' \href{https://github.com/r-lib/cli/blob/HEAD/R/num-ansi-colors.R}{aab-num-ansi-colors.R}
#' @keywords internal
#' @returns named list of characters
#' @examples
#' color_tag()
#'
#' @export
color_tag <- function() {
    list(
        r = "\u001b[31m", # red
        g = "\u001b[32m", # green
        y = "\u001b[33m", # yellow
        b = "\u001b[34m", # blue
        p = "\u001b[35m", # purple
        t = "\u001b[36m", # teal
        x = "\u001b[39m" # none (return)
    )
}

#' @rdname color_tag
#' @param x text to color
#' @examples
#' color_red("My text")
#'
#' @export
color_red <- function(x) {
    ct <- color_tag()
    if (use_color_text()) {
        paste0(ct$r, x, ct$x)
    } else {
        x
    }
}

#' @rdname color_tag
#' @param x text to color
#' @examples
#' color_green("My text")
#'
#' @export
color_green <- function(x) {
    ct <- color_tag()
    if (use_color_text()) {
        paste0(ct$g, x, ct$x)
    } else {
        x
    }
}

#' @rdname color_tag
#' @param x text to color
#' @examples
#' color_yellow("My text")
#'
#' @export
color_yellow <- function(x) {
    ct <- color_tag()
    if (use_color_text()) {
        paste0(ct$y, x, ct$x)
    } else {
        x
    }
}

#' @rdname color_tag
#' @param x text to color
#' @examples
#' color_blue("My text")
#'
#' @export
color_blue <- function(x) {
    ct <- color_tag()
    if (use_color_text()) {
        paste0(ct$b, x, ct$x)
    } else {
        x
    }
}

#' @rdname color_tag
#' @param x text to color
#' @examples
#' color_purple("My text")
#'
#' @export
color_purple <- function(x) {
    ct <- color_tag()
    if (use_color_text()) {
        paste0(ct$p, x, ct$x)
    } else {
        x
    }
}

#' @rdname color_tag
#' @param x text to color
#' @examples
#' color_teal("My text")
#'
#' @export
color_teal <- function(x) {
    ct <- color_tag()
    if (use_color_text()) {
        paste0(ct$t, x, ct$x)
    } else {
        x
    }
}

# nocov end


#' @describeIn color_tag Determine if system should print color
#' @keywords internal
#' @examples
#' use_color_text()
#'
#' @export
use_color_text <- function() {
    opt <- getOption("giotto.color_show", default = NULL)
    ansi8_color <- ansi_colors() >= 8L
    if (!is.null(opt)) {
        if (!isTRUE(opt)) {
            return(opt)
        }
        if (isTRUE(opt) && isTRUE(ansi8_color)) {
            return(opt)
        }
        if (isTRUE(opt) && !isTRUE(ansi8_color)) {
            wrap_msg('Color text not supported on this system.
               Set options("giotto.color_show" = FALSE)')
        }
    } else {
        ansi8_color
    }
}



#' @describeIn color_tag Determine if system can print at least 8 colors
#' @keywords internal
#' @examples
#' ansi_colors()
#'
#' @export
ansi_colors <- function() {
    # options
    opt <- getOption("cli.num_colors", default = NULL)
    if (!is.null(opt)) {
        return(as.integer(opt))
    }
    opt <- getOption("giotto.num_colors", default = NULL)
    if (!is.null(opt)) {
        return(as.integer(opt))
    }

    if ((env <- Sys.getenv("R_CLI_NUM_COLORS", "")) != "") {
        return(as.integer(env))
    }

    # crayon compatibility (allow color disabling through crayon)
    cray_opt_has <- getOption("crayon.enabled", NULL)
    cray_opt_num <- getOption("crayon.colors", NULL)
    if (!is.null(cray_opt_has) && !isTRUE(cray_opt_has)) {
        return(1L)
    } # disable
    if (isTRUE(cray_opt_has) && !is.null(cray_opt_num)) {
        return(as.integer(cray_opt_num))
    }
    if (isTRUE(cray_opt_has) && is.null(cray_opt_num)) {
        return(8L)
    }

    # 'NO_COLOR env setting disabling
    if (!is.na(Sys.getenv("NO_COLOR", NA_character_))) {
        return(1L)
    }

    # if knitr then no color in .Rmd chunks
    if (isTRUE(getOption("knitr.in.progress"))) {
        return(1L)
    }

    if (.Platform$GUI == "AQUA") {
        return(1L)
    }

    # No specific usage cases needed
    # Colors only used in show functions
    if (identical(Sys.getenv("RSTUDIO"), "1")) {
        return(8L)
    } # at least

    # Must be placed after 'RSTUDIO' check
    if (.Platform$GUI == "Rgui") {
        return(1L)
    }

    # Windows Emacs
    if (.Platform$OS.type == "windows" &&
        "--ess" %in% commandArgs() &&
        is_emacs_with_color()) {
        return(8L)
    }

    # end catch
    return(8L)
}



#' @describeIn color_tag Determine if emacs can print color
#' @keywords internal
#' @examples
#' is_emacs_with_color()
#'
#' @export
is_emacs_with_color <- function() {
    (Sys.getenv("EMACS") != "" || Sys.getenv("INSIDE_EMACS") !=
        "") && !is.na(emacs_version()[1]) && emacs_version()[1] >=
        23
}



#' @describeIn color_tag Determine emacs version
#' @keywords internal
#' @examples
#' emacs_version()
#'
#' @export
emacs_version <- function() {
    ver <- Sys.getenv("INSIDE_EMACS")
    ver <- gsub("[^0-9\\.]+", "", ver)
    if (ver == "") {
        return(NA_integer_)
    }
    ver <- strsplit(ver, ".", fixed = TRUE)[[1]]
    as.numeric(ver)
}













# radians and degrees ####
#' @title Radian/degree conversions
#' @name degrees
#' @description Convert radians to degrees and vice versa
#' @param deg degrees
#' @param rad radians
NULL

#' @describeIn degrees Degrees to radians
#' @examples
#' radians(180)
#' @export
radians <- function(deg) {
    deg * pi / 180
}

#' @describeIn degrees Radians to degrees
#' @return numeric
#' @examples
#' degrees(pi)
#' @export
degrees <- function(rad) {
    rad * 180 / pi
}






# Time ####
#' @title Format time for printing
#' @name time_format
#' @param secs numeric. seconds
#' @details Code from \code{\link[data.table]{timetaken}}
#' @returns character
#' @examples
#' time_format(90)
#'
#' @export
time_format <- function(secs) {
    if (secs > 60) {
        secs <- as.integer(secs)
        sprintf(
            "%02d:%02d:%02d", secs %/% 3600L, (secs %/% 60L) %% 60L,
            secs %% 60L
        )
    } else {
        sprintf(if (secs >= 10) {
            "%.1fs"
        } else {
            "%.3fs"
        }, secs)
    }
}
