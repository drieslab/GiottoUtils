#' @include symbols.R
NULL

# Print ####

# nocov start
# simple wrapper around wrap_txt
#' @title Wrap message
#' @name wrap_msg
#' @param ... additional strings and/or elements to pass to wrap_txt
#' @param sep how to join elements of string (default is one space)
#' @export
wrap_msg <- function(..., sep = " ") {
  message(wrap_txt(..., sep = sep))
}
# nocov end

#' @title Wrap text
#' @name wrap_txt
#' @param ... additional params to pass
#' @param sep how to join elements of string (default is one space)
#' @param strWidth externally set wrapping width. (default value of 100 is not effected)
#' @param errWidth default = FALSE. Set strWidth to be compatible with error printout
#' @export
wrap_txt <- function(..., sep = " ", strWidth = 100, errWidth = FALSE) {
  custom_width <- ifelse(is.null(match.call()$strWidth), yes = FALSE, no = TRUE)
  if (!isTRUE(custom_width)) {
    if (isTRUE(errWidth)) strWidth <- getOption("width") - 6
  }

  cat(..., sep = sep) %>%
    utils::capture.output() %>%
    strwrap(
      prefix = " ", initial = "", # indent later lines, no indent first line
      width = min(80, getOption("width"), strWidth)
    ) %>%
    paste(collapse = "\n")
}








# Color text (8 colors) ####


#' @title Colorize print text
#' @name color_tag
#' @description
#' Return a list of 8 formatting tags for ansi colored text
#' @details supported colors checking is modified from \pkg{cli}
#' \href{https://github.com/r-lib/cli/blob/HEAD/R/num-ansi-colors.R}{aab-num-ansi-colors.R}
#' @keywords internal
#' @return named list of characters
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



#' @describeIn color_tag Determine if system should print color
#' @keywords internal
#' @export
use_color_text <- function() {
  opt <- getOption("giotto.color_show", default = NULL)
  ansi8_color <- ansi_colors() >= 8L
  if (!is.null(opt)) {
    if (!isTRUE(opt)) {
      return(opt)
    }
    if (isTRUE(opt) & isTRUE(ansi8_color)) {
      return(opt)
    }
    if (isTRUE(opt) & !isTRUE(ansi8_color)) {
      wrap_msg('Color text not supported on this system.
               Set options("giotto.color_show" = FALSE)')
    }
  } else {
    ansi8_color
  }
}



#' @describeIn color_tag Determine if system can print at least 8 colors
#' @keywords internal
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
  if (!is.null(cray_opt_has) & !isTRUE(cray_opt_has)) {
    return(1L)
  } # disable
  if (isTRUE(cray_opt_has) & !is.null(cray_opt_num)) {
    return(as.integer(cray_opt_num))
  }
  if (isTRUE(cray_opt_has) & is.null(cray_opt_num)) {
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
  if (.Platform$OS.type == "windows" &
    "--ess" %in% commandArgs() &
    is_emacs_with_color()) {
    return(8L)
  }

  # end catch
  return(8L)
}



#' @describeIn color_tag Determine if emacs can print color
#' @keywords internal
#' @export
is_emacs_with_color <- function() {
  (Sys.getenv("EMACS") != "" || Sys.getenv("INSIDE_EMACS") !=
    "") & !is.na(emacs_version()[1]) & emacs_version()[1] >=
    23
}



#' @describeIn color_tag Determine emacs version
#' @keywords internal
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
