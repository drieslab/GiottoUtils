

# Print ####

#' @title Wrap message
#' @name wrap_msg
#' @param ... additional strings and/or elements to pass to wrap_txt
#' @param sep how to join elements of string (default is one space)
#' @export
wrap_msg = function(..., sep = ' ') {
  message(wrap_txt(..., sep = sep))
}

#' @title Wrap text
#' @name wrap_txt
#' @param ... additional params to pass
#' @param sep how to join elements of string (default is one space)
#' @param strWidth externally set wrapping width. (default value of 100 is not effected)
#' @param errWidth default = FALSE. Set strWidth to be compatible with error printout
#' @export
wrap_txt = function(..., sep = ' ', strWidth = 100, errWidth = FALSE) {
  custom_width = ifelse(is.null(match.call()$strWidth), yes = FALSE, no = TRUE)
  if(!isTRUE(custom_width)) {
    if(isTRUE(errWidth)) strWidth = getOption('width') - 6
  }

  cat(..., sep = sep) %>%
    utils::capture.output() %>%
    strwrap(prefix =  ' ', initial = '', # indent later lines, no indent first line
            width = min(80, getOption("width"), strWidth)) %>%
    paste(collapse = '\n')
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
radians = function(deg) {
  deg * pi / 180
}

#' @describeIn degrees Radians to degrees
#' @examples
#' degrees(pi)
#' @export
degrees = function(rad) {
  rad * 180 / pi
}






# Time ####
#' @title Format time for printing
#' @name time_format
#' @param secs numeric. seconds
#' @details Code from \code{\link[data.table]{timetaken}}
#' @export
time_format = function(secs) {
  if(secs > 60) {
    secs = as.integer(secs)
    sprintf("%02d:%02d:%02d", secs%/%3600L, (secs%/%60L)%%60L,
            secs%%60L)
  }
  else {
    sprintf(if(secs >= 10)
      "%.1fs"
      else "%.3fs", secs)
  }
}
