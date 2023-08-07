

#' @name special_characters
#' @title Special characters
#' @description
#' Framework and functions for printing with special characters within R packages.
NULL


# Framework functions -------------------------------------------------------- #
# Determine whether system is utf8 and will produce the expected symbols based
# on the provided escape codes

# nocov start
#' @describeIn special_characters Determine if print is latex output
#' @keywords internal
#' @export
is_latex_output = function() {
  if(!('knitr' %in% loadedNamespaces())) return(FALSE)
  get('is_latex_output', asNamespace('knitr'))()
}
# nocov end

#' @describeIn special_characters Determine if system is using UTF-8 encoding
#' @keywords internal
#' @export
is_utf8_output = function() {
  opt = getOption('cli.unicode', default = NULL)
  if(!is.null(opt)) return(isTRUE(opt))
  opt = getOption('giotto.unicode', default = NULL)
  if(!is.null(opt)) return(isTRUE(opt))

  is_utf8 = (l10n_info()$`UTF-8` & !is_latex_output())
  options('giotto.unicode' = is_utf8)
  return(is_utf8)

}

# ---------------------------------------------------------------------------- #







#' @title Box characters
#' @describeIn special_characters Helper function to print unicode box characters using escape codes.
#' @keywords internal
#' @details Much inspiration taken from \pkg{fs} \href{https://rdrr.io/cran/fs/src/R/tree.R}{tree.R}
#' These are derived from: \href{https://github.com/r-lib/cli/blob/e9acc82b0d20fa5c64dd529400b622c0338374ed/R/tree.R#L111}{code}
#' @export
box_chars = function() {
  if(is_utf8_output()) {
    list(
      "h" = "\u2500",                   # horizontal
      "v" = "\u2502",                   # vertical
      "l" = "\u2514",
      "j" = "\u251C",
      "b" = "\u2514\u2500\u2500",       # branch
      "t" = "\u251C\u2500\u2500",       # T
      "i" = "\u2502  ",                 # layer
      "s" = "   "                       # spaces
    )
  } else {
    list(
      "h" = "-",                        # horizontal
      "v" = "|",                        # vertical
      "l" = "\\",
      "j" = "+",
      "b" = "\\--",                     # branch
      "t" = "+--",                      # T
      "i" = "|  ",                      # layer
      "s" = "   "                       # spaces
    )
  }
}
