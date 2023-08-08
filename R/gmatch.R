

#' @name g_match_arg
#' @title Partial matching of character choices
#' @description
#' Given an arg to match to, tries to partially match to the provided characeter
#' vector of choices. Matching ignores case and returns the first match.
#' @param arg character. Argument to match
#' @param choices character vector of choices to match to
#' @param \dots additional params passed to grep
#' @keywords internal
#' @export
g_match_arg = function(arg, choices, ...) {
  args_list = list(...)
  args_list$pattern = arg
  args_list$x = choices
  if(is.null(args_list$value)) args_list$value = TRUE
  if(is.null(args_list$ignore.case)) args_list$ignore.case = TRUE

  try_val = try(
    do.call('grep', args_list)[[1]],
    silent = TRUE
  )
  if(inherits(try_val, 'try-error')) {
    stop('\'arg\' should be one of ', paste0('"', choices, '"', collapse = ', '))
  }
  try_val
}
