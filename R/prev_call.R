#' @name prev_call
#' @title Previous calls
#' @description Functions to check the call stack and get aspects of previous
#' calls. Used in putting together object histories and error handling.
#' @param toplevel integer. Relative stack where the function call was made.
#' @param verbose be verbose
NULL


#' @describeIn prev_call Get previous call
#' @export
get_prev_call <- function(toplevel = 1L) {
  deparse(sys.call(-toplevel))
}


# Determine the name of the function n levels above the current evaluation frame,
# where n is toplevel - 1
#' @describeIn prev_call Get previous call function name
#' @export
get_prev_fname <- function(toplevel = 3L) {
  as.character(sys.call(-toplevel)[[1]])
}



#' @describeIn prev_call Get previous call args as named character vector
#' @export
get_args <- function(toplevel = 2L, verbose = FALSE) {
  nframes <- sys.nframe()

  if (isTRUE(verbose)) {
    cat("\n number of frames: ")
    print(nframes)
    cat("\n")
  }


  cl <- sys.call(-toplevel)

  if (isTRUE(verbose)) {
    cat("\n system call: ")
    print(cl)
    cat("\n")
  }


  # function name
  fname <- as.character(cl[[1]])

  if (length(fname) > 1) {
    fname <- fname[[3]]
  }

  if (isTRUE(verbose)) {
    cat("\n function name: ")
    print(fname)
    cat("\n")
  }


  # function
  # f = get(x = fname, mode = "function", pos = 'package:Giotto')
  f <- get(x = fname, mode = "function", pos = sys.frame(-2))

  # get used arguments
  cl <- match.call(definition = f, call = cl)
  user_args <- as.list(cl)[-1]

  # all fun arguments
  fun_args <- formals(fun = fname)
  fun_args[names(user_args)] <- user_args

  unl_args <- unlist(fun_args)
  final_args <- as.character(unl_args)
  names(final_args) <- names(unl_args)

  # select first from vector
  bool_det <- grepl("c\\(", final_args)
  if (any(bool_det) == TRUE) {
    for (bool_name in names(final_args[bool_det])) {
      bool_vec <- final_args[bool_name]
      new_vec <- strsplit(bool_vec, split = "\"")[[1]][2]

      final_args[bool_name] <- new_vec
    }
  }

  return(final_args)
}

#' @describeIn prev_call Get call args as named list
#' @param \dots additional params to capture
#' @export
get_args_list <- function(toplevel = 1L, ...) {
  c(
    as.list(as.environment(parent.frame(toplevel))),
    list(...)
  )
}
