## General utility functions ##


#' @title lapply_flex
#' @name lapply_flex
#' @param X list to use
#' @param FUN function to be performed
#' @param cores cores to use
#' @param future.seed whether to set a seed when using future_lapply
#' @param fun deprecated. Backwards compatibility for FUN
#' @param ... other arguments to pass
#' @keywords internal
#' @export
lapply_flex <- function(X,
                        FUN,
                        cores = NA,
                        future.seed = TRUE,
                        fun = NULL,
                        ...) {
  # a simple wrapper for future.apply::future_lapply
  # probably does not need any additional changes

  # potential addition:
  # check if future::plan() was already set by user
  # if not, set plan(multisession, workers = cores) by default


  # backwards compatible with previous version
  if (!is.null(fun)) {
    FUN <- fun
  }

  # get type of os
  os <- .Platform$OS.type

  # set number of cores automatically, but with limit of 10
  cores <- determine_cores(cores)

  # future_lapply call
  save_list <- future.apply::future_lapply(X = X, FUN = FUN, future.seed = future.seed, ...)

  # if(os == 'unix') {
  #  save_list = parallel::mclapply(X = X, mc.cores = cores,
  #                                 FUN = fun, ...)
  # } else if(os == 'windows') {
  #  save_list = parallel::mclapply(X = X, mc.cores = 1,
  #                                 FUN = fun, ...)
  # }

  return(save_list)
}
