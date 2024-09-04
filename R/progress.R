
#' @name pbar
#' @title Create a \pkg{progressr} progress tracker.
#' @description
#' Create a \pkg{progressr} progress tracker. This is a wrapper around
#' [progressr::progressor()].
#' @inheritParams progressr::progressor
#' @export
#' @seealso [with_pbar()]
#' @examples
#' a <- function(x = 10) {
#'     pb <- pbar(steps = x)
#' }
#'
#' a <- function(x = seq_len(10)) {
#'     pb <- pbar(along = x)
#' }
#'
pbar <- progressr::progressor

#' @name with_pbar
#' @title Track progress while evaluating an R expression
#' @description
#' Track progress while evaluating an R expression. This is a wrapper around
#' [progressr::with_progress()]. \pkg{progressr} is useful because it handles
#' progress reporting in parallel contexts. However, it is worth noting that
#' Giotto does not adhere to \pkg{progressr}'s guideline that `with_progress()`
#' should not be used within packages.
#' The default behavior of displaying progress is more convenient for end
#' users of packages with many functions. This implementation may shift over
#' time.
#' @export
#' @seealso [pbar()]
#' @examples
#' a <- function(x = 10) {
#'     with_pbar({
#'         pb <- pbar(x)
#'         for (i in seq_len(x)) {
#'             Sys.sleep(0.1)
#'             pb()
#'         }
#'     })
#' }
#'
#' a()
with_pbar <- progressr::with_progress



