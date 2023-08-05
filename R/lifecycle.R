
# lifecycle deprecation functions


#' @importFrom lifecycle deprecated
#' @family lifecycle
#' @export
lifecycle::deprecated

#' @importFrom lifecycle is_present
#' @export
lifecycle::is_present

#' @importFrom lifecycle deprecate_soft
#' @export
lifecycle::deprecate_soft

#' @importFrom lifecycle deprecate_warn
#' @export
lifecycle::deprecate_warn

#' @importFrom lifecycle deprecate_stop
#' @export
lifecycle::deprecate_stop

#' @name lifecycle_badge
#' @title lifecycle badge
#' @description
#' Generate markdown needed for lifecycle badges that help communicate stability
#' of functions in documentation when added in backticks as shown below.
#' ```
#'  #' `r lifecycle::badge("experimental")`
#'  #' `r lifecycle::badge("deprecated")`
#'  #' `r lifecycle::badge("superseded")`
#' ```
#' @param stage character. 'stable', 'experimental', 'deprecated', or 'superseded'
#' @importFrom lifecycle badge
#' @family lifecycle
#' @export
lifecycle_badge = function(stage = 'stable') {
  lifecycle::badge(stage)
}
