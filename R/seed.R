#' @title Set a random seed
#' @name random_seed
#' @param set.seed Whether to apply the random seed. Defaults to TRUE
#' @description
#' From reproducible package, set.randomseed()\cr
#' Convenient function for applying a random seed. Usually used with
#' `on.exit()` when a specified seed is applied so that non-random operations
#' do not interfere with other aspects of a user's work. May also be called with
#' `set.seed = TRUE` to simply return a random seed to use, without actually
#' having applied it.
#' @return The seed value is returned invisibly
#' @export
random_seed <- function(set.seed = TRUE) {
    digits <- 9
    newSeed <- as.numeric(Sys.time()) * 10^(digits - 3)
    newSeed <- as.integer(round(newSeed, -digits) - newSeed)
    if (isTRUE(set.seed)) {
        set.seed(newSeed)
    }
    return(invisible(newSeed))
}
