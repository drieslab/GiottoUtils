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
#' @examples
#' random_seed()
#' 
#' @export
random_seed <- function(set.seed = TRUE) { 
    # TODO deprecate in favor of local_seed
    digits <- 9
    newSeed <- as.numeric(Sys.time()) * 10^(digits - 3)
    newSeed <- as.integer(round(newSeed, -digits) - newSeed)
    if (isTRUE(set.seed)) {
        set.seed(newSeed)
    }
    return(invisible(newSeed))
}





#' @name local_seed
#' @title Set a seed local to a call
#' @description
#' When used inside a call, `local_seed()` first records the existing seed
#' then transiently sets a specific one. When the call exits, the recorded seed
#' is set again, so that the transient seed setting leaves no effects.\cr
#' Based on discussion from \url{https://support.bioconductor.org/p/110439/}
#' @returns set up seed passed to environment
#' @param seed seed value to set
#' @examples
#' f <- function() {
#'     local_seed(1234)
#'     r_val <- rnorm(1)
#'     return(r_val)
#' }
#'
#' rnorm(1) # make sure a seed exists (not needed but handy for this example)
#'
#' seed1 <- .Random.seed
#' x <- f()
#' seed2 <- .Random.seed
#' y <- rnorm(1)
#' seed3 <- .Random.seed
#' z <- f()
#'
#' identical(seed1, seed2)
#' !identical(seed1, seed3)
#'
#' identical(x, z)
#' !identical(x, y)
#' @export
local_seed <- function(seed) {
    prev_seed <- if (.has_seed()) {
        get(".Random.seed", 1)
    } else {
        NULL
    }

    .gutils_prev_seed <- NULL
    assign(".gutils_prev_seed", prev_seed, sys.frame(-1)) 
    # send to prev stack frame

    set.seed(seed)

    do.call(
        "on.exit",
        args = list(add = TRUE, expr = {
            quote(
                if (is.null(.gutils_prev_seed)) {
                    .rm_seed()
                } else {
                    assign(".Random.seed", .gutils_prev_seed, 1)
                }
            )
        }),
        envir = sys.frame(-1)
    )
}


# internals ####

# based on the has_seed() and rm_seed() internals from package withr
.has_seed <- function() {
    exists(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)
}
.rm_seed <- function() {
    if (!.has_seed()) {
        return(NULL)
    }
    rm(".Random.seed", envir = globalenv())
}
