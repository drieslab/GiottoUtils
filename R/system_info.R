#' @title determine_cores
#' @description guesses how many cores to use
#' @return numeric
#' @keywords internal
#' @examples
#' determine_cores()
#' @export
determine_cores <- function(
        cores = getOption("giotto.cores", default = NA),
        min_cores = 1,
        max_cores = 10) {
    if (is.na(cores) ||
        !is.numeric(cores) ||
        (is.numeric(cores) && cores <= 0)) {
        package_check("parallel")
        cores <- parallel::detectCores()

        if (cores <= 2) {
            cores <- ifelse(cores < min_cores, cores, min_cores)
        } else {
            cores <- cores - 2
            cores <- ifelse(cores > max_cores, max_cores, cores)
        }
        options("giotto.cores" = cores)
        return(cores)
    } else {
        cores <- cores
        return(cores)
    }
}




#' @title get_os
#' @description return the type of operating system,
#' see https://conjugateprior.org/2015/06/identifying-the-os-from-r/
#' @return character osx, linux or windows
#' @keywords internal
#' @returns character
#' @examples
#' get_os()
#' @export
get_os <- function() {
    if (.Platform[["OS.type"]] == "windows") {
        os <- "windows"
    } else {
        sysinf <- Sys.info()
        if (!is.null(sysinf)) {
            os <- sysinf["sysname"]
            if (os == "Darwin") {
                os <- "osx"
            }
        } else { ## mystery machine
            os <- .Platform$OS.type
            if (grepl("^darwin", R.version$os)) {
                os <- "osx"
            }
            if (grepl("linux-gnu", R.version$os)) {
                os <- "linux"
            }
        }
    }
    return(tolower(os))
}
