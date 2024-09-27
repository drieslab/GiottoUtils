# Run on library loading


.onAttach <- function(libname, pkgname) {
    ## detect onAttach packages ####
    hasDT <- requireNamespace("data.table", quietly = TRUE)
    hasParallel <- requireNamespace("parallel", quietly = TRUE)

    ## check package version ####
    check_ver <- getOption("giotto.check_version", TRUE)
    if (isTRUE(check_ver)) {
        check_github_suite_ver(pkg = "GiottoUtils")
        options("giotto.check_version" = FALSE)
    }

    ## cores detection ##
    check_core <- getOption("giotto.check_core", TRUE)
    if (!hasParallel) check_core <- FALSE
    if (isTRUE(check_core)) {
        cores <- determine_cores(cores = NA)
        if (hasDT) data.table::setDTthreads(threads = cores)
        options("giotto.check_core" = FALSE)
    }

    # options #
    init_option("giotto.verbose", TRUE)
    init_option("giotto.logdir", tempdir())
}
