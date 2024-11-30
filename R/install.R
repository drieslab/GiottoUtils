#' @name suite_packages
#' @title Giotto Suite Packages
#' @description Returns character vector of Giotto Suite's packages. Only the
#' core packages are returned by default.
#' @param type character. Type of module to return. Current choices are `"core"`
#' for the packages needed for Giotto to run, `"extensions"` for extending
#' packages such as *GiottoData*, and `"all"` for all current modules to be
#' returned.
#' @examples
#' suite_packages()
#' suite_packages("core")
#' suite_packages("extensions")
#' suite_packages("all")
#' @export
suite_packages <- function(type = "core") {
    type <- match.arg(type, c("core", "extensions", "all"))
    suite_pkgs <- list(
        "core" = c("GiottoUtils", "GiottoClass", "GiottoVisuals", "Giotto"),
        "extensions" = c("GiottoData", "GiottoDB")
    )
    switch(type,
        "core" = suite_pkgs$core,
        "extensions" = suite_pkgs$extensions,
        "all" = c(suite_pkgs$core, suite_pkgs$extensions)
    )
}

#' @name suite_install
#' @title Giotto Suite GitHub Installation
#' @description
#' `r lifecycle_badge("experimental")`\cr
#' Installation convenience function. Helps with Giotto Suite installation from
#' the GitHub website by ensuring modules are installed in the correct order.
#' Also automatically switches to the R4.4.0 version when user R version is
#' lower than 4.4.1.\cr
#'
#' This utility will do a full reinstall of the specified Giotto Suite modules
#' but non-Suite dependencies will never prompt to be updated.
#' @param modules character. Which modules to install. Defaults to the core
#' packages needed for Giotto to run.
#' @param suite_deps logical. Whether to install any potential Giotto Suite
#' dependency modules
#' @param ref character. Currently one of "main", "R4.4.0", or "dev". These
#' determine which branches to install. See details.
#' @param \dots additional params to pass to `remotes::install_github()`
#' @section ref `"main"`:
#' Installs the main Giotto version. This version is expected to chase the
#' highest R version.
#' @section ref `"R4.4.0"`:
#' Mirrors the main branch, but locks the R version at 4.4.0
#' @section ref `"dev"`:
#' This version is ahead of the main version and also chases the latest R
#' version.
#' @examples
#' if (FALSE) {
#'     # install core packages
#'     suite_install()
#'     suite_install("GiottoClass", ref = "dev")
#'
#'     # install ONLY Giotto, ignoring module dependencies
#'     # (i.e. GiottoVisuals, GiottoClass, etc)
#'     suite_install("Giotto", suite_deps = FALSE)
#' }
#' @export
suite_install <- function(modules = suite_packages(), suite_deps = TRUE, ref = "main", ...) {
    
    package_check("remotes", repository = "CRAN")
    
    # switch to R4.4.0 branch if user version low
    if (identical(ref, "main") && .rver() < "4.4.1") {
        ref <- "4.4.0"
    }

    not_module <- !modules %in% suite_packages("all")
    if (any(not_module)) {
        stop(sprintf(
            "The following are not Giotto Suite modules:\n\'%s\'",
            paste(modules[not_module], collapse = "\', \'")
        ))
    }

    # handle module deps
    if (suite_deps) {
        if ("Giotto" %in% modules) {
            modules <- c(
                "Giotto", "GiottoVisuals", "GiottoClass", "GiottoUtils", modules
            )
        } else if ("GiottoVisuals" %in% modules) {
            modules <- c("GiottoVisuals", "GiottoClass", "GiottoUtils", modules)
        } else if ("GiottoData" %in% modules) {
            modules <- c("GiottoClass", "GiottoUtils", modules)
        } else if ("GiottoDB" %in% modules) {
            modules <- c("GiottoClass", "GiottoUtils", modules)
        } else if ("GiottoClass" %in% modules) {
            modules <- c("GiottoUtils", modules)
        }
    }

    modules <- unique(modules)
    # establish install order
    match_res <- match(.module_inst_order, modules)
    match_res <- match_res[!is.na(match_res)]
    modules <- modules[match_res]

    vmsg(.is_debug = TRUE, paste0(modules, collapse = "\n"), .prefix = "")

    # pick set of repo references
    ref <- match.arg(ref, c("main", "dev", "R4.4.0"))
    fullrefs <- switch(ref,
        "main" = .mainrefs[modules],
        "dev" = .devrefs[modules],
        "R4.4.0" = .r440refs[modules]
    )

    repos <- fullrefs[modules]
    vmsg(.is_debug = TRUE, "\n")
    vmsg(.is_debug = TRUE, paste0(repos, collapse = "\n"), .prefix = "")

    # install loop
    for (r in repos) {
        remotes::install_github(repo = r, upgrade = "never", ...)
    }
    return(invisible(TRUE))
}






# internals ####

# get the R version
.rver <- function() {
    paste(version$major, version$minor, sep = ".")
}

.mainrefs <- c(
    GiottoUtils = "drieslab/GiottoUtils",
    GiottoClass = "drieslab/GiottoClass",
    GiottoVisuals = "drieslab/GiottoVisuals",
    Giotto = "drieslab/Giotto",
    GiottoData = "drieslab/GiottoData",
    GiottoDB = "drieslab/GiottoDB"
)

.devrefs <- c(
    GiottoUtils = "drieslab/GiottoUtils@dev",
    GiottoClass = "drieslab/GiottoClass@dev",
    GiottoVisuals = "drieslab/GiottoVisuals@dev",
    Giotto = "drieslab/Giotto@suite_dev",
    GiottoData = "drieslab/GiottoData@dev",
    GiottoDB = "drieslab/GiottoDB@dev"
)

.r440refs <- c(
    GiottoUtils = "drieslab/GiottoUtils@R4.4.0",
    GiottoClass = "drieslab/GiottoClass@R4.4.0",
    GiottoVisuals = "drieslab/GiottoVisuals@R4.4.0",
    Giotto = "drieslab/Giotto@R4.4.0",
    GiottoData = "drieslab/GiottoData", # TODO
    GiottoDB = "drieslab/GiottoDB" # TODO
)

.module_inst_order <- c(
    "GiottoUtils",
    "GiottoClass",
    "GiottoData",
    "GiottoDB",
    "GiottoVisuals",
    "Giotto"
)
