#' @title Check for updates to Giotto Suite
#' @name check_github_suite_ver
#' @description Checks the Giotto Suite github repository and compares the
#' version number to the currently installed.
#' @param pkg character. Package to check (pattern matches)
#' @keywords internal
#' @returns Message indicating a new version available, otherwise returns NULL
#' @examples
#' check_github_suite_ver("GiottoUtils")
#' @export
check_github_suite_ver <- function(pkg = "Giotto") {
    pkg <- g_match_arg(
        pkg, c("Giotto", "GiottoUtils", "GiottoClass", "GiottoVisuals")
    )
    repo <- switch(pkg,
        "Giotto" = "Giotto/suite",
        "GiottoUtils" = "GiottoUtils/master",
        "GiottoClass" = "GiottoClass/master",
        "GiottoVisuals" = "GiottoVisuals/master"
    )

    current_ver <- utils::packageVersion(pkg)
    url <- sprintf(
        "https://raw.githubusercontent.com/drieslab/%s/DESCRIPTION", repo
    )

    new_avail <- new_github_ver_avail(
        url = url, current_ver = current_ver
    )

    if (!is.null(new_avail)) {
        wrap_msg("Newer devel version of", pkg, "on GitHub:", new_avail)
    }
}


#' @title Check if a package has newer github version
#' @name new_github_ver_avail
#' @description
#' This function works by downloading the DESCRIPTION file from a github
#' repo and then comparing the verison number with a provided value. If
#' the download fails or the github version is not newer than what is
#' installed, then it silently returns NULL. If the github version is newer,
#' the new version number will be returned
#' @param url character. url to the package to check's DESCRIPTION file.
#' @param current_ver character. Current version to check against
#' @examples
#' url <- "https://raw.githubusercontent.com/drieslab/GiottoData/master/DESCRIPTION"
#' new_github_ver_avail(url, 0.2)
#' @returns character. Version number
#' @export
new_github_ver_avail <- function(
        url, current_ver = NULL
) {
    # Return NULL if any warnings or errors due to inaccessible
    descfile <- tryCatch(
        expr = readLines(url),
        warning = function(w) NULL,
        error = function(e) NULL
    )
    # silently return NULL if not found
    if (is.null(descfile)) return(invisible())

    # parse github version number
    gh_ver <- descfile[grep(pattern = "Version:", descfile)]
    gh_ver <- gsub(pattern = "Version: ", replacement = "", gh_ver)
    # see if GH version is newer
    ver_compare <- utils::compareVersion(gh_ver, as.character(current_ver))

    # silently return NULL if not newer
    if (ver_compare != 1) return(invisible())

    # return newer version number
    return(gh_ver)
}




#' @title package_check
#' @name package_check
#' @param pkg_name name of package
#' @param repository where is the package (in format repo:cooltool for CRAN,
#' Bioc, and pip repos. format repo:johndoe/cooltool for github or bitbucket)
#' @param github_repo name of github repository if needed
#' @param optional whether the package is optional. If `TRUE`, an error is
#' thrown. If `FALSE`, a warning is sent
#' and only \code{message()} will be sent if FALSE.
#' @param custom_msg custom message to be sent instead of default error or
#'  message
#' @description check if package is available and provide installation
#' instruction if not available. pip installations can accept github links.
#' For pip links, the `repo:link` format is always preferred especially since
#' the link cannot be used as the python package name to check the isntallation
#' status of.
#' @keywords internal
#' @returns character
#' @examples
#' \dontrun{
#' package_check("Matrix")
#' package_check("BiocSingular", repository = "Bioc")
#' # (only expected to work when giottoenv is loaded)
#' package_check("leidenalg", repository = "pip:leidenalg")
#'
#' # expected to fail
#' package_check("faketool")
#' package_check("faketool", repository = "Bioc")
#' package_check("installme", repository = "pip:installme")
#'
#' # vectorized
#' package_check(
#'     pkg_name = c("faketool", "cooltool"),
#'     repository = c("CRAN", "github:johndoe/cooltool")
#' )
#'
#' # github pip checks
#' package_check(
#'     pkg_name = "pysodb",
#'     repository =
#'         "pip:git+https://github.com/TencentAILabHealthcare/pysodb.git"
#' )
#' }
#' @export
package_check <- function(
        pkg_name,
        repository = NULL,
        github_repo = NULL,
        optional = FALSE,
        custom_msg = NULL) {
    # NSE vars
    repo <- location <- name <- NULL

    # set default repo to CRAN if not supplied
    no_val <- is.null(repository)
    if (any(is.na(repository))) no_val <- is.na(repository)
    if (any(no_val)) {
        repository[no_val] <- paste0("CRAN:", pkg_name[no_val])
    }
    no_split <- !vapply(
        repository,
        function(x) grepl(":", x),
        FUN.VALUE = logical(1L)
    )
    repository[no_split] <- sprintf(
        "%s:%s", repository[no_split], pkg_name[no_split]
    )


    # handle deprecations
    # Only single length inputs to github repo can be recovered from.
    # Longer inputs make the relative ordering of the other param inputs unclear
    if (!is.null(github_repo)) {
        if (length(github_repo) > 1L) {
            stop(sprintf(
                "%s\n  %s",
                "[pkg_check] 'github_repo' param is deprecated",
                "Use 'repository' param with github:repo style formatting instead"
            ))
        } else {
            # only allowed for single length github_repo inputs
            # assume github is the repository requested
            repository <- paste0("github:", github_repo)
        }
    }


    checkmate::assert_character(pkg_name)
    checkmate::assert_logical(optional)

    # optional is a flag for whether error or warning is thrown.
    # throw error if ANY optional entry is FALSE
    is_error <- any(!optional)

    # treat custom_msg as a replacement for the overall error/warning message

    repo_split <- strsplit(repository, ":")
    repos_dt <- data.table::data.table(
        name = pkg_name,
        repo = lapply(repo_split, function(r) r[1L]),
        location = lapply(
            repo_split, function(r) paste(r[-1L], collapse = ":")
        )
    )


    # check repos
    repo_choices <- c("CRAN", "Bioc", "github", "bitbucket", "pip")

    if (!all(repos_dt[, unlist(repo)] %in% repo_choices)) {
        stop(sprintf(
            "[package_check] all 'repository' input(s) must be one of\n  \'%s\'",
            paste(repo_choices, collapse = "', '")
        ))
    }

    # check missing packages
    repos_dt[, missing := vapply(
        seq(.N),
        function(i) {
            .check_package_handler(name = name[[i]], repo = repo[[i]])
        },
        FUN.VALUE = logical(1L),
        USE.NAMES = FALSE
    )]

    install_dt <- repos_dt[(missing), ]
    if (nrow(install_dt) == 0L) {
        return(invisible(TRUE))
    } # return TRUE if no installs needed



    # prints

    # select console print function
    print_fun <- ifelse(
        is_error,
        function(...) stop(..., call. = FALSE),
        function(...) {
            message(...)
            return(invisible(FALSE))
        }
    )



    # custom install msg
    if (!is.null(custom_msg)) {
        print_fun(custom_msg)
    } else { # default install msg

        # header
        plural_installs <- install_dt[, .N > 1L]
        inst_msg <- sprintf(
            "package%s '%s' %s not yet installed\n\n To install:\n",
            ifelse(plural_installs, "s", ""),
            install_dt[, paste0(name, collapse = "\', \'")],
            ifelse(plural_installs, "are", "is")
        )

        # pip
        inst_msg <- c(
            inst_msg,
            .msg_pip_install(location = install_dt[repo == "pip", location])
        )
        # bioc
        inst_msg <- c(
            inst_msg,
            .msg_bioc_install(location = install_dt[repo == "Bioc", location])
        )
        # cran
        inst_msg <- c(
            inst_msg,
            .msg_cran_install(location = install_dt[repo == "CRAN", location])
        )
        # github
        inst_msg <- c(
            inst_msg,
            .msg_github_install(location = install_dt[repo == "github", location])
        )
        # bitbucket
        inst_msg <- c(
            inst_msg,
            .msg_bitbucket_install(location = install_dt[repo == "bitbucket", location])
        )

        print_fun(inst_msg)
    }
}




# package check functions ####
# return TRUE if install is needed
.check_package_handler <- function(name, repo) {
    switch(repo,
        "pip" = .check_package_py(name),
        .check_package_r(name) # default
    )
}

.check_package_r <- function(name) {
    !requireNamespace(name, quietly = TRUE)
}

.check_package_py <- function(name) {
    package_check("reticulate", repository = "CRAN")
    !reticulate::py_module_available(name)
}



# vectorized install prints ####
.msg_github_install <- function(location) {
    if (length(location) == 0L) {
        return(NULL)
    }
    sprintf(
        "devtools::install_github(\"%s\")\n",
        location
    )
}

.msg_cran_install <- function(location) {
    if (length(location) == 0L) {
        return(NULL)
    }
    locs_string <- paste0(location, collapse = "\", \"")

    sprintf(
        "install.packages(c(\"%s\"))\n",
        locs_string
    )
}

.msg_bitbucket_install <- function(location) {
    if (length(location) == 0L) {
        return(NULL)
    }
    sprintf(
        "devtools::install_bitbucket(\"%s\")\n",
        location
    )
}

.msg_bioc_install <- function(location) {
    if (length(location) == 0L) {
        return(NULL)
    }
    locs_string <- paste0(location, collapse = "\", \"")

    sprintf(
        "if(!requireNamespace('BiocManager', quietly = TRUE)) install.packages('BiocManager');\nBiocManager::install(c(\"%s\"))\n",
        locs_string
    )
}

.msg_pip_install <- function(location) { # nocov start
    if (length(location) == 0L) {
        return(NULL)
    }

    py <- py_active_env()
    header_msg <- sprintf(
        "## active python env: '%s' \n## python version: %s\n%s %s\n",
        py, getOption("giotto.py_active_ver"),
        "## restart session then use GiottoClass::set_giotto_python_path()",
        "if this is incorrect"
    )

    inst_msg <- sprintf(
        "reticulate::conda_install(envname = '%s', packages = c('%s'), pip = TRUE)\n",
        py, paste(location, collapse = "', '")
    )

    c(header_msg, inst_msg)
} # nocov end
