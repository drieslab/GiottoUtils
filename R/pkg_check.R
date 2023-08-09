#' @title Check for updates to Giotto Suite
#' @name check_github_suite_ver
#' @description Checks the Giotto Suite github repository and compares the version
#' number to the currently installed.
#' @param pkg character. Package to check (pattern matches)
#' @keywords internal
#' @export
check_github_suite_ver <- function(pkg = "Giotto") {
  pkg <- g_match_arg(pkg, c("Giotto", "GiottoUtils", "GiottoClass", "GiottoVisuals"))
  repo <- switch(pkg,
    "Giotto" = "Giotto/suite",
    "GiottoUtils" = "GiottoUtils/master",
    "GiottoClass" = "GiottoClass/master",
    "GiottoVisuals" = "GiottoVisuals/master"
  )

  current_ver <- utils::packageVersion(pkg)
  url <- paste0(
    "https://raw.githubusercontent.com/drieslab/",
    repo,
    "/DESCRIPTION"
  )
  # suppress warnings and errors if inaccessible
  x <- suppressWarnings(try(readLines(url), silent = TRUE))
  if (!inherits(x, "try-error")) {
    gh_ver <- x[grep(pattern = "Version:", x)]
    gh_ver <- gsub(pattern = "Version: ", replacement = "", gh_ver)
    ver_compare <- utils::compareVersion(gh_ver, as.character(current_ver))

    if (ver_compare == 1) wrap_msg("Newer devel version of", pkg, "on GitHub:", gh_ver)
  }
}




# TODO This is more similar to an assertion

#' @title package_check
#' @name package_check
#' @param pkg_name name of package
#' @param repository where is the package
#' @param github_repo name of github repository if needed
#' @param optional whether the package is optional. \code{stop()} is used if TRUE
#' and only \code{message()} will be sent if FALSE.
#' @param custom_msg custom message to be sent instead of default error or message
#' @description check if package is available and provide installation instruction if not available
#' @keywords internal
#' @export
package_check <- function(pkg_name,
                          repository = c("CRAN", "Bioc", "github", "pip"),
                          github_repo = NULL,
                          optional = FALSE,
                          custom_msg = NULL) {
  repository <- match.arg(repository, choices = c("CRAN", "Bioc", "github", "pip"))

  check_message <- function(default_msg, custom_msg, optional) {
    if (!isTRUE(optional)) {
      if (is.null(custom_msg)) {
        stop(default_msg, call. = FALSE)
      } else {
        stop(custom_msg, call. = FALSE)
      }
    } else {
      if (is.null(custom_msg)) {
        message(default_msg)
      } else {
        message(custom_msg)
      }
    }
  }

  if (repository == "CRAN") {
    default_msg <- c(
      "\n package ", pkg_name, " is not yet installed \n",
      "To install: \n",
      "install.packages('", pkg_name, "')"
    )

    if (!requireNamespace(pkg_name, quietly = TRUE)) {
      check_message(
        default_msg = default_msg,
        custom_msg = custom_msg,
        optional = optional
      )
    } else {
      return(TRUE)
    }
  } else if (repository == "Bioc") {
    default_msg <- c(
      "\n package ", pkg_name, " is not yet installed \n",
      "To install: \n",
      "if(!requireNamespace('BiocManager', quietly = TRUE)) install.packages('BiocManager');\nBiocManager::install('", pkg_name, "')"
    )

    if (!requireNamespace(pkg_name, quietly = TRUE)) {
      check_message(
        default_msg = default_msg,
        custom_msg = custom_msg,
        optional = optional
      )
    } else {
      return(TRUE)
    }
  } else if (repository == "github") {
    if (is.null(github_repo)) stop(wrap_txt("provide the github repo of package, e.g. 'johndoe/cooltool' ", sep = ""))

    default_msg <- c(
      "\n package ", pkg_name, " is not yet installed \n",
      "To install: \n",
      "devtools::install_github('", github_repo, "')"
    )

    if (!requireNamespace(pkg_name, quietly = TRUE)) {
      check_message(
        default_msg = default_msg,
        custom_msg = custom_msg,
        optional = optional
      )
    } else {
      return(TRUE)
    }
  } else if (repository == "pip") { # nocov start

    package_check("reticulate", repository = "CRAN")

    default_msg <- c(
      "\n package ", pkg_name, " is not yet installed \n",
      "To install for default Giotto miniconda environment: \n",
      "reticulate::conda_install(envname = 'giotto_env',packages = '", pkg_name, "',pip = TRUE)"
    )

    if (!reticulate::py_module_available(pkg_name)) {
      check_message(
        default_msg = default_msg,
        custom_msg = custom_msg,
        optional = optional
      )
    }
  } # nocov end
}
