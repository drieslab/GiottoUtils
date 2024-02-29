# Functions for logging progress

#' @name giottoReadLog
#' @title Read from the last generated log file
#' @param filepath character. filepath to log file. If omitted, tries to find the
#' last created log (will not work after a crash)
#' @returns character
#' @examples
#' giottoReadLog()
#' giottoReadLog("my_path_to_log")
#' 
#' @export
giottoReadLog <- function(filepath = getOption("giotto.last_logpath", NULL)) {
    if (is.null(filepath)) {
        .gstop("No last log found.", .call = FALSE)
    }

    file_conn <- file(filepath)
    on.exit(close(file_conn))
    readLines(file_conn)
}

#' @name giottoNewLog
#' @title Create a new Giotto log
#' @param logdir (optional) specific directory in which to generate logfiles.
#' If not provided, will choose a directory based on
#' `getOption("giotto.logdir", tempdir())`
#' @returns a log file
#' @examples
#' giottoNewLog("my_log_directory")
#' 
#' @export
giottoNewLog <- function(logdir) {
    # if logdir is provided, create a new log file in the specified logdir
    if (!missing(logdir)) .log_dir(logdir)
    # otherwise use directory defaults

    .log_create()
}




# Unique .txt filename creator. Creates a base of YYYYMMDD_I on top of which a
# prefix and suffix can be attached. The I stands for the I-th file existing
# that was generated on this date.
.unique_filename <- function(
        filedir = tempdir(),
        prefix = "giotto_",
        suffix = NULL) {
    logfile_base <- Sys.Date() %>%
        gsub(pattern = "-", replacement = "")

    # generate initial full ID
    i <- 1
    full_path <- .combine_filename(filedir, logfile_base, prefix, suffix, i = i)

    # loop until an unused name is found
    while (file.exists(full_path)) {
        i <- i + 1 # increment
        full_path <- .combine_filename(filedir, logfile_base, 
                                    prefix, suffix, i = i)
    }

    return(full_path)
}

.combine_filename <- function(filedir, logfile_base, prefix, suffix, i = 1) {
    logfile_id <- paste0(prefix, logfile_base, "_", i, suffix, ".txt")
    full_path <- file.path(filedir, logfile_id)
    return(full_path)
}


#' @title Giotto logging directory
#' @name .log_dir
#' @description
#' Set the Giotto logging directory. New logfiles will be generated here.
#' @param logdir character. Directory to log to (defaults to tempdir)
#' @returns internal setting of option giotto.logdir
#' @keywords internal
.log_dir <- function(logdir = tempdir()) {
    options("giotto.logdir" = logdir)
}

#' @name .log_create
#' @title Create a log file
#' @param filedir character. Directory to create a logfile
#' @description
#' Creates a file called 'log.txt' at the specified directory. If no directory
#' is provided, it defaults to `tempdir()`, but a specific one can be provided
#' by setting it to the "giotto.logdir" option or using `.log_dir()`.
#' The filepath is additionally written to the option 'giotto.last_logpath'.
#' @returns file 'log.txt'
#' @keywords internal
.log_create <- function(filedir = getOption("giotto.logdir", tempdir())) {
    if (!dir.exists(filedir)) dir.create(filedir, recursive = TRUE)
    filepath <- .unique_filename(filedir = filedir, prefix = "giotto_")
    file.create(filepath)
    filepath <- normalizePath(filepath)

    options("giotto.last_logpath" = filepath) # store filepath
    return(invisible(filepath))
}


#' @title Create a logfile connection
#' @name .log_conn
#' @param filepath path to the logfile
#' @description Create an active file connection object to the logfile to
#' write to. Opens it in mode "a+" which allows both appending and reading.
#' @returns file connection
#' @keywords internal
.log_conn <- function(filepath = getOption("giotto.last_logpath", NULL)) {
    if (is.null(filepath)) {
        filepath <- .log_create() %>%
            normalizePath()
        message("Logging to:", filepath)
    }

    file(filepath, open = "a+") # open in 'a'ppend and reading (+) mode
}

#' @title Write to log file
#' @name log_write
#' @param file_conn a file connection (Uses last created or generates a new one
#' if previous does not exist.)
#' @param x character vector. Content to write
#' @param collapse character. Collapse to use with `x`
#' @param main character. Title to assign log entry
#' @returns character
#' @export
log_write <- function(file_conn = .log_conn(), x = "", 
                    collapse = " ", main = NULL) {
    on.exit(close(file_conn), add = TRUE)

    if (!is.null(main)) main <- str_bracket(main)

    log_entry <- paste(x, collapse = collapse)

    timestamped_log_entry <- paste(
        main,
        str_parenth(as.character(format(Sys.time()))),
        log_entry,
        collapse = " "
    )
    writeLines(timestamped_log_entry, con = file_conn, sep = "\n")
}
