#' @name file_extension
#' @title Get file extension
#' @description
#' Get the file extension from a provided filepath
#' @param file character. Filepath
#' @returns character
#' @examples
#' file_extension("my_file.txt")
#'
#' @export
file_extension <- function(file) {
    ex <- strsplit(basename(file), split = ".", fixed = TRUE)[[1L]]
    return(ex[-1])
}


#' @title Generate file manifest list from a directory
#' @name dir_manifest
#' @description Create a `list` of full filepaths (character) that are named
#' by with the respective `[basename()]`. Allows easy `$` exploration and
#' indexing of items.\cr
#' All params are directly passed to `[list.files()]` except
#' for `full.names`. `[list.files()]` also normally returns both actual files
#' and directories when `recursive = FALSE`, but this function specifically
#' tests if items are existing files and not directories with
#' `file_test(op = -f)` and fully obeys that flag in all cases.
#' @param path a character vector of full path names; the default corresponds
#' to the working directory, `[getwd()]`. Tilde expansion (see [path.expand])
#' and [`normalizePath()`] are performed. Missing values will be ignored.
#' Elements with a marked encoding  will be converted to the native encoding
#' (and if that fails, considered non-existent).
#' @param pattern an optional regular expression. Only file names which match
#' the regular expression will be returned.
#' @param all.files a logical value. If `FALSE`, only the names of visible
#' files are returned (following Unix-style visibility, that is files whose
#' name does not start with a dot). If `TRUE`, all file names will be returned.
#' @param recursive logical. Should the listing recurse into directories?
#' @param ignore.case logical. Should pattern-matching be case-insensitive?
#' @param include.dirs logical. Should subdirectory names be included in
#' recursive listings?
#' @param no.. logical. Should both `"."` and `".."` be excluded also from
#' non-recursive listings?
#' @param as.list logical. Should output be a list or a named character vector
#' @examples
#' dir_manifest()
#' @returns full and normalized filepaths named by the file basename as either
#' a list (default) or if `as.list = FALSE`, a character vector.
#' @export
dir_manifest <- function(
        path = ".", pattern = NULL, all.files = FALSE, recursive = FALSE,
        ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE,
        as.list = TRUE) {
    a <- get_args_list(keep = c(
        "path", "pattern", "all.files", "recursive", "ignore.case",
        "include.dirs", "no.."
    ))
    a$full.names <- TRUE
    fullpaths <- do.call("list.files", args = a)
    fullpaths <- normalizePath(fullpaths)
    if (include.dirs == FALSE) {
        is_file <- file_test(op = "-f", x = fullpaths)
        fullpaths <- fullpaths[is_file]
    }
    names(fullpaths) <- basename(fullpaths)
    if (as.list) fullpaths <- as.list(fullpaths)
    return(fullpaths)
}


#' @title Fread specific rows based on column matches
#' @name fread_colmatch
#' @param file path to file to load
#' @param col name of col to match from
#' @param sep grep term to match as column delimiters within the file
#' @param values_to_match values in \code{col} to match given as a vector
#' @param verbose whether to print the grep command
#' @param ... additional parameters to pass to \code{\link[data.table]{fread}}
#' @keywords internal
#' @returns A data.table
#' @examples
#' \dontrun{
#' x <- data.frame(a = c("a", "b", "c"), b = 1:3, c = 5:7)
#' write.csv(x, "my_file.csv")
#' fread_colmatch("my_file.csv", col = "a", values_to_match = c(1, 3))
#' }
#'
#' @export
fread_colmatch <- function(file,
    col,
    sep = NULL,
    values_to_match,
    verbose = FALSE,
    ...) {
    package_check("data.table", repository = "CRAN")

    # get colnames
    col_names <- colnames(data.table::fread(file, nrows = 1L))
    col_num <- which(col_names == col)

    # try to guess col separating char if not given
    if (is.null(sep)) {
        filename <- basename(file)
        if (grepl(pattern = ".csv", x = filename)) {
            sep <- ".*,"
        } else if (grepl(pattern = ".tsv", x = filename)) {
            sep <- ".*\t"
        } else {
            .gstop("sep param cannot be guessed")
        }
    }

    # create grep search
    pattern <- paste(values_to_match, collapse = "|")
    gpat <- paste0(
        "'", strrep(x = sep, times = col_num - 1),
        "(", pattern, "),' "
    )
    fread_cmd <- paste0("grep -E ", gpat, file)
    if (isTRUE(verbose)) print(fread_cmd)

    file_DT <- data.table::fread(cmd = fread_cmd, col.names = col_names, ...)
    return(file_DT)
}
