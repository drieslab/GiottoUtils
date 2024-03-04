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
#' x = data.frame(a = c("a","b","c"), b = 1:3, c = 5:7)
#' write.csv(x, "my_file.csv")
#' fread_colmatch("my_file.csv", col = "a", values_to_match = c(1,3))
#' }
#' 
#' @export
fread_colmatch <- function(
        file,
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
    gpat <- paste0("'", strrep(x = sep, times = col_num - 1), 
                "(", pattern, "),' ")
    fread_cmd <- paste0("grep -E ", gpat, file)
    if (isTRUE(verbose)) print(fread_cmd)

    file_DT <- data.table::fread(cmd = fread_cmd, col.names = col_names, ...)
    return(file_DT)
}
