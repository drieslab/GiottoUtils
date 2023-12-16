
#' @name file_extention
#' @title Get file extention
#' @description
#' Get the file extention from a provided filepath
#' @param file character. Filepath
#' @export
file_extension = function(file) {
  ex = strsplit(basename(file), split = '.', fixed = TRUE)[[1L]]
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
#' @export
fread_colmatch = function(file,
                          col,
                          sep = NULL,
                          values_to_match,
                          verbose = FALSE,
                          ...) {

  package_check('data.table', repository = 'CRAN')

  # get colnames
  col_names = colnames(data.table::fread(file, nrows = 1L))
  col_num = which(col_names == col)

  # try to guess col separating char if not given
  if(is.null(sep)) {
    filename = basename(file)
    if(grepl(pattern = '.csv', x = filename)) {
      sep = '.*,'
    } else if(grepl(pattern = '.tsv', x = filename)) {
      sep = '.*\t'
    } else {
      .gstop('sep param cannot be guessed')
    }
  }

  # create grep search
  pattern = paste(values_to_match, collapse = '|')
  gpat = paste0('\'', strrep(x = sep, times = col_num - 1), '(', pattern, '),\' ')
  fread_cmd = paste0('grep -E ', gpat, file)
  if(isTRUE(verbose)) print(fread_cmd)

  file_DT = data.table::fread(cmd = fread_cmd, col.names = col_names, ...)
  return(file_DT)
}
