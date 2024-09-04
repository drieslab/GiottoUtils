


#' @name melt_matrix
#' @title Melt a matrix
#' @description Simple implementation of melt for matrices to produces results
#' similar to that of reshape2's now that it is deprecated. \cr
#' The params `varnames`, `na.rm` and, and `as.is` are not implemented.
#' @export
#' @examples
#' set.seed(9)
#' Data <- matrix(round(rnorm(12, 10, 4)), nrow = 4, ncol = 3)
#' melt_matrix(Data)
melt_matrix <- function(data, value.name = "value") {
    df <- as.data.frame.table(data, responseName = value.name)
    dt <- data.table::as.data.table(df)

    if (is.null(rownames(data)) ||
        suppressWarnings(all(!is.na(as.numeric(rownames(data)))))) {
        dt[, Var1 := as.integer(Var1)]
    }
    if (is.null(colnames(data)) ||
        suppressWarnings(all(!is.na(as.numeric(colnames(data)))))) {
        dt[, Var2 := as.integer(Var2)]
    }

    return(dt)
}



