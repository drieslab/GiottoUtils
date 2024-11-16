#' @name melt_matrix
#' @title Melt a matrix
#' @description Simple implementation of melt for matrices to produces results
#' similar to that of reshape2's now that it is deprecated. \cr
#' The param `na.rm` is not implemented.
#' @param data `matrix` to melt
#' @param varnames variable names to use in molten `data.table`
#' @param \dots further arguments passed to or from other methods
#' @param as.is if `FALSE`, dimnames will be converted to factor or integer etc
#' as appropriate. If `TRUE`, they will be left as strings.
#' @param value.name name of variable used to store values (default = "value")
#' @export
#' @returns `data.table` in long format
#' @examples
#' set.seed(9)
#' Data <- matrix(round(rnorm(12, 10, 4)), nrow = 4, ncol = 3)
#' melt_matrix(Data)
melt_matrix <- function(data, varnames = NULL, ..., as.is = FALSE, value.name = "value") {
    # NSE vars
    Var1 <- Var2 <- NULL

    # determine if rownames are either missing or numericals converted to string
    no_rn <- is.null(rownames(data))
    numeric_rn <- is.numeric(type.convert(rownames(data), as.is = TRUE))
    no_cn <- is.null(colnames(data))
    numeric_cn <- is.numeric(type.convert(colnames(data), as.is = TRUE))

    df <- as.data.frame.table(data, responseName = value.name)
    dt <- data.table::as.data.table(df)

    # `as.data.frame.table` default is factor both when unnamed and when
    # dimnames are present. In order to match default melt behavior:
    # - convert `Var1` and `Var2` to integer when no dimnames were provided or
    #   when dimnames were numeric
    if (no_rn) {
        dt[, Var1 := as.integer(Var1)]
    } else if (numeric_rn) {
        dt[, Var1 := as.character(Var1)]
        if (!as.is) dt[, Var1 := as.integer(Var1)]
    }
    if (no_cn) {
        dt[, Var2 := as.integer(Var2)]
    } else if (numeric_cn) {
        dt[, Var2 := as.character(Var2)]
        if (!as.is) dt[, Var2 := as.integer(Var2)]
    }

    # further convert to character if as.is and rownames were present.
    if (as.is && !no_rn) {
        dt[, Var1 := as.character(Var1)]
    }
    if (as.is && !no_cn) {
        dt[, Var2 := as.character(Var2)]
    }

    if (!is.null(varnames)) {
        checkmate::assert_character(varnames, len = 2L)
        data.table::setnames(dt, old = c("Var1", "Var2"), new = varnames)
    }

    return(dt)
}
