#' @importClassesFrom data.table data.table
NULL

## data.table helper functions ####

#' @title dt_remove_na
#' @name dt_remove_na
#' @param DT datatable
#' @description set NA values to 0 in a data.table object
#' @concept data.table
#' @returns A data.table
#' @examples
#' x <- data.table::data.table(x = 1:3, y = c(1, NA, 2))
#' dt_remove_na(x)
#' x
#'
#' @export
dt_remove_na <- function(DT) {
    for (i in names(DT)) {
        DT[is.na(get(i)), (i) := 0]
    }
    return(DT)
}


#' @title dt_sort_combine_two_columns
#' @name dt_sort_combine_two_columns
#' @param DT datatable
#' @param column1 column1 to use
#' @param column2 column2 to use
#' @param myname name of combined column to generate
#' @description fast sorting and pasting of 2 character columns in a data.table
#' @concept data.table
#' @returns A data.table
#' @examples
#' x <- data.table::data.table(x = 1:3, y = 5:7)
#' dt_sort_combine_two_columns(x, column1 = "x", column2 = "y")
#' x
#'
#' @export
dt_sort_combine_two_columns <- function(
        DT,
        column1,
        column2,
        myname = "unif_gene_gene") {
    # data.table variables
    values_1_num <- values_2_num <- scolumn_1 <- scolumn_2 <-
        unif_sort_column <- NULL

    # maybe faster with converting to factors??

    # make sure columns are character
    selected_columns <- c(column1, column2)
    DT[, (selected_columns) := lapply(.SD, as.character),
        .SDcols = selected_columns
    ]

    # convert characters into numeric values
    uniq_values <- sort(unique(c(DT[[column1]], DT[[column2]])))
    uniq_values_num <- seq_along(uniq_values)
    names(uniq_values_num) <- uniq_values


    DT[, values_1_num := uniq_values_num[get(column1)]]
    DT[, values_2_num := uniq_values_num[get(column2)]]


    DT[, scolumn_1 := ifelse(values_1_num < values_2_num, get(column1),
        get(column2)
    )]
    DT[, scolumn_2 := ifelse(values_1_num < values_2_num, get(column2),
        get(column1)
    )]

    DT[, unif_sort_column := paste0(scolumn_1, "--", scolumn_2)]
    DT[, c("values_1_num", "values_2_num", "scolumn_1", "scolumn_2") := NULL]
    data.table::setnames(DT, "unif_sort_column", myname)

    return(DT)
}





#' @title dt_to_matrix
#' @description converts `data.table` to `Matrix`
#' @param x data.table object
#' @param chunked logical. Whether to chunk the ingestion to `Matrix`
#' @concept data.table
#' @details
#' When matrices are very large, `Matrix::Matrix(x)` and `as(x, "Matrix")` may
#' throw `Error: vector memory exhausted (limit reached?)`. To get around
#' this, we chunk the conversion to `Matrix` by chunks of up to roughly 2e+08
#' matrix cells.
#' @returns A `Matrix`
#' @examples
#' x <- data.table::data.table(x = c("a", "b", "c"), y = 1:3, z = 5:7)
#' dt_to_matrix(x)
#' dt_to_matrix(x, chunked = TRUE)
#'
#' @export
dt_to_matrix <- function(x, chunked = FALSE) {
    package_check("Matrix", repository = "CRAN")

    rownames <- as.character(x[[1]])
    x <- x[, -1]

    if (!chunked) {
        vmsg(.v = NULL, .is_debug = TRUE, "dt_to_matrix: nochunk")
        mat <- Matrix::Matrix(as.matrix(x))
    } else {
        vmsg(.v = NULL, .is_debug = TRUE, "dt_to_matrix: chunked")
        # chunking (to avoid vector mem error)
        #  -- Error was found on a macbook with a 48944 x 21731 matrix
        #
        # Chunk size
        # 10,000 * 20,000 = 2e+08 (matrix cells / chunk)
        chunk_ncells <- 2e+08
        nchunk <- ceiling(prod(dim(x)) / chunk_ncells)
        total_nr <- nrow(x)
        nc <- ncol(x)
        # expected number of rows needed for 2e+08 cells based on ncol
        chunk_rows <- ceiling(chunk_ncells / nc)

        last <- 0L
        mlist <- list()
        for (i in seq_len(nchunk)) {
            vmsg(
                .v = NULL, .is_debug = TRUE,
                "dt_to_matrix: chunk", i, "of", nchunk
            )
            rstart <- last + 1L
            rend <- rstart + chunk_rows
            last <- rend

            chunk <- x[rstart:min(rend, total_nr), ]
            vmsg(
                .v = NULL, .is_debug = TRUE,
                "dt_to_matrix: chunkdims:", paste(dim(chunk))
            )

            # automatically sparsifies if more than half of entries are 0
            mat <- Matrix::Matrix(as.matrix(chunk))
            mlist <- c(mlist, mat)
        }

        mat <- do.call("rbind", mlist)
        if (!identical(dim(mat), dim(x))) {
            stop("Matrix chunked ingestion failed")
        }
    }

    rownames(mat) <- rownames
    return(mat)
}





#' @title dt_dcast_string
#' @description Data.table dcast using character inputs for formula
#' @param data a `data.table`
#' @param col_name1 character. LHS of cast formula
#' @param col_name2 character. RHS of cast formula
#' @param value.var character. Name of the column whose values will be filled to
#' cast.
#' @returns A keyed data.table that has been cast
#' @seealso [data.table::dcast.data.table()]
#' @examples
#' x <- data.table::data.table(
#'     col1 = c(rep("a", 3), rep("b", 3)),
#'     col2 = rep(LETTERS[1:3], 2),
#'     value = c(1:6)
#' )
#' force(x)
#' y <- dt_dcast_string(x, "col1", "col2", "value")
#' force(y)
#' @concept data.table
#' @export
dt_dcast_string <- function(data, col_name1, col_name2, value.var) {
    checkmate::assert_data_table(data)
    data.table::dcast.data.table(
        data,
        paste(col_name1, "~", col_name2),
        value.var = value.var
    )
}



# Based on https://stackoverflow.com/questions/37878620/
#' @title Set specific data.table row order
#' @name dt_set_row_order
#' @param x data.table
#' @param neworder numerical vector to reorder rows
#'
#' @returns A data.table
#' @examples
#' x <- data.table::data.table(x = c("a", "b", "c"), y = 1:3, z = 5:7)
#' dt_set_row_order(x, neworder = c(1, 3, 2))
#' x
#'
#' @export
#' @concept data.table
dt_set_row_order <- function(x, neworder) {
    if (".r" %in% colnames(x)) {
        temp_r <- x[, .SD, .SDcols = ".r"]
        data.table::setorderv(temp_r[, eval(call(
            ":=", as.name(".r_alt"),
            call("order", neworder)
        ))], ".r_alt")[, ".r_alt" := NULL]
        data.table::setorderv(x[, eval(call(
            ":=", as.name(".r"),
            call("order", neworder)
        ))], ".r")[, ".r" := NULL]
        x[, eval(call(":=", as.name(".r"), temp_r$.r))]
    } else {
        data.table::setorderv(x[, eval(call(
            ":=", as.name(".r"),
            call("order", neworder)
        ))], ".r")[, ".r" := NULL]
    }
}
