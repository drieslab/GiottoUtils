#' @importClassesFrom data.table data.table
NULL

## data.table helper functions ####

#' @title dt_remove_na
#' @name dt_remove_na
#' @param DT datatable
#' @description set NA values to 0 in a data.table object
#' @concept data.table
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
#' @export
dt_sort_combine_two_columns <- function(DT,
                                        column1,
                                        column2,
                                        myname = "unif_gene_gene") {
  # data.table variables
  values_1_num <- values_2_num <- scolumn_1 <- scolumn_2 <- unif_sort_column <- NULL

  # maybe faster with converting to factors??

  # make sure columns are character
  selected_columns <- c(column1, column2)
  DT[, (selected_columns) := lapply(.SD, as.character), .SDcols = selected_columns]

  # convert characters into numeric values
  uniq_values <- sort(unique(c(DT[[column1]], DT[[column2]])))
  uniq_values_num <- 1:length(uniq_values)
  names(uniq_values_num) <- uniq_values


  DT[, values_1_num := uniq_values_num[get(column1)]]
  DT[, values_2_num := uniq_values_num[get(column2)]]


  DT[, scolumn_1 := ifelse(values_1_num < values_2_num, get(column1), get(column2))]
  DT[, scolumn_2 := ifelse(values_1_num < values_2_num, get(column2), get(column1))]

  DT[, unif_sort_column := paste0(scolumn_1, "--", scolumn_2)]
  DT[, c("values_1_num", "values_2_num", "scolumn_1", "scolumn_2") := NULL]
  data.table::setnames(DT, "unif_sort_column", myname)

  return(DT)
}





#' @title dt_to_matrix
#' @description converts data.table to matrix
#' @param x data.table object
#' @concept data.table
#' @export
dt_to_matrix <- function(x) {
  rownames <- as.character(x[[1]])
  mat <- methods::as(as.matrix(x[, -1]), "Matrix")
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
#' @seealso [data.table::dcast.data.table()]
#' @examples
#' library(data.table)
#' ChickWeight <- as.data.table(ChickWeight)
#' setnames(ChickWeight, tolower(names(ChickWeight)))
#' DT <- melt(as.data.table(ChickWeight), id = 2:4) # calls melt.data.table
#'
#' dt_dcast_string(DT, "chick", "time", "value")
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



# Based on https://stackoverflow.com/questions/37878620/reorder-rows-in-data-table-in-a-specific-order
#' @title Set specific data.table row order
#' @name dt_set_row_order
#' @param x data.table
#' @param neworder numerical vector to reorder rows
#' @export
#' @concept data.table
dt_set_row_order <- function(x, neworder) {
  if (".r" %in% colnames(x)) {
    temp_r <- x[, .SD, .SDcols = ".r"]
    data.table::setorderv(temp_r[, eval(call(":=", as.name(".r_alt"), call("order", neworder)))], ".r_alt")[, ".r_alt" := NULL]
    data.table::setorderv(x[, eval(call(":=", as.name(".r"), call("order", neworder)))], ".r")[, ".r" := NULL]
    x[, eval(call(":=", as.name(".r"), temp_r$.r))]
  } else {
    data.table::setorderv(x[, eval(call(":=", as.name(".r"), call("order", neworder)))], ".r")[, ".r" := NULL]
  }
}
