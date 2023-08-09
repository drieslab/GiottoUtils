# ---------- #
# Assertions #
# ---------- #

# Assert something is true. If an assertion is false, then a (preferably informative)
# error should be thrown. These can help guide developers and users about the
# proper usage and limitations of your functions. No values are returned.
# Use these for guard clauses.

# global for g_assert
.name <- NULL

# Note that assertions built upon g_assert require that the object asserted againt
# MUST be supplied as param from the parent frame

#' @title Assertion framework
#' @name g_assert
#' @description Framework to perform assertions and provide informative errors
#' about both the input object and the test that was performed. Functions for
#' specific checks and messages can be built using this base function.
#' @param x object to test
#' @param test test to perform that should have a TRUE or FALSE outcome
#' @param msg error message provided as character vector. Using the token .name
#' will send the name of object being tested in the stack frame referenced by
#' \code{n}
#' @param n stack frames back in which to evaluate the param
#' @param ... additional params to pass
#' @keywords internal
#' @examples
#' \dontrun{
#' g_assert(
#'   x,
#'   test = inherits(x, "data.table"),
#'   msg = c(.name, "must be of class data.table, not", class(x))
#' )
#' }
#' @export
g_assert <- function(x, test, msg = NULL, n = 2L, ...) {
  if (!test) {
    # get name of function where test failed
    fn_name <- deparse(sys.calls()[[sys.nframe() - n]])
    # get name of object that failed test
    .name <- deparse(eval(call("substitute", as.name(substitute(x)), parent.frame(n = 1L))))
    .name <- paste0('\"\'', .name, '\'\"')

    # compose message
    msg <- gsub(pattern = "\\.name", replacement = .name, x = deparse(substitute(msg)))
    msg <- parse(text = msg)

    # send error
    stop(wrap_txt(fn_name, ":\n", eval(msg, envir = parent.frame(n = 1L)), errWidth = TRUE),
      call. = FALSE
    )
  }
}



#' @describeIn g_assert Test for whether supplied object is a \code{giotto} object
#' @param gobject giotto object
#' @keywords internal
#' @export
guard_against_notgiotto <- function(gobject, n = 1L, ...) {
  fn_name <- deparse(sys.calls()[[sys.nframe() - n]])
  orig_name <- deparse(eval(call("substitute", as.name(substitute(gobject)), parent.frame())))
  if (!methods::hasArg(gobject)) {
    stop(
      wrap_txt(fn_name, ":\ngiotto object must be given",
        errWidth = TRUE
      ),
      call. = FALSE
    )
  }
  if (!inherits(gobject, "giotto")) {
    stop(
      wrap_txt(fn_name, ":\n", orig_name, "is not a giotto object",
        errWidth = TRUE
      ),
      call. = FALSE
    )
  }
}



#' @describeIn g_assert Test whether input is a data.table object
#' @export
assert_DT <- function(x) {
  g_assert(
    x,
    test = inherits(x, "data.table"),
    msg = c(.name, "must be of class data.table, not", class(x))
  )
}


# NOTE: this currently overrides the checkmate function of the same name
#' @describeIn g_assert Test whether input is an existing file
#' @export
assert_file <- function(x) {
  g_assert(
    x,
    test = is.character(x),
    msg = c(.name, "must be a character vector filepath")
  )
  g_assert(
    x,
    test = file.exists(x),
    msg = c(.name, "is not an existing file")
  )
}

# NOTE: this currently overrides the checkmate function of the same name
#' @describeIn g_assert Test whether input is of class numeric
#' @export
assert_numeric <- function(x) {
  g_assert(
    x,
    test = is.numeric(x),
    msg = c(.name, "must be of class numeric, not", class(x))
  )
}







# checkmate re-exports ####
# nocov start

#' @importFrom checkmate assert_access
#' @importFrom checkmate assert_access
#' @importFrom checkmate assert_array
#' @importFrom checkmate assert_atomic
#' @importFrom checkmate assert_atomic_vector
#' @importFrom checkmate assert_character
#' @importFrom checkmate assert_choice
#' @importFrom checkmate assert_class
#' @importFrom checkmate assert_complex
#' @importFrom checkmate assert_count
#' @importFrom checkmate assert_data_frame
#' @importFrom checkmate assert_data_table
#' @importFrom checkmate assert_date
#' @importFrom checkmate assert_directory
#' @importFrom checkmate assert_directory_exists
#' @importFrom checkmate assert_disjunct
#' @importFrom checkmate assert_double
#' @importFrom checkmate assert_environment
#' @importFrom checkmate assert_factor
#' @importFrom checkmate assert_false
# #' @importFrom checkmate assert_file
#' @importFrom checkmate assert_file_exists
#' @importFrom checkmate assert_flag
#' @importFrom checkmate assert_formula
#' @importFrom checkmate assert_function
#' @importFrom checkmate assert_int
#' @importFrom checkmate assert_integer
#' @importFrom checkmate assert_integerish
#' @importFrom checkmate assert_list
#' @importFrom checkmate assert_logical
#' @importFrom checkmate assert_matrix
#' @importFrom checkmate assert_multi_class
#' @importFrom checkmate assert_named
#' @importFrom checkmate assert_names
#' @importFrom checkmate assert_null
#' @importFrom checkmate assert_number
# #' @importFrom checkmate assert_numeric
#' @importFrom checkmate assert_os
#' @importFrom checkmate assert_path_for_output
#' @importFrom checkmate assert_permutation
#' @importFrom checkmate assert_posixct
#' @importFrom checkmate assert_r6
#' @importFrom checkmate assert_raw
#' @importFrom checkmate assert_scalar
#' @importFrom checkmate assert_scalar_na
#' @importFrom checkmate assert_set_equal
#' @importFrom checkmate assert_string
#' @importFrom checkmate assert_subset
#' @importFrom checkmate assert_tibble
#' @importFrom checkmate assert_true
#' @importFrom checkmate assert_vector
#' @family assertions
#' @family checkmate
#' @export
checkmate::assert_access
#' @export
checkmate::assert_array
#' @export
checkmate::assert_atomic
#' @export
checkmate::assert_atomic_vector
#' @export
checkmate::assert_character
#' @export
checkmate::assert_choice
#' @export
checkmate::assert_class
#' @export
checkmate::assert_complex
#' @export
checkmate::assert_count
#' @export
checkmate::assert_data_frame
#' @export
checkmate::assert_data_table
#' @export
checkmate::assert_date
#' @export
checkmate::assert_directory
#' @export
checkmate::assert_directory_exists
#' @export
checkmate::assert_disjunct
#' @export
checkmate::assert_double
#' @export
checkmate::assert_environment
#' @export
checkmate::assert_factor
#' @export
checkmate::assert_false
# #' @export
# checkmate::assert_file
#' @export
checkmate::assert_file_exists
#' @export
checkmate::assert_flag
#' @export
checkmate::assert_formula
#' @export
checkmate::assert_function
#' @export
checkmate::assert_int
#' @export
checkmate::assert_integer
#' @export
checkmate::assert_integerish
#' @export
checkmate::assert_list
#' @export
checkmate::assert_logical
#' @export
checkmate::assert_matrix
#' @export
checkmate::assert_multi_class
#' @export
checkmate::assert_named
#' @export
checkmate::assert_names
#' @export
checkmate::assert_null
#' @export
checkmate::assert_number
# #' @export
# checkmate::assert_numeric
#' @export
checkmate::assert_os
#' @export
checkmate::assert_path_for_output
#' @export
checkmate::assert_permutation
#' @export
checkmate::assert_posixct
#' @export
checkmate::assert_r6
#' @export
checkmate::assert_raw
#' @export
checkmate::assert_scalar
#' @export
checkmate::assert_scalar_na
#' @export
checkmate::assert_set_equal
#' @export
checkmate::assert_string
#' @export
checkmate::assert_subset
#' @export
checkmate::assert_tibble
#' @export
checkmate::assert_true
#' @export
checkmate::assert_vector

# nocov end
