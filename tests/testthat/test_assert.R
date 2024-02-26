test_that("assert framework - DT", {
    expect_no_condition(assert_dt(data.table::data.table()))

    test_char <- "not a data.table"
    foo <- function(input = test_char) {
        assert_dt(x = input)
    }
    expect_error(foo(), regexp = "input' must be of class data.table, not character")
})

test_that("assert framework - numeric", {
    expect_no_condition(assert_numeric(7L))

    test_char <- "not a numeric"
    foo <- function(input = test_char) {
        assert_numeric(x = input)
    }
    expect_error(foo(), regexp = "input' must be of class numeric, not character")
})

test_that("assert framework - file", {
    expect_no_condition(assert_file(tempdir()))

    test_num <- 10
    test_char <- "not a path"
    foo1 <- function(input = test_num) assert_file(x = input)
    foo2 <- function(input = test_char) assert_file(x = input)
    expect_error(foo1(), regexp = "'input' must be a character vector filepath")
    expect_error(foo2(), regexp = "'input' is not an existing file")
})

test_that("assert framework no .name", {
    bad_input <- FALSE
    foo1 <- function(input = bad_input) {
        g_assert(
            x = input,
            test = isTRUE(input),
            msg = c("error should not return param name"),
            n = 1L
        )
    }
    foo1 <- function(input = bad_input) {
        g_assert(
            x = input,
            test = isTRUE(input),
            msg = c("error should return param name", .name),
            n = 1L
        )
    }
    expect_error(foo1(), regexp = "error should return param name 'bad_input'")
})

test_that("giotto object check", {
    test_num <- 7L
    expect_error(assert_giotto(test_num))

    expect_error(assert_giotto(),
        regexp = "giotto object must be given"
    )
})
