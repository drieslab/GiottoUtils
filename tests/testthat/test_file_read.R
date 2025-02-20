test_that("file_extension returns the correct file extension", {
    # Test on file with extension
    expected <- "txt"
    actual <- file_extension("path/to/file.txt")
    expect_equal(actual, expected)

    # Test on file without extension
    expected <- character(0)
    actual <- file_extension("path/to/file")
    expect_equal(actual, expected)

    # Test on file with multiple dots in name
    expected <- c("with", "multiple", "dots", "csv")
    actual <- file_extension("path/to/file.with.multiple.dots.csv")
    expect_equal(actual, expected)
})

x <- data.frame(a = c("a", "b", "c"), b = 1:3, c = 5:7)

test_that("read_colmatch works", {
    f <- file.path(tempdir(), "my_file.csv")
    write.csv(x, f)
    res <- read_colmatch(f, col = "a", values_to_match = c("a", "c"))

    checkmate::expect_data_frame(res, nrows = 2, ncols = 4)

    unlink(f)
})

test_that("read_colmatch works with headerless files", {
    f <- file.path(tempdir(), "my_file.csv")
    names(x) <- NULL
    write.csv(x, f)
    # this will throw a warning from data.table
    res <- suppressWarnings(
        read_colmatch(f, col = "V2", values_to_match = c("a", "b"))
    )

    checkmate::expect_data_frame(res, nrows = 2, ncols = 4)

    unlink(f)
})
