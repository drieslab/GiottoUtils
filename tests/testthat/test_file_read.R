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
