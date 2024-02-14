test_that("distinct: n colors are supplied", {
    expect_no_condition(getDistinctColors(10))
    out444 <- getDistinctColors(444)
    checkmate::expect_character(out444, unique = TRUE, len = 444L)

    expect_warning(getDistinctColors(500), regexp = "not enough unique colors in R")
})

test_that("rainbow: n colors are supplied", {
    expect_no_condition(getRainbowColors(10))
    out10 <- getRainbowColors(10)
    checkmate::expect_character(out10, unique = TRUE, len = 10L)

    checkmate::expect_character(getRainbowColors(100), unique = TRUE)
    checkmate::expect_character(getRainbowColors(101), unique = FALSE)
})

test_that("error thrown when too few colors requested", {
    error_pattern <- "colors wanted must be at least 1"
    expect_error(getRainbowColors(0.5),
        regexp = error_pattern
    )
    expect_error(getDistinctColors(0.5),
        regexp = error_pattern
    )
})
