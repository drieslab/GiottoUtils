# Test for a matching argument
test_that("g_match_arg returns the matching argument", {
    choices <- c("apple", "banana", "cherry")
    result <- g_match_arg("banana", choices)
    expect_equal(result, "banana")
})

# Test for a non-matching argument
test_that("g_match_arg throws an error for a non-matching argument", {
    choices <- c("apple", "banana", "cherry")
    expect_error(
        g_match_arg("grape", choices),
        "'arg' should be one of \"apple\", \"banana\", \"cherry\""
    )
})

# Test for a matching argument with custom TRUE value
test_that("g_match_arg returns the matching argument with custom TRUE value", {
    choices <- c("cat", "dog", "elephant")
    result <- g_match_arg("dog", choices, value = TRUE)
    expect_equal(result, "dog")
})

# Test for a non-matching argument with custom TRUE value
test_that("g_match_arg throws an error for a non-matching argument with custom TRUE value", {
    choices <- c("cat", "dog", "elephant")
    expect_error(
        g_match_arg("lion", choices, value = TRUE),
        "'arg' should be one of \"cat\", \"dog\", \"elephant\""
    )
})

# Test for a matching argument with custom FALSE value
test_that("g_match_arg returns the matching argument with custom FALSE value", {
    choices <- c("red", "green", "blue")
    result <- g_match_arg("green", choices, value = FALSE)
    expect_equal(result, 2)
})

# Test for a non-matching argument with custom FALSE value
test_that("g_match_arg throws an error for a non-matching argument with custom FALSE value", {
    choices <- c("red", "green", "blue")
    expect_error(
        g_match_arg("yellow", choices, value = FALSE),
        "'arg' should be one of \"red\", \"green\", \"blue\""
    )
})

# Test for a case-insensitive matching argument
test_that("g_match_arg returns the case-insensitive matching argument", {
    choices <- c("one", "two", "three")
    result <- g_match_arg("TWO", choices, ignore.case = TRUE)
    expect_equal(result, "two")
})

# Test for a case-insensitive non-matching argument
test_that("g_match_arg throws an error for a case-insensitive non-matching argument", {
    choices <- c("one", "two", "three")
    expect_error(
        g_match_arg("four", choices, ignore.case = TRUE),
        "'arg' should be one of \"one\", \"two\", \"three\""
    )
})
