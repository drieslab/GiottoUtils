# Test for an empty character vector
test_that("is_empty_char returns TRUE for an empty character vector", {
  result <- is_empty_char(character(0))
  expect_true(result)
})

# Test for a NULL input
test_that("is_empty_char returns TRUE for a NULL input", {
  result <- is_empty_char(NULL)
  expect_true(result)
})

# Test for a NA input
test_that("is_empty_char returns TRUE for a NA input", {
  result <- is_empty_char(NA)
  expect_true(result)
})

# Test for an empty character
test_that("is_empty_char returns TRUE for an empty character", {
  result <- is_empty_char("")
  expect_true(result)
})

# Test for a non-empty character
test_that("is_empty_char returns FALSE for a non-empty character", {
  result <- is_empty_char("Hello")
  expect_false(result)
})

# Test for a numeric input
test_that("is_empty_char returns FALSE for a numeric input", {
  result <- is_empty_char(123)
  expect_false(result)
})

# Test for a logical input
test_that("is_empty_char returns FALSE for a logical input", {
  result <- is_empty_char(TRUE)
  expect_false(result)
})

# Test for a character vector with non-empty elements
test_that("is_empty_char returns FALSE for a character vector with non-empty elements", {
  result <- is_empty_char(c("a", "b", "c"))
  expect_false(result)
})

# Test for a character vector with empty elements
test_that("is_empty_char returns TRUE for a character vector with empty elements", {
  result <- is_empty_char(c("", "", ""))
  expect_out <- rep(TRUE, 3L)
  names(expect_out) <- c("", "", "")
  expect_identical(result, expect_out)
})


# Test for an existing element in the list
test_that("list_element_exists returns TRUE for an existing element", {
  my_list <- list(a = 1, b = "hello", c = TRUE)

  result <- list_element_exists(my_list, "a")
  expect_true(result)
})

# Test for a non-existing element in the list
# test_that("list_element_exists returns FALSE for a non-existing element", {
#   my_list <- list(a = 1, b = "hello", c = TRUE)
#
#   result <- list_element_exists(my_list, "d")
#   expect_false(result)
# })

# Test for an existing element with a NULL value
test_that("list_element_exists returns TRUE for an existing element with a NULL value", {
  my_list <- list(a = NULL, b = "hello", c = TRUE)

  result <- list_element_exists(my_list, "a")
  expect_true(result)
})

# Test for an existing element with an empty list value
test_that("list_element_exists returns TRUE for an existing element with an empty list value", {
  my_list <- list(a = list(), b = "hello", c = TRUE)

  result <- list_element_exists(my_list, "a")
  expect_true(result)
})

# Test for an existing element with a numeric value
test_that("list_element_exists returns TRUE for an existing element with a numeric value", {
  my_list <- list(a = 123, b = "hello", c = TRUE)

  result <- list_element_exists(my_list, "a")
  expect_true(result)
})

# Test for an existing element with a logical value
test_that("list_element_exists returns TRUE for an existing element with a logical value", {
  my_list <- list(a = TRUE, b = "hello", c = TRUE)

  result <- list_element_exists(my_list, "a")
  expect_true(result)
})

# Test for an existing element with a character value
test_that("list_element_exists returns TRUE for an existing element with a character value", {
  my_list <- list(a = "world", b = "hello", c = TRUE)

  result <- list_element_exists(my_list, "a")
  expect_true(result)
})

# Test for an existing element with a complex value
test_that("list_element_exists returns TRUE for an existing element with a complex value", {
  my_list <- list(a = 1 + 2i, b = "hello", c = TRUE)

  result <- list_element_exists(my_list, "a")
  expect_true(result)
})

# Test for an existing element with a function value
test_that("list_element_exists returns TRUE for an existing element with a function value", {
  my_list <- list(a = function(x) x + 1, b = "hello", c = TRUE)

  result <- list_element_exists(my_list, "a")
  expect_true(result)
})

# Test for an element with NULL index
test_that("list_element_exists returns FALSE for an element with NULL index", {
  my_list <- list(a = 1, b = "hello", c = TRUE)

  result <- list_element_exists(my_list, NULL)
  expect_false(result)
})
