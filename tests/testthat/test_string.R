fruit <- c("apple", "banana", "pear", "pineapple")

# copied from stringr::str_locate() outputs
expect_1 <- matrix(
  c(6L, 7L, 5L, 10L, 5L, 6L, 4L, 9L),
  ncol = 2,
  dimnames = list(NULL, c("start", "end"))
)
expect_2 <- matrix(
  c(1L, 2L, 3L, 5L, 1L, 2L, 3L, 5L),
  ncol = 2,
  dimnames = list(NULL, c("start", "end"))
)
expect_3 <- matrix(
  c(5L, NA_integer_, 2L, 4L, 5L, NA_integer_, 2L, 4L),
  ncol = 2,
  dimnames = list(NULL, c("start", "end"))
)
expect_4 <- matrix(
  c(1L, 1L, 1L, 4L, 1L, 1L, 1L, 4L),
  ncol = 2,
  dimnames = list(NULL, c("start", "end"))
)

test_that("string_locate2 works like stringr outputs", {
  expect_identical(expect_1, str_locate2(fruit, "$"))
  expect_identical(expect_2, str_locate2(fruit, "a"))
  expect_identical(expect_3, str_locate2(fruit, "e"))
  expect_identical(expect_4, str_locate2(fruit, c("a", "b", "p", "e")))
})
