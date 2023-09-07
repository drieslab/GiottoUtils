test_that("depth returns the correct depth", {
  # Test on empty list
  expected <- 0L
  actual <- depth(list())
  expect_equal(actual, expected)

  # Test on data frame
  expected <- 0L
  actual <- depth(data.frame())
  expect_equal(actual, expected)

  # Test on list of lists
  expected <- 1L
  actual <- depth(list(list(), list()))
  expect_equal(actual, expected)

  # Test on list of data frames
  expected <- 1L
  actual <- depth(list(data.frame(), data.frame()))
  expect_equal(actual, expected)

  # Test on nested list of data frames
  expected <- 2L
  actual <- depth(list(list(data.frame()), list(data.frame())))
  expect_equal(actual, expected)
})



