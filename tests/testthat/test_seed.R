
test_that('random_seed works with no error', {
  expect_no_condition(
    random_seed(set.seed = TRUE)
  )
})

test_that('random_seed is random', {
  a = random_seed(set.seed = FALSE)
  b = random_seed(set.seed = FALSE)

  expect_true(a != b)
})
