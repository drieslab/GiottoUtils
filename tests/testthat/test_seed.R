test_that("random_seed works with no error", {
    expect_no_condition(
        random_seed(set.seed = TRUE)
    )
})

test_that("random_seed is random", {
    a <- random_seed(set.seed = FALSE)
    b <- random_seed(set.seed = FALSE)

    expect_true(a != b)
})



test_that("local_seed() works with existing seed", {
  f <- function() {
    local_seed(1234)
    r_val <- rnorm(1)
    return(r_val)
  }

  rnorm(1) # make sure a seed exists

  seed1 <- .Random.seed
  x <- f()
  seed2 <- .Random.seed
  y <- rnorm(1)
  seed3 <- .Random.seed
  z <- f()

  expect_identical(seed1, seed2)
  expect_false(identical(seed1, seed3))

  expect_identical(x, z)
  expect_false(identical(x, y))
})


test_that("local_seed() works with no existing seed", {
  f <- function() {
    local_seed(1234)
    r_val <- rnorm(1)
    return(r_val)
  }

  rm(".Random.seed", envir = globalenv())

  exist_seed1 <- exists(".Random.seed")
  x <- f()
  exist_seed2 <- exists(".Random.seed")
  y <- rnorm(1)
  seed3 <- .Random.seed
  z <- f()

  expect_false(exist_seed1)
  expect_false(exist_seed2)
  checkmate::expect_numeric(seed3)

  expect_identical(x, z)
  expect_false(identical(x, y))
})






