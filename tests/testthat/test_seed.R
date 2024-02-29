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


# test to ensure that seed setting is not local to if statement
# and that it will not end the transient seed before `rnorm()`
test_that("local_seed() ends when expected", {
    invisible(rnorm(1)) # set a random seed for convenience

    f1 <- function(seed) {
        if (TRUE) {
            local_seed(seed)
        }

        r_val <- rnorm(1)
        return(r_val)
    }
    f2 <- function(seed) {
        if (FALSE) {
            local_seed(seed)
        }

        r_val <- rnorm(1)
        return(r_val)
    }

    s1 <- .Random.seed
    a <- f1(1)
    b <- f1(1)
    s2 <- .Random.seed

    x <- f2(1)
    s3 <- .Random.seed
    y <- f2(1)
    s4 <- .Random.seed

    expect_identical(a, b)
    expect_identical(s1, s2) # no impact on seed when local_seed is set

    # global random seed and outputs expected to be different when no
    # local_seed is set
    expect_false(identical(x, y))
    expect_false(identical(s2, s3))
    expect_false(identical(s3, s4))
})
