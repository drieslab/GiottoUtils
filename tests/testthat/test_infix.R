test_that("%null% works", {
    expect_identical(1 %null% 2, 1)
    expect_identical(NULL %null% 1, 1)
})


test_that("%none% works", {
    expect_identical("a" %none% 20, "a")
    expect_identical(character() %none% 20, 20)
    expect_identical(list() %none% 10, 10)
})

test_that("%na% works", {
    expect_identical(1 %na% 2, 1)
    expect_identical(NA %na% 1, 1)
})


# set_if infix operations can also return. These checks are here to ensure
# that stays the case.

test_that("%null% can return", {
    f <- function(x = NULL) {
        x %null% return(TRUE)
        FALSE
    }
    expect_true(f())
    expect_false(f(1))
})


test_that("%none% can return", {
    f <- function(x = integer(0L)) {
        x %none% return(TRUE)
        FALSE
    }
    expect_true(f())
    expect_false(f(1L))
})

test_that("%na% can return", {
    f <- function(x = NA) {
        x %na% return(TRUE)
        FALSE
    }
    expect_true(f())
    expect_false(f(1))
})
