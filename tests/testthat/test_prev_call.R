test_that("get_args_list works", {
    a <- function(x = 1, y = 2, ...) {
        get_args_list(...)
    }

    b <- a(z = 3, keep = "y")

    expect_identical(b, list(y = 2, z = 3))
})

test_that("get_args() works", {
    foo <- function(a, b) get_args(toplevel = 1)
    out <- foo(a = 1, b = 2)
    expect_identical(out, c(a = "1", b = "2"))

    bar <- function() get_args() # default toplevel = 2
    baz <- function(x, y) bar()
    out <- baz("a", "b")
    expect_identical(out, c(x = "a", y = "b"))
})
