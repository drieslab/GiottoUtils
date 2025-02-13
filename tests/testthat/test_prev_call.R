test_that("get_args_list works", {
    a <- function(x = 1, y = 2, ...) {
        get_args_list(...)
    }

    b <- a(z = 3, keep = "y")

    expect_identical(b, list(y = 2, z = 3))
})

foo <- function(a, b) get_args(toplevel = 1)
bar <- function() get_args() # default toplevel = 2
baz <- function(x, y) bar()

test_that("get_args() works", {
    out <- foo(a = 1, b = 2)
    expect_identical(out, c(a = "1", b = "2"))
    out <- baz("a", "b")
    expect_identical(out, c(x = "a", y = "b"))
})

rm(foo)
rm(bar)
rm(baz)