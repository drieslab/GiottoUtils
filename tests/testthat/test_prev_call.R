

test_that("get_args_list works", {
    a <- function(x = 1, y = 2, ...) {
        get_args_list(...)
    }

    b <- a(z = 3, keep = "y")
    
    expect_identical(b, list(y = 2, z = 3))
})
