x <- list(x = 1, y = 2)

test_that("lapply functions produce similar values", {
    # suppress sequential warnings
    options("giotto.warn_sequential" = FALSE)
    resbase <- lapply(x, log)
    resdefault <- lapply_flex(x, log)
    resfuture <- lapply_flex(x, log, method = "future")
    resbioc <- lapply_flex(x, log, method = "biocparallel")
    
    expect_identical(resbase, resdefault)
    expect_identical(resbase, resfuture)
    expect_identical(resbase, resbioc)
})

options("giotto.warn_sequential" = TRUE) 

test_that("future warns sequential", {
    expect_warning(
        lapply_flex(x, log, method = "future"), regexp = "future"
    )
})

test_that("bioc warns sequential", {
    expect_warning(
        lapply_flex(x, log, method = "biocparallel"), regexp = "BiocParallel"
    )
})
