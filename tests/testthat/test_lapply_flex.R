x <- list(x = 1, y = 2)

options("giotto.warn_sequential" = FALSE)

test_that("lapply functions produce similar values", {
    # suppress sequential warnings
    resbase <- lapply(x, log)
    resdefault <- lapply_flex(x, log)
    resfuture <- lapply_flex(x, log, method = "future")
    resbioc <- lapply_flex(x, log, method = "biocparallel")

    expect_identical(resbase, resdefault)
    expect_identical(resbase, resfuture)
    expect_identical(resbase, resbioc)
})

test_that("bpparam is serial by default", {
    checkmate::expect_class(giotto_bpparam(), "SerialParam")
})

test_that("bpparam can be set", {
    res <- giotto_bpparam(BiocParallel::SnowParam())
    checkmate::expect_class(res, "SnowParam")
    checkmate::expect_class(giotto_bpparam(), "SnowParam")
})

giotto_bpparam(BiocParallel::SerialParam()) # reset
options("giotto.warn_sequential" = TRUE)

test_that("future warns sequential", {
    expect_warning(
        lapply_flex(x, log, method = "future"),
        regexp = "future"
    )
})

test_that("bioc warns sequential", {
    expect_warning(
        lapply_flex(x, log, method = "biocparallel"),
        regexp = "BiocParallel"
    )
})
