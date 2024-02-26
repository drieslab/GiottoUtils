# test that the wrapped function works as expected.

test_that("Badges work", {
    t1 <- "stable"
    t2 <- "experimental"
    t3 <- "deprecated"
    t4 <- "superseded"
    expect_identical(
        lifecycle_badge(stage = t1),
        lifecycle::badge(stage = t1)
    )
    expect_identical(
        lifecycle_badge(stage = t2),
        lifecycle::badge(stage = t2)
    )
    expect_identical(
        lifecycle_badge(stage = t3),
        lifecycle::badge(stage = t3)
    )
    expect_identical(
        lifecycle_badge(stage = t4),
        lifecycle::badge(stage = t4)
    )
})
