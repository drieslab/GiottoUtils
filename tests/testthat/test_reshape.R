set.seed(9)
Data <- matrix(round(rnorm(12, 10, 4)), nrow = 4, ncol = 3)



test_that("test matrix melting - defaults", {
    res <- melt_matrix(Data)
    expect_data_table(res)
    expect_integer(res$Var1)
    expect_integer(res$Var2)
})

test_that("test matrix melting with colnames", {
    colnames(Data) <- seq_len(3)
    rownames(Data) <- LETTERS[seq_len(4)]

    res <- melt_matrix(Data)
    expect_data_table(res)
    expect_integer(res$Var2)
    expect_factor(res$Var1)
})
