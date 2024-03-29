

test_that("dt NA removal works", {
    x <- data.table::data.table(x = 1:3, y = c(1,NA,2))
    dt_remove_na(x)
    y <- data.table::data.table(x = 1:3, y = c(1,0,2))
    expect_identical(x, y)
})

test_that("sort combine for dt cols works", {
    x = data.table::data.table(x = 1:3, y = 5:7)
    dt_sort_combine_two_columns(x, column1 = "x", column2 = "y")
    
    y <- c("1--5", "2--6", "3--7")
    expect_identical(x$unif_gene_gene, y)
})

test_that("dt to matrix conversion works", {
    x <- data.table::data.table(x = c("a","b","c"), y = 1:3, z = 5:7)
    y1 <- matrix(c(1:3, 5:7), nrow = 3, ncol = 2, byrow = FALSE,
                 dimnames = list(letters[1:3], c("y", "z")))
    y2 <- dt_to_matrix(x)
    expect_true(all(y1 == y2))
    expect_identical(dim(y1), dim(y2))
})

test_that("dcast workaround for character formula inputs works", {
    x <- data.table::data.table(
        col1 = c(rep("a", 3), rep("b", 3)),
        col2 = rep(LETTERS[1:3], 2),
        value = c(1:6)
    )
    x <- dt_dcast_string(x, "col1", "col2", "value")
    
    y <- data.table::data.table(
        col1 = letters[1:2],
        A = c(1, 4),
        B = c(2, 5),
        C = c(3, 6)
    )
    data.table::setkeyv(y, "col1")
    expect_equal(x, y)
})

test_that("specific order setting works for dt", {
    x <- data.table::data.table(x = c("a","b","c"), y = 1:3, z = 5:7)
    dt_set_row_order(x, neworder = c(1,3,2))
    
    y <- data.table::data.table(
        x = c(letters[c(1,3,2)]),
        y = c(1L,3L,2L),
        z = c(5L,7L,6L)
    ) 
    
    expect_identical(x, y)
})
