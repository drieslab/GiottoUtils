# mock matrix
m <- matrix(data = 0L, nrow = 400, ncol = 300)
m[sample(400, 100), sample(300, 100)] <- runif(100, max = 10)

dgc <- Matrix::rsparsematrix(nrow = 400L, ncol = 300L, density = 0.1)
dgr <- as(dgc, "RsparseMatrix")
dgt <- as(dgc, "TsparseMatrix")

# setup py class checks
expect_scipy_sparse_c <- function(object, ...) {
  expect_s3_class(object, class = "scipy.sparse._csc.csc_matrix")
}
expect_scipy_sparse_r <- function(object, ...) {
  expect_s3_class(object, class = "scipy.sparse._csr.csr_matrix")
}




test_that("matrix can be converted", {
  py_c <- to_scipy_sparse(m, format = "C")
  expect_scipy_sparse_c(py_c)
  expect_true(identical(dim(py_c), c(400L, 300L)))
  py_c_t <- to_scipy_sparse(m, format = "C", transpose = TRUE)
  expect_scipy_sparse_c(py_c_t)
  expect_true(identical(dim(py_c_t), c(300L, 400L)))


  py_r <- to_scipy_sparse(m, format = "R")
  expect_scipy_sparse_r(py_r)
  expect_true(identical(dim(py_r), c(400L, 300L)))
  py_r_t <- to_scipy_sparse(m, format = "R", transpose = TRUE)
  expect_scipy_sparse_r(py_r_t)
  expect_true(identical(dim(py_r_t), c(300L, 400L)))
})


test_that("Matrix C can be converted", {
  py_c <- to_scipy_sparse(dgc, format = "C")
  expect_scipy_sparse_c(py_c)
  expect_true(identical(dim(py_c), c(400L, 300L)))
  py_c_t <- to_scipy_sparse(dgc, format = "C", transpose = TRUE)
  expect_scipy_sparse_c(py_c_t)
  expect_true(identical(dim(py_c_t), c(300L, 400L)))


  py_r <- to_scipy_sparse(dgc, format = "R")
  expect_scipy_sparse_r(py_r)
  expect_true(identical(dim(py_r), c(400L, 300L)))
  py_r_t <- to_scipy_sparse(dgc, format = "R", transpose = TRUE)
  expect_scipy_sparse_r(py_r_t)
  expect_true(identical(dim(py_r_t), c(300L, 400L)))
})

test_that("Matrix C can be converted", {
  py_c <- to_scipy_sparse(dgc, format = "C")
  expect_scipy_sparse_c(py_c)
  expect_true(identical(dim(py_c), c(400L, 300L)))
  py_c_t <- to_scipy_sparse(dgc, format = "C", transpose = TRUE)
  expect_scipy_sparse_c(py_c_t)
  expect_true(identical(dim(py_c_t), c(300L, 400L)))


  py_r <- to_scipy_sparse(dgc, format = "R")
  expect_scipy_sparse_r(py_r)
  expect_true(identical(dim(py_r), c(400L, 300L)))
  py_r_t <- to_scipy_sparse(dgc, format = "R", transpose = TRUE)
  expect_scipy_sparse_r(py_r_t)
  expect_true(identical(dim(py_r_t), c(300L, 400L)))
})

test_that("Matrix R can be converted", {
  py_c <- to_scipy_sparse(dgr, format = "C")
  expect_scipy_sparse_c(py_c)
  expect_true(identical(dim(py_c), c(400L, 300L)))
  py_c_t <- to_scipy_sparse(dgr, format = "C", transpose = TRUE)
  expect_scipy_sparse_c(py_c_t)
  expect_true(identical(dim(py_c_t), c(300L, 400L)))


  py_r <- to_scipy_sparse(dgr, format = "R")
  expect_scipy_sparse_r(py_r)
  expect_true(identical(dim(py_r), c(400L, 300L)))
  py_r_t <- to_scipy_sparse(dgr, format = "R", transpose = TRUE)
  expect_scipy_sparse_r(py_r_t)
  expect_true(identical(dim(py_r_t), c(300L, 400L)))
})


test_that("Matrix C can be converted", {
  py_c <- to_scipy_sparse(dgt, format = "C")
  expect_scipy_sparse_c(py_c)
  expect_true(identical(dim(py_c), c(400L, 300L)))
  py_c_t <- to_scipy_sparse(dgt, format = "C", transpose = TRUE)
  expect_scipy_sparse_c(py_c_t)
  expect_true(identical(dim(py_c_t), c(300L, 400L)))


  py_r <- to_scipy_sparse(dgt, format = "R")
  expect_scipy_sparse_r(py_r)
  expect_true(identical(dim(py_r), c(400L, 300L)))
  py_r_t <- to_scipy_sparse(dgt, format = "R", transpose = TRUE)
  expect_scipy_sparse_r(py_r_t)
  expect_true(identical(dim(py_r_t), c(300L, 400L)))
})
