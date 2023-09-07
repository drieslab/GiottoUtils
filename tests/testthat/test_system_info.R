
test_that("get_os returns the correct operating system", {
  # Test on Windows
  if (.Platform$OS.type == "windows") {
    expected <- "windows"
    actual <- get_os()
    expect_equal(actual, expected)
  }

  # Test on OSX
  if (.Platform$OS.type == "unix" && grepl("^darwin", R.version$os)) {
    expected <- "osx"
    actual <- get_os()
    expect_equal(actual, expected)
  }

  # Test on Linux
  if (.Platform$OS.type == "unix" && grepl("linux-gnu", R.version$os)) {
    expected <- "linux"
    actual <- get_os()
    expect_equal(actual, expected)
  }
})
