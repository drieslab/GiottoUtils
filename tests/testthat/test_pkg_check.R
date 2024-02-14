# check_github_suite_ver ####
test_that("Existing package is checked without issue", {
  # with the assumption that no development code on this package will be behind
  # what is available on github
  expect_no_error(check_github_suite_ver("GiottoUtils"))
})

test_that("Non-existing package throws error", {
  expect_error(check_github_suite_ver(
    pkg = "this-does-not-exist",
    regexp = "this-does-not-exist"
  ))
})



# package_check ####
test_that("Existing packages are checked with no conditions", {
  expect_no_condition(package_check("GiottoUtils", repository = "CRAN"))
  expect_no_condition(package_check("GiottoUtils", repository = "Bioc"))
  expect_no_condition(package_check("GiottoUtils", repository = "github", github_repo = "johndoe/cooltool"))
})

test_that("Existing packages return TRUE", {
  expect_true(package_check("GiottoUtils", repository = "CRAN"))
  expect_true(package_check("GiottoUtils", repository = "Bioc"))
  expect_true(package_check("GiottoUtils", repository = "github", github_repo = "johndoe/cooltool"))
})

test_that("Nonexisting optional packages return message with install instructions", {
  mock_pkg <- "I-dont-exist"
  expect_message(package_check(mock_pkg, repository = "CRAN", optional = TRUE),
    regexp = "install.packages"
  )
  expect_message(package_check(mock_pkg, repository = "Bioc", optional = TRUE),
    regexp = "BiocManager::install"
  )
  expect_message(package_check(mock_pkg, repository = "github", github_repo = "fake_repo", optional = TRUE),
    regexp = "devtools::install_github\\(\"fake_repo"
  )
})

test_that("Nonexisting Nonoptional packages throw error with install instructions", {
  mock_pkg <- "I-also-dont-exist"
  expect_error(package_check(mock_pkg, repository = "CRAN", optional = FALSE),
    regexp = "install.packages"
  )
  expect_error(package_check(mock_pkg, repository = "Bioc", optional = FALSE),
    regexp = "BiocManager::install"
  )
  expect_error(package_check(mock_pkg, repository = "github", github_repo = "I_made_it_up", optional = FALSE),
    regexp = "devtools::install_github\\(\"I_made_it_up"
  )
})

test_that("Nonexisting packages with specified custom message will send custom", {
  mock_pkg <- "its_amazing_how_much_I_dont_exist"
  custom <- "I am custom"
  expect_message(package_check(mock_pkg, repository = "CRAN", custom_msg = custom, optional = TRUE),
    regexp = custom
  )
  expect_message(package_check(mock_pkg, repository = "Bioc", custom_msg = custom, optional = TRUE),
    regexp = custom
  )
  expect_message(package_check(mock_pkg, repository = "github", custom_msg = custom, optional = TRUE, github_repo = "still_fake"),
    regexp = custom
  )
})

test_that("Nonexisting nonoptional packages with specified custom message will error custom", {
  mock_pkg <- "ive_never_heard_of_me"
  custom <- "I am custom"
  expect_error(package_check(mock_pkg, repository = "CRAN", custom_msg = custom, optional = FALSE),
    regexp = custom
  )
  expect_error(package_check(mock_pkg, repository = "Bioc", custom_msg = custom, optional = FALSE),
    regexp = custom
  )
  expect_error(package_check(mock_pkg, repository = "github", custom_msg = custom, optional = FALSE, github_repo = "a_fabrication"),
    regexp = custom
  )
})
