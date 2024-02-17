

test_that("%null% works", {
  expect_identical(1 %null% 2, 1)
  expect_identical(NULL %null% 1, 1)
})


test_that("%none works", {
  expect_identical("a" %none% 20, "a")
  expect_identical(character() %none% 20, 20)
  expect_identical(list() %none% 10, 10)
})

test_that("%null% works", {
  expect_identical(1 %na% 2, 1)
  expect_identical(NA %na% 1, 1)
})
