test_that('box_char provides list of 8 values with expected names', {
  x = box_chars()
  expect_list(x,
              types = 'character',
              len = 8L,
              names = 'named')
  expect_identical(names(x),
                   c("h", "v", "l", "j", "b", "t", "i", "s"))
})

test_that('box_char provides alternatives when not utf8', {
  opt1 = getOption('cli.unicode', default = NULL)
  options('cli.unicode' = NULL)
  opt2 = getOption("giotto.unicode", default = NULL)
  options(giotto.unicode = TRUE)
  b1 = box_chars()
  options(giotto.unicode = FALSE)
  b2 = box_chars()

  expect_list(b1, types = 'character', len = 8L, names = 'named')
  expect_list(b2, types = 'character', len = 8L, names = 'named')

  expect_identical(names(b1), c("h", "v", "l", "j", "b", "t", "i", "s"))
  expect_identical(names(b2), c("h", "v", "l", "j", "b", "t", "i", "s"))

  expect_false(identical(b1, b2))

  on.exit({
    options(cli.unicode = opt1)
    options(giotto.unicode = opt2)
  })
})

test_that('is_utf8_output returns FALSE with cli settings', {
  opt1 = getOption('cli.unicode', default = NULL)
  opt2 = getOption('giotto.unicode', default = NULL)
  options('cli.unicode' = FALSE)
  options('giotto.unicode' = NULL)
  expect_false(is_utf8_output())

  on.exit({
    options('cli.unicode' = opt1)
    options('giotto.unicode' = opt2)
  })
})

test_that('is_utf8_output returns FALSE with giotto settings', {
  opt1 = getOption('cli.unicode', default = NULL)
  opt2 = getOption('giotto.unicode', default = NULL)
  options('cli.unicode' = NULL)
  options('giotto.unicode' = FALSE)
  expect_false(is_utf8_output())

  on.exit({
    options('cli.unicode' = opt1)
    options('giotto.unicode' = opt2)
  })
})
