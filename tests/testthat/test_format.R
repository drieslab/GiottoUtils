

test_that('Angle conversions', {
  rads = pi
  degs = 180L

  expect_equal(degrees(rads), degs)
  expect_equal(radians(degs), rads)
})

