context("test utils.R functions")

test_that("shift functions do as expected",{
  expect_equal(shift(1:5, -2),
               c(3, 4, 5, NA, NA))
  expect_equal(shift(1:5, 2),
               c(NA, NA, 1, 2, 3))
  expect_equal(shift(1:5, 0),
               c(1, 2, 3, 4, 5))
})
