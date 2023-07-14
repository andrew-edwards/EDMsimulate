context("salmon_bif.R")

test_that("salmon_bif() gives error if any non-numeric inputs", {
  expect_error(salmon_bif(alpha_vec = "a"))
  expect_error(salmon_bif(last = "a"))
  expect_error(salmon_bif(T = "a"))
})

test_that("salmon_bif() gives error if inputs are invalid", {
  expect_error(salmon_bif(T = c(50, 60)))
  expect_error(salmon_bif(last = c(60, 70)))
  expect_error(salmon_bif(alpha_vec = c(0, 0.1, 0.2)))
  expect_error(salmon_bif(last = 0))
  expect_error(salmon_bif(T = 7))
  expect_error(salmon_bif(T = 50, last = 60))
})

test_that("salmon_bif() returns a matrix of the correct size", {
  x <- salmon_bif(alpha_vec = seq(1, 2, length = 21),
                  last = 10,
                  T = 30)
  expect_equal(dim(x), c(10, 21))
})


test_that("plot_salmon_bif() gives error if any non-numeric inputs", {
  expect_error(plot_salmon_bif(alpha_vec = "a"))
  expect_error(plot_salmon_bif(alpha_vec = 7,
                               x = "a"))
})

test_that("plot_salmon_bif() gives error if inputs are invalid", {
  alpha_vec_test <- seq(1, 2, length = 10)
  x_test <- matrix(1:100, ncol = 10)
  expect_error(plot_salmon_bif(alpha_vec = alpha_vec_test,
                               x = x_test,
                               new_plot = FALSE))   # Need here with no plots,
                                        # will sometimes fail locally but not on
                                        # a clean build on GitHub.
  expect_error(plot_salmon_bif(alpha_vec = alpha_vec_test,
                               x = 1:10))
  expect_error(plot_salmon_bif(alpha_vec = alpha_vec_test,
                               x = x_test,
                               new_plot = 7))
  expect_error(plot_salmon_bif(alpha_vec = alpha_vec_test,
                               x = x_test,
                               new_plot = c(7, 7)))
  expect_error(plot_salmon_bif(alpha_vec = alpha_vec_test,
                               x = x_test[, -1]))
})

test_that("plot_salmon_bif() can create a plot for default values", {
  alpha_vec_test <- seq(1, 2, length = 10)
  x_test <- matrix(1:100, ncol = 10)
  expect_silent(plot_salmon_bif(alpha_vec = alpha_vec_test,
                                x = x_test))
  expect_silent(plot_salmon_bif(alpha_vec = alpha_vec_test,
                                x = x_test,
                                new_plot=FALSE))
  dev.off()
})

test_that("salmon_bif_run() can calculate and plot a default diagram", {
  expect_silent(salmon_bif_run(alpha_vec = seq(1, 2, length = 10),
                               last = 20,
                               T = 30,))
})
