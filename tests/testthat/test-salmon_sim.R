context("salmon_sim.R")

test_that("salmon_sim() gives correct answer with original default inputs", {
          set.seed(42)
          expect_equal(salmon_sim(alpha = 0.8,
                                  beta = c(0.8, 0.2, 0.1, 0.1),
                                  p_prime = c(0.01, 0.98, 0.01),
                                  rho = 0.6,
                                  omega = 0.8,
                                  sigma_nu = 0.75,
                                  phi_1 = 0.1,
                                  T = 100,
                                  # h_t = rep(0.2, T), # in case T is globally defined
                                  R_t_init = c(0.6, 0.1, 0.1, 0.1, 0.6, 0.1, 0.1, 0.1)),
                       default_sim_seed_42)
})

test_that("salmon_sim() gives error if any non-numeric inputs", {
          expect_error(salmon_sim(T = "a"))
          expect_error(salmon_sim(h_t = "a"))
})

test_that("salmon_sim() gives error if deterministic not TRUE or FALSE", {
          expect_error(salmon_sim(deterministic = "a"))
          expect_error(salmon_sim(deterministic = c(TRUE, FALSE)))
          expect_error(salmon_sim(deterministic = NA))
})


test_that("salmon_sim() gives error if length(h_t) != T (if h_t not scalar), T too small, h_t too big ", {
          expect_error(salmon_sim(T = 50,
                                  h_t = rep(0.2, 49)))
          expect_error(salmon_sim(T = 7))
          expect_error(salmon_sim(h_t = 1))
          expect_silent(salmon_run(h_t = 0.5))
})

test_that("salmon_sim() gives error if length(R_t_init) != 8 or has no positive values", {
          expect_error(salmon_sim(R_t_init = 1:7))
          expect_error(salmon_sim(R_t_init = rep(0,8)))
          expect_silent(salmon_sim(R_t_init = c(0.5, rep(0, 7))))
})

test_that("salmon_sim() gives error if sum(p_prime) != 1", {
          expect_error(salmon_sim(p_prime = c(0.1, 0.1, 0.1)))
})

test_that("salmon_sim() gives error if all parameters not >= 0", {
          expect_error(salmon_sim(alpha = -1))
})

test_that("salmon_sim() gives error if alpha = 0", {
          expect_error(salmon_sim(alpha = 0))
})

test_that("salmon_sim() gives error if scalar inputs are vectors instead", {
          expect_error(salmon_sim(alpha = c(0.2, 0.5)))
})

test_that("salmon_sim() gives error if beta wrong length", {
          expect_error(salmon_sim(beta = c(1, 2, 3)))
})

test_that("salmon_sim() gives correct steady state for deterministic run", {
  # Use defaults
  h_star <- 0.2
  alpha <- 0.8
  beta_vec <- c(0.8, 0.2, 0.1, 0.1)
  T <- 1000
  R_star <- max((log(alpha) + log(1 - h_star)) / (sum(beta_vec) * (1 - h_star)),
                0)         # Don't care about a -ve steady state
  R_numeric_tail <- salmon_sim(alpha = alpha,
                               h_t = h_star,
                               beta = beta_vec,
                               T = T,
                               deterministic = TRUE
                               )$R_t[(T-9):T]
  expect_equal(min(R_numeric_tail), R_star)
  expect_equal(max(R_numeric_tail), R_star)
})

test_that("salmon_sim() results in R^* = 0 steady state if alpha < 1/(1-h_star)", {
  # Use defaults
  h_star <- 0.2
  alpha <- 0.8
  T <- 1000
  R_numeric_tail <- salmon_sim(alpha = alpha,
                               h_t = h_star,
                               T = T,
                               deterministic = TRUE
                               )$R_t[(T-9):T]
  expect_equal(min(R_numeric_tail), 0)
  expect_equal(max(R_numeric_tail), 0)
})



test_that("plot_sim() can create a plot for default run (no seed set)", {
          expect_silent(salmon_run())
          expect_silent(salmon_run(new_plot=FALSE))
          dev.off()
})
