context("sim_and_fit.R")

test_that("sim_and_fit() runs and gives correct answer for simulation with some old original default inputs", {
  set.seed(42)
  default_sim_seed_42_new_create_in_test <-
    sim_and_fit(salmon_sim_args =
                  list(alpha = 0.8,
                       beta = c(0.8, 0.2, 0.1, 0.1)/1.2,
                       p_prime = c(0.01, 0.98, 0.01),
                       rho = 0.6,
                       omega = 0.8,
                       sigma_nu = 0.75,
                       phi_1 = 0.1,
                       T = 100,
                       R_t_init = c(0.6, 0.1, 0.1, 0.1, 0.6, 0.1,
                                    0.1, 0.1) * 1.2,
                       extirp = 2e-6 * 1.2),
                pbsEDM_args = list(lags = list(R_prime_t = 0,
                                               S_t = 0:3),
                                   first_difference = TRUE))

  expect_equal(default_sim_seed_42_new_create_in_test$simulated,
               default_sim_seed_42_new)

  expect_equal(default_sim_seed_42_new_create_in_test$fit,
               default_sim_seed_42_new_fit)

  expect_error(sim_and_fit(salmon_sim_args = 0.8,   # not a list but should be
                           pbsEDM_args = list(lags = list(R_prime_t = 0,
                                                          S_t = 0:3))))

  expect_error(sim_and_fit(pbsEDM_args = 7))  # lags is not a list (nor is pbsEDM_args)
})
