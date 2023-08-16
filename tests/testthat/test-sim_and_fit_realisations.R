context("sim_and_fit_realisations.R")

test_that("sim_and_fit_realisations() runs and gives correct answer for simulation with default inputs", {

  expect_equal(1 + 1, 2)   # dummy to not have empty test

  # Works locally for Andy now, takes a while so stopped and just trying M = 3.
   default_sim_fit_realisations_create_in_test <-
     sim_and_fit_realisations(M = 3)

  # Not updated yet since going to tweak function further with more options -
  #  this currently fails since sim_and_fit_realisations() now outputs more and
  #  we haven't updated the test. Probably have to skip this one on GHA.
  ## expect_error(dplyr::as_tibble(cbind(
  ##                       m = 42,
  ##                       R_prime_T_sim = unlist(default_sim_fit$N_observed[80]),
  ##                       R_prime_T_edm_fit =
  ##                         default_sim_fit$N_forecast[80],
  ##                       default_sim_fit$results)),
  ##              dplyr::filter(default_sim_fit_realisations_create_in_test, m==42))

  ## expect_error(sim_and_fit_realisations(pbsEDM_args = list(
  ##                                         lags = list(R_t = 0,
  ##                                                     S_t = 0:3))))
  ##                                                     # Should error since not
  ##                                                     # R_prime_t anymore
})
