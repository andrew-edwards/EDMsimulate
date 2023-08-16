context("sim_and_fit_realisations.R")

test_that("sim_and_fit_realisations() runs and gives correct answer for simulation with default inputs", {

  ## default_sim_fit_realisations_create_in_test <-
  ##   sim_and_fit_realisations(M = 42)

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
