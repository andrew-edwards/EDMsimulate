context("sim_and_fit_realisations.R")

test_that("sim_and_fit_realisations() runs and gives correct answer for simulation with some old original default inputs", {

  default_sim_seed_42_new_fit_realisations_create_in_test <-
    sim_and_fit_realisations(salmon_sim_args =
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
                            pbsEDM_args = list(lags = list(R_t = 0,
                                                           S_t = 0:3),
                                               first_difference = TRUE,
                                               centre_and_scale = TRUE),
                            M = 42)     # to correspond with usual seed = 42


  expect_equal(dplyr::as_tibble(cbind(
                        m = 42,
                        R_T_sim = default_sim_seed_42_new_fit$N_observed[100],
                        R_T_edm_fit =
                          default_sim_seed_42_new_fit$N_forecast[100],
                        default_sim_seed_42_new_fit$results)),
               dplyr::filter(default_sim_seed_42_new_fit_realisations_create_in_test, m==42))

  expect_error(sim_and_fit_realisations(pbsEDM_args = list(
                                          lags = list(R_prime_t = 0,
                                                      S_t = 0:3))))
                                                      # Should error since not R_t
})
