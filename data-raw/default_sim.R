# Default simulation for fitting and testing.

set.seed(42)

default_sim <- salmon_sim()

default_sim_to_fit <- default_sim
R_prime_T <- default_sim_to_fit$"R_prime_t"[nrow(default_sim_to_fit)]
default_sim_to_fit[nrow(default_sim_to_fit), "R_prime_t"] = NA    # Ensure no
                                        # knowledge of it for pbsEDM(), so it
                                        # doesn't affect the rho values (else it does)

# If change this again then see the tests in test-salmon_sim.R for things to check
default_sim_fit <- pbsEDM::pbsEDM(default_sim_to_fit,
                                  lags = list(R_prime_t = 0,
                                              S_t = 0:3),
                                  first_difference = TRUE,
                                  centre_and_scale = TRUE)

testthat::expect_equal(default_sim_to_fit$R_prime_t,
                       default_sim_fit$N_observed[-length(default_sim_fit$N_observed)])

# So remove the final value and put back in the observed T value, which we want
# to compare to the simulated
default_sim_fit$N_observed <-
  default_sim_fit$N_observed[-length(default_sim_fit$N_observed)]
default_sim_fit$N_observed[length(default_sim_fit$N_observed)] <- R_prime_T



default_sim_fit_realisations <- sim_and_fit_realisations(M = 42)
     # so the last row results should match default_sim_fit results

testthat::expect_equal(
            dplyr::as_tibble(cbind(m = 42,
                                   R_prime_T_sim =
                                     default_sim$R_prime_t[nrow(default_sim)],
                                   # that wasn't input so can't save from fit
            R_prime_T_edm_fit = default_sim_fit$N_forecast[80],
                                   default_sim_fit$results)),
            dplyr::filter(default_sim_fit_realisations,
                          m == 42))

usethis::use_data(default_sim,
                  overwrite = TRUE)

usethis::use_data(default_sim_fit,
                  overwrite = TRUE)

usethis::use_data(default_sim_fit_realisations,
                  overwrite = TRUE)
