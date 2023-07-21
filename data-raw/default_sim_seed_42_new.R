# Old default inputs (as used for earlier default_sim_seed_42), with new betas
#  proportionately scaled, and R_t_init and extirp scaled also.

# TODO - see ...fit.R which also saves default_sim_seed_42_new, should get rid
# of that I think.

# Also fitting it manually here to then test sim_and_fit() and sim_and_fit_realisations()

set.seed(42)

default_sim_seed_42_new <- salmon_sim(alpha = 0.8,
                                      beta = c(0.8, 0.2, 0.1, 0.1)/1.2,
                                      p_prime = c(0.01, 0.98, 0.01),
                                      rho = 0.6,
                                      omega = 0.8,
                                      sigma_nu = 0.75,
                                      phi_1 = 0.1,
                                      T = 100,
                                      # h_t = rep(0.2, T), # in case T is globally defined
                                      R_t_init = c(0.6, 0.1, 0.1, 0.1, 0.6, 0.1,
                                                   0.1, 0.1) * 1.2,
                                      extirp = 2e-6 * 1.2)

# If change this again then see the tests in test-salmon_sim.R for things to check
default_sim_seed_42_new_fit <- pbsEDM::pbsEDM(default_sim_seed_42_new,
                                              lags = list(R_t = 0,
                                                          S_t = 0:3),
                                              first_difference = TRUE,
                                              centre_and_scale = FALSE)

default_sim_seed_42_new_fit_realisations <- sim_and_fit_realisations(
  salmon_sim_args =
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
                     centre_and_scale = FALSE),
  M = 42)   # so the last row results should match default_sim_seed_42_new_fit results

HERE - next test is failing with centre_and_scale = TRUE above, but not when
FALSE. Does not fail when change to S_t = 0:4 or first_difference = FALSE, so the arguments are getting
passed okay).


  testthat::expect_equal(
            dplyr::as_tibble(cbind(m = 42,
                                   R_T_sim = default_sim_seed_42_new_fit$N_observed[100],
                                   R_T_edm_fit = default_sim_seed_42_new_fit$N_forecast[100],
                                   default_sim_seed_42_new_fit$results)),
                             dplyr::filter(default_sim_seed_42_new_fit_realisations,
                                           m == 42))


Error: dplyr::as_tibble(...) not equal to dplyr::filter(...).
Component "N_rho": Mean relative difference: 1.905788e-05
Component "N_rmse": Mean relative difference: 6.076536e-05
Component "X_rho": Mean relative difference: 7.591083e-05
Component "X_rmse": Mean relative difference: 0.005152032

  but not when
FALSE (so arguments are getting passed okay). Had thought it does not fail when
change to S_t = 0:4 , but it actually does seems to fail (but mostly not as badly) with S_t=0:4:

Error: dplyr::as_tibble(...) not equal to dplyr::filter(...).
Component "N_rho": Mean relative difference: 2.070968e-06
Component "N_rmse": Mean relative difference: 3.792039e-06
Component "X_rho": Mean relative difference: 0.0004112734
Component "X_rmse": Mean relative difference: 0.005095349

Could be to do with the simulation going to 0, and exactly how that gets dealt
with. Probably worth changing that anyway.

stop("don't go further until figure out above problem")
usethis::use_data(default_sim_seed_42_new,
                  overwrite = TRUE)

usethis::use_data(default_sim_seed_42_new_fit,
                  overwrite = TRUE)
