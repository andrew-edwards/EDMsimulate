# Old default inputs (as used for earlier default_sim_seed_42), with new betas
#  proportionately scaled, and R_t_init and extirp scaled also.

# Also fitting it manually here to then test sim_and_fit() and sim_and_fit_realisations()

# These ended up not working due to different defaults, but now we've updated
# default parameters so creating new data objects for testing just based on
# defaults. Keeping this around for reference, though population dies out here
# so not the best test to do.
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

ISSUE was that salmon_sim has default of T = 100 (which I
                                                  didn't want to change as would mess up tests), but ...realisation() has default of T = 80. Doesn't matter for c_and_s = FALSE, but does for TRUE because the extra 20 0's influence the values.

So, this is kind of documented here. Going to redo defaults anyway with Carrie's
updated values, so make them the same, and put in a transient that gets
ignored.

HERE - next test is failing with centre_and_scale = TRUE above, but not when
FALSE. Does not fail when first_difference = FALSE, so the arguments are getting
passed okay.


  testthat::expect_equal(
            dplyr::as_tibble(cbind(m = 42,
                                   R_T_sim = default_sim_seed_42_new_fit$N_observed[100],
                                   R_T_edm_fit = default_sim_seed_42_new_fit$N_forecast[100],
                                   default_sim_seed_42_new_fit$results)),  # not _fit$fit_results??
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
with. Probably worth changing that anyway. Going to use
Carrie's as defaults, but even using current defaults gives differerent results:

fit_and_sim_default_realisation <- sim_and_fit_realisations(pbsEDM_args = list(lags = list(R_t = 0, S_t = 0:3)), M = 42)

Think it's passing the simulated values wrong. Check the epsilon_tg agree with
each approach, putting a browser in when m=42:

fit_and_sim_default_realisation <- sim_and_fit_realisations(pbsEDM_args = list(lags = list(R_t = 0, S_t = 0:3)), M = 42)
when browser appears then save simulated values (then epsilon_tg if needed)

realisation_R_t_42 <- simulated$R_t

Then do this:
set.seed(42)
fit_and_sim_defaults <- sim_and_fit(salmon_sim_args = list(T = 80),
                                    pbsEDM_args = list(lags = list(R_t = 0, S_t = 0:3)))



stop("don't go further until figure out above problem")
usethis::use_data(default_sim_seed_42_new,
                  overwrite = TRUE)

usethis::use_data(default_sim_seed_42_new_fit,
                  overwrite = TRUE)
