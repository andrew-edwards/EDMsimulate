set.seed(42)
# Old default inputs (as used for earlier default_sim_seed_42), with new betas
#  proportionately scaled, and R_t_init and extirp scaled also.

# Also fitting it manually here to then test sim_and_fit()

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
                          lags = list(R_prime_t = 0,
                                      S_t = 0:3),
                          first_difference = TRUE)

usethis::use_data(default_sim_seed_42_new,
                  overwrite = TRUE)

usethis::use_data(default_sim_seed_42_new_fit,
                  overwrite = TRUE)