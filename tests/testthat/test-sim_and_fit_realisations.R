context("sim_and_fit_realisations.R")

test_that("sim_and_fit_realisations() runs and gives correct answer for simulation with default inputs", {

  expect_equal(1 + 1, 2)   # dummy to not have empty test

  # cmdstanr::check_cmdstan_toolchain()   # if gives error on GHA then will be fiddly to
  # include tests. If doesn't error then ideally want to do cmdstanr::install_cmdstan()
  # which should automatically set the path.

  # TODO when come to update tests.
  # Andy should make some EDM tests again.
  # Also use something like these to check outputs:
  # ten_sim_fits_create_in_test <- sim_and_fit_realisations()_
  # expect_equal(dplyr::pull(ten_sim_fits_create_in_test$res_realisations[10, ],
  #                                 "R_prime_T_edm_fit"), dplyr::pull(ten_sim_fits_create_in_test$fit_edm_full_series[10, ], "80"))
  # And save one as data object to compare.

  skip_on_ci()

  if(Sys.getenv("USERNAME") == "EdwardsAnd"){
    if(Sys.getenv("TERM") == "emacs"){
      skip()
    }
  }           # Will fail on Emacs Speaks Statistics; keep an eye on
              #  https://discourse.mc-stan.org/t/cmdstanr-backend-ends-r-process-in-emacs-but-not-terminal/32421
  # Andy can still run test() in an R shell.

  three_sim_fits_create_in_test <- sim_and_fit_realisations(M = 3,
                                                            larkin_fit = TRUE,
                                                            ricker_fit = TRUE)
  expect_equal(three_sim_fits,
               three_sim_fits_create_in_test$res_realisations)

  # Not updated yet since going to tweak function further with more options -
  #  this currently fails since sim_and_fit_realisations() now outputs more and
  #  we haven't updated the test. Probably have to skip this one on GHA. Think
  #  this should be an error for expect_equal? But want an expect_equal one eventually.
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
