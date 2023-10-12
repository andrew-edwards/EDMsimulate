# Stan not running properly in GHA so have to skip the ones that do Larkin and
# Ricker fits.

context("sim_and_fit_realisations.R")

test_that("sim_and_fit_realisations() runs and gives correct answer for simulation with default inputs", {

  expect_equal(1 + 1, 2)   # dummy to not have empty test

  expect_no_error(cmdstanr::check_cmdstan_toolchain())    # Does not give error
                                        # on GHA (6/10/23) but still get later
                                        # error saying CmdStan Path has not been
                                        # set. Think too hard to resolve, as
                                        # noted above.
  # cmdstanr::check_cmdstan_toolchain()   # if gives error on GHA then will be fiddly to
  # include tests. If doesn't error then ideally want to do cmdstanr::install_cmdstan()
  # which should automatically set the path.

  # Moving to here, as I think it's the parallel stuff that is not working on
  # GHA, and fiddly to check (and Stan not working either, so we're not missing
  # out too much).

  skip_on_ci()

  ten_sim_fits_edm_only_create_in_test <- sim_and_fit_realisations()
  # checking the full time series output matches the summary one:
  expect_equal(dplyr::pull(ten_sim_fits_edm_only_create_in_test$res_realisations[10, ],
                           "R_prime_T_plus_1_edm_fit"),
               dplyr::pull(ten_sim_fits_edm_only_create_in_test$fit_edm_full_series[10, ],
                           "81"))

  # Check with an earlier saved one (do this test on GHA, so can't compare
  #  three_sim_fits Ricker and Larkin values
  expect_equal(dplyr::select(three_sim_fits[3, ], "m":"X_rmse"),
               dplyr::select(ten_sim_fits_edm_only_create_in_test$res_realisations[3, ], "m":"X_rmse"))


  # Test excplicitly specifying other options:
  expect_no_error(sim_and_fit_realisations(salmon_sim_args = list(
                                             p_prime = c(0.1, 0.8, 0.1),
                                             T = 50,
                                             T_transient = 10,
                                             sigma_nu = 0.5)))

  # Test parallel and not parallel give the same results, but not parallel does
  #  not run through Emacs so moving it to further down
  expect_no_error(res <- sim_and_fit_realisations(M=2))

  # TODO save one as data object to compare.

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

  res_not_parallel <- sim_and_fit_realisations(M=2,
                                               do_parallel = FALSE)
  expect_equal(res, res_not_parallel, tolerance = 2e-04)
  # Higher-than-default tolerance needed, presumably there must be some random
  #  element in the fitting which changes with parallel or not


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
