##' @title Simulation of multiple realisations of Larkin population dynamics
##' model with estimation using EDM, and Larkin and Ricker models.
##'
##' @description Simulate a population and fit it using `pbsEDM::pbsEDM()` for
##'   EDM, and `larkin::forecast()` for Larkin and Ricker model. Can switch off
##'   running of any of the methods. Doing multiple realisations for a given set
##'   of parameters (so the only difference is the stochasticity)
##'
##' For each realisation `m`, this calculates returns the simulated recruitment `R_prime_T_sim` for the final time step, the
##' forecasted `R_prime_T_edm_fit` calculated using EDM (obviously taking `R_prime_T_sim`
##' out of the input to `pbsEDM::pbsEDM()`, and the EDM summary results;
##'   followed by outputs from fitting the Larkin and Ricker models.
##'
##' For EDM
##' use `sim_and_fit()` with a particular seed to get full results for any
##' specific realisation.
##'
##' @param salmon_sim_args List of arguments to pass onto `salmon_sim()`,
##'   including `T` for the final time step. TODO make p_prime standalone
##' @param T Explicit `T` because we need a default; this gets overwritten by
##'   any `T` in `salmon_sim_args`.
##' @param pbsEDM_args List of arguments to pass onto `pbsEDM::pbsEDM()`. Note
##'   that `lags` has no default, so needs to be specified here, and that `R_prime_t`
##'   has to be the first one (with a lag of zero, so `R_prime_t = 0` or `R_prime_t = 0:1`
##'   etc.) to be the response variable. Note that the default list here is just
##'   to run examples, and if the list is different then `first_difference` and
##'   `centre_and_scale` need to be explicictly specified in the new list (since
##'   their defaults in `pbsEDM()` are FALSE, not TRUE like here.
##' @param larkin_args List of arguments to pass onto `larkin::forecast()`.
##' @param ricker_args List of arguments to pass onto `larkin::forecast()`.
##' @param M number of realisations
##'
##' @return Tibble with row `m` corresponding to realisation `m`, for which
##' the seed for the simulated data set is given by `set.seed(m)`. Columns are
##' \describe{
##'   \item{m:}{realisation, from 1 to `M`}
##'   \item{R_prime_T_sim:}{the simulated `R_prime_T` value (i.e. simulated
##'   recruitment from year-T spawners) for the final time
##'   step), which was not used for any of the fitting methods}
##'   \item{R_prime_T_edm_fit`:}{forecasted value of the final recruitment calculated using EDM}
##'   \item{`E`, `N_rho`, `N_rmse`, `X_rho`, `X_rmse`:}{standard output from EDM}
##'   \item{R_prime_T_lar_fit`:}{forecasted value of the final recruitment calculated using fitting a Larkin
##'   model}
##'   \item{`lar_5`, `lar_95`, `lar_sd`, `lar_rhat`:}{5th and 95th percentiles
##'   and standard deviation of `R_prime_t_lar_fit`, and max rhat TODO what is
##'   that, from fitting the Larkin model}
##'   \item{`R_prime_T_ric_fit`, `ric_5`, `ric_95`, `ric_sd`, `ric_rhat`:}{ equivalent
##'   results from fitting a Ricker model.}
##' }
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' TODO res <- sim_and_fit(pbsEDM_args = list(lags = list(R_prime_t = 0,
##'                                                   S_t = 0:3),
##'                                       first_difference = TRUE))
##' res$fit$results$X_rho
##' # Think of year 80 as being 2019ish, not 2023. Unless we change it to
##'   returns ;)
##' sim_and_fit_realisations(M=1)
##' res <- sim_and_fit_realisations(M=2, larkin_fit = TRUE, ricker_fit = TRUE)
##' }
sim_and_fit_realisations <- function(salmon_sim_args = list(),
                                     edm_fit = TRUE,
                                     larkin_fit = FALSE,
                                     ricker_fit = FALSE,
                                     pbsEDM_args = list(
                                       lags = list(R_prime_t = 0,
                                                   S_t = 0:3),
                                       first_difference = TRUE,
                                       centre_and_scale = TRUE),
                                     larkin_args = list(
                                       run_stan = TRUE,#FALSE,
                                       prior_mean_alpha = 2,
                                       prior_mean_beta = -rep(1,4),
                                       prior_mean_sigma = 0.5,
                                       prior_sd_alpha = 0.5,
                                       prior_sd_beta = rep(0.25,4),
                                       prior_sd_sigma = 0.25),
                                     ricker_args = list(
                                       run_stan = TRUE,
                                       prior_mean_alpha = 2,
                                       prior_mean_beta = -1,
                                       prior_mean_sigma = 0.5,
                                       prior_sd_alpha = 0.5,
                                       prior_sd_beta = 0.25,
                                       prior_sd_sigma = 0.25),
                                     M = 10) {

  tictoc::tic("model run time")#start_time <- Sys.time()

  # Need explicit values for these three here
  if(is.null(salmon_sim_args$p_prime)){
    p_prime <- eval(formals(salmon_sim)$p_prime)
  } else {
    p_prime <- salmon_sim_args$p_prime
  }

  if(is.null(salmon_sim_args$T)){
    T <- eval(formals(salmon_sim)$T)
  } else {
    T <- salmon_sim_args$T
  }

  if(is.null(salmon_sim_args$T_transient)){
    T_transient <- eval(formals(salmon_sim)$T_transient)
  } else {
    T_transient <- salmon_sim_args$T_transient
  }

  T_total <- T_transient + T

  res_realisations <- dplyr::tibble(m = numeric(),
                                    R_prime_T_sim = numeric(),
                                    R_prime_T_edm_fit = numeric(),
                                    E = numeric(),   # Though will need specific
                                        # lags also kept track of or specified
                                    N_rho = numeric(),
                                    N_rmse = numeric(),
                                    X_rho = numeric(),
                                    X_rmse = numeric(),
                                    R_prime_T_lar_fit = numeric(),
                                    lar_5 = numeric(),
                                    lar_95 = numeric(),
                                    lar_sd = numeric(),
                                    lar_rhat = numeric(),
                                    R_prime_T_ric_fit = numeric(),
                                    ric_5 = numeric(),
                                    ric_95 = numeric(),
                                    ric_sd = numeric(),
                                    ric_rhat = numeric()
                                    )

  for(m in 1:M){
    cat(m, " of ", M, " realisations")
    set.seed(m)

    epsilon_tg <- matrix(rnorm(T_total * length(p_prime),
                               0,
                               1),
                         T_total,
                         length(p_prime))

    simulated <- do.call(salmon_sim,
                         c(salmon_sim_args,
                           list(epsilon_tg = epsilon_tg)))   # will still get ignored
                                        # if use deterministic = TRUE TODO add
                                        # as test
                                        # Returns values for years 1:T (so we
                                        #  are done with T_transient from here on)
    R_prime_T_sim <- simulated$R_prime_t[T] # Value we are testing the forecast of. Does not
                                # return a tibble like simulated[T, "R_t"] does.
    simulated[T, "R_prime_t"] <- NA    # Ensure no knowledge of it for pbsEDM() (as
                                # neighbour etc., though our code ensure that anyway).

    res_realisations[m, "m"] <- m
    res_realisations[m, "R_prime_T_sim"] <- R_prime_T_sim

    if(edm_fit){
      fit_edm <- do.call(pbsEDM::pbsEDM,
                         c(list(N = simulated),
                           pbsEDM_args))

      stopifnot("First lags argument in pbsEDM_args must relate to R_prime_t with no lag" =
                  names(as.data.frame(fit_edm$N))[1] == "R_prime_t")

      testthat::expect_equal(simulated$R_prime_t,
                             fit_edm$N_observed[-(T+1)])  # Extra check, above one
                                                          # should catch lagging misnaming.

      res_realisations[m, "R_prime_T_edm_fit"] = fit_edm$N_forecast[T] # TODO
                                        # double check what to do when pbsedm
                                        # arguments change
      res_realisations[m, "E"] = fit_edm$results$E  # Though will need specific
                                                    # lags also kept track of or specified
      res_realisations[m, "N_rho"] = fit_edm$results$N_rho
      res_realisations[m, "N_rmse"] = fit_edm$results$N_rmse
      res_realisations[m, "X_rho"] = fit_edm$results$X_rho
      res_realisations[m, "X_rmse"] = fit_edm$results$X_rmse
    }

    if(larkin_fit){
      fit_lar <- do.call(larkin::forecast,
                         c(list(data = simulated,
                                recruits = "R_prime_t",
                                spawners = "S_t"),
                           larkin_args))

      res_realisations[m, "R_prime_T_lar_fit"] = fit_lar$forecasts$median
      res_realisations[m, "lar_5"] = fit_lar$forecasts$q5
      res_realisations[m, "lar_95"] = fit_lar$forecasts$q95
      res_realisations[m, "lar_sd"] = fit_lar$forecasts$sd
      res_realisations[m, "lar_rhat"] = fit_lar$forecasts$max_rhat # Note
                                        # max_rhat TODO check with Carrie
    }

    if(ricker_fit){
      fit_ric <- do.call(larkin::forecast,
                         c(list(data = simulated,
                                recruits = "R_prime_t",
                                spawners = "S_t"),
                           ricker_args))

      res_realisations[m, "R_prime_T_ric_fit"] = fit_ric$forecasts$median
      res_realisations[m, "ric_5"] = fit_ric$forecasts$q5
      res_realisations[m, "ric_95"] = fit_ric$forecasts$q95
      res_realisations[m, "ric_sd"] = fit_ric$forecasts$sd
      res_realisations[m, "ric_rhat"] = fit_ric$forecasts$max_rhat # Note

    }
  }

  # end_time <- Sys.time()
  # cat("runtime = ", round(end_time-start_time, 2), "minutes")
  tictoc::toc()
  return(res_realisations)
}
