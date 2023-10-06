##' @title Fit EDM, Larkin, and Ricker models
##' @description Fits EDM, Larkin, and Ricker models to simulated data using
##'   parallel processing
##' @param all_sims List of simulated data generated from salmon_sim()
##' @param res_realisations Empty data frame with correct headers for outputs
##'   of estimation
##' @param R_switch either `R_prime_t` or `R_t` to specify which one
##'   the calculations are based on.
##' @param T number of time points being used to forecast `R_switch` at `T+1`
##' @param pbsEDM_args List of arguments to pass onto `pbsEDM::pbsEDM()`. Note
##'   that `lags` has no default, so needs to be specified here. First one
##'   must be called `R_switch` to then get automatically changed to what is
##'   specified by the `R_switch` parameter
##' @param larkin_args List of arguments to pass onto `larkin::....()`.
##' @param ricker_args List of arguments to pass onto `larkin::....()`.
##' @return List object call output with components:
##'  - `single_realisation` forecasts from all models in last year
##'  - `fit_edm_single` predictions for each year from EDM, with realisation
##'   number appended to start
##'  - `fit_lar_single` predictions for each year from larkin, with realisation
##'   number appended to start
##'  - `fit_ric_single` predictions for each year from ricker, with realisation
##'   number appended to start
##' @export
##' @author Carrie Holt and Andrew Edwards
##' @examples
##' \dontrun{
##' }

fit_models <- function(all_sims,
                       res_realisations,
                       R_switch,
                       T,
                       pbsEDM_args,
                       larkin_args,
                       ricker_args){

  fit_edm <- do.call(pbsEDM::pbsEDM,
                     c(list(N = all_sims),
                       pbsEDM_args))

  testthat::expect_equal(dplyr::pull(all_sims, R_switch),
                         fit_edm$N_observed[-(T+1)])  # Extra check

  # TO DO AE: Check that the time-series is aligned correctly
  # CH Changed to a single vector and revised the call to m from the all_sims
  # input
  realisation <- pull(all_sims['m'][1,])

  fit_edm_single  <- t(c(realisation,
                         fit_edm$N_forecast[-length(fit_edm$N_forecast)]))
  # Note the realisation number added to the front (to keep track of in parallel calculations).
  # And had to take off the last value, as pbsEDM adds on an extra time step. So, for T
  # = 80, we will have simulated 81, and so length(fit_edm_single) = 82 because of
  # the realisation number added to the front.

  # TO DO AE: Check that this call below to T=80 is correct, as the time-
  # series fit_edm$N_forecast is 81 years long, and when predictions are
  # aligned 1:81 (or 2:82 in fit_edm_full_series[m, ]) then it aligns well
  # with sims 1:80 (see plots below). AE fixed has fixed T+1 defintion,
  # hopefully this then makes plots agree.

  # CH changed to single vector, and changed 'm' to 'realisation'

  single_realisation <- res_realisations[realisation,]
  single_realisation[ "R_switch_T_plus_1_edm_fit"] <-  fit_edm$N_forecast[T+1]
  # TODO double check what to do when pbsedm
  # arguments change
  single_realisation["E"] <-  fit_edm$results$E  # Though will need specific
  # lags also kept track of or specified
  single_realisation["N_rho"] <-  fit_edm$results$N_rho
  single_realisation["N_rmse"] <-  fit_edm$results$N_rmse
  single_realisation["X_rho"] <-  fit_edm$results$X_rho
  single_realisation["X_rmse"] <-  fit_edm$results$X_rmse


  fit_lar <- do.call(larkin::forecast,
                     c(list(data = all_sims,
                            recruits = "R_prime_t",
                            # This is simply the label for recruitment, and
                            # does not specify the type of forecasts.
                            spawners = "S_t"),
                       larkin_args))

  # Give the full time-series of predicted values:
  # For the stan version, all MCMC draws are provided for both R_prime_t
  # and R_t from larkin::forecast. First, the median of posteriors is
  # calculated for either R_prime_t or R_t
  if(larkin_args$run_stan){
    if (R_switch == "R_prime_t"){
      predR_med <- apply(fit_lar$predR_prime_t, 2, median, na.rm=TRUE)
    }
    if (R_switch == "R_t"){
      predR_med <- apply(fit_lar$predR_t, 2, median, na.rm=TRUE)
    }
    #CH changed 'm' to 'realisation'
    fit_lar_single  <- t( c(realisation, rep(NA, length(
                                                   larkin_args$prior_mean_beta ) - 1), predR_med) )
    # Note,the first 3 years are NAs, since the Larkin model cannot be
    # calculated until year 4 of time-series as it requires lagged spawner
    # abundances at t=-1, t=-2 and =-3 years.
  }

  # For MLE version, the best estimates are provided for R_prime_t
  # and R_t from larkin::forecast.
  if(!larkin_args$run_stan){
    if (R_switch == "R_prime_t"){
      #CH changed 'm' to 'realisation'
      fit_lar_single  <- t( c(realisation, rep(NA, length(
                                                     larkin_args$prior_mean_beta)  - 1),	fit_lar$predR_prime_t))
    }

    if (R_switch == "R_t"){
      #CH changed 'm' to 'realisation'
      fit_lar_single  <- t( c(realisation, rep(NA, length(
                                                     larkin_args$prior_mean_beta ) - 1),	fit_lar$predR_t) )
    }
  }

  names(fit_lar)[names(fit_lar) == paste0("forecasts_", R_switch)] <-
    "forecasts"
  single_realisation["R_switch_T_plus_1_lar_fit"] <-
    fit_lar$forecasts$median
  single_realisation["lar_5"]  <- fit_lar$forecasts$q5
  single_realisation["lar_95"] <- fit_lar$forecasts$q95
  single_realisation["lar_sd"] <- fit_lar$forecasts$sd
  single_realisation["lar_rhat"] <-  fit_lar$forecasts$max_rhat
  # AE: Note max_rhat TODO check with Carrie

  fit_ric <- do.call(larkin::forecast,
                     c(list(data = all_sims,
                            recruits = "R_prime_t",
                            spawners = "S_t"),
                       ricker_args))


  # Give the full time-series of predicted values:
  # For the stan version, all MCMC draws are provided for both R_prime_t
  # and R_t from larkin::forecast. First, the median of posterior is
  # calculated.
  if(ricker_args$run_stan){
    if (R_switch == "R_prime_t"){
      predR_med <- apply(fit_ric$predR_prime_t, 2, median, na.rm=TRUE)
    }
    if (R_switch == "R_t"){
      predR_med <- apply(fit_ric$predR_t, 2, median, na.rm=TRUE)
    }
    #CH changed 'm' to 'realisation'
    fit_ric_single  <- t( c(realisation, predR_med) )
  }

  # For MLE version, the best estimates are provided for R_prime_t
  # and R_t from larkin::forecast.
  if(!ricker_args$run_stan){
    if (R_switch == "R_prime_t"){
      #CH changed 'm' to 'realisation'
      fit_ric_single  <- t( c(realisation, fit_ric$predR_prime_t) )
    }

    if (R_switch == "R_t"){
      #CH changed 'm' to 'realisation'
      fit_ric_single  <- t( c(realisation,	fit_ric$predR_t) )
    }
  }

  names(fit_ric)[names(fit_ric) == paste0("forecasts_", R_switch)] <-
    "forecasts"
  single_realisation["R_switch_T_plus_1_ric_fit"] <-
    fit_ric$forecasts$median
  single_realisation["ric_5"]  <-  fit_ric$forecasts$q5
  single_realisation["ric_95"] <-  fit_ric$forecasts$q95
  single_realisation["ric_sd"] <-  fit_ric$forecasts$sd
  single_realisation["ric_rhat"] <- fit_ric$forecasts$max_rhat


  return(list(fit_edm_single = fit_edm_single,
              fit_lar_single = fit_lar_single,
              fit_ric_single = fit_ric_single,
              single_realisation = single_realisation))
}
