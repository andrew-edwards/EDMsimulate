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
##'   followed by outputs from fitting the Larkin and Ricker models. Also
##'   returns the estimated fitted value at each time step for all three
##'   methods.
##'
##' For EDM
##' use `sim_and_fit()` with a particular seed to get full results for any
##' specific realisation.
##'
##' @param salmon_sim_args List of arguments to pass onto `salmon_sim()`,
##'   including `T` for the final time step. If any of `p_prime`, `T`,
##'   `T_transient`, or `sigma_nu` are not specified then they are given the
##'   default values from `salmon_sim()`. TODO make p_prime standalone
##' @param T Explicit `T` because we need a default; this gets overwritten by
##'   any `T` in `salmon_sim_args`.
##' @param R_switch either `R_prime_t` or `R_t` to specify which one
##'   the calculations are based on. TODO NOT IMPLEMENTED YET FOR LARKIN OR
##'   RICKER, ALWAYS DOES R_prime_t DESPITE WHAT IS NAMED IN THE OUTPUT. Remove
##'   message command when done.
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
##' @return List containing `res_realisations`, `fit_edm_full_series`, `fit_lar_full_series`,
##'   and `fit_ric_full_series`. These are:
##' \describe{
##'   \item{res_realisations:}{Tibble with row `m` corresponding to realisation `m`, for which
##' the seed for the simulated data set is given by `set.seed(m)`. Columns of
##'   which are:}
##'   \describe{
##'     \item{m:}{realisation, from 1 to `M`}
##'     \item{R_prime_T_sim:}{the simulated `R_prime_T` value (i.e. simulated
##'     recruitment from year-T spawners) for the final time
##'     step), which was not used for any of the fitting methods}
##'     \item{R_prime_T_edm_fit`:}{forecasted value of the final recruitment calculated using EDM}
##'     \item{`E`, `N_rho`, `N_rmse`, `X_rho`, `X_rmse`:}{standard output from EDM}
##'     \item{R_prime_T_lar_fit`:}{forecasted value of the final recruitment calculated using fitting a Larkin
##'     model}
##'     \item{`lar_5`, `lar_95`, `lar_sd`, `lar_rhat`:}{5th and 95th percentiles
##'     and standard deviation of `R_prime_t_lar_fit`, and max rhat TODO what is
##'     that, from fitting the Larkin model}
##'     \item{`R_prime_T_ric_fit`, `ric_5`, `ric_95`, `ric_sd`, `ric_rhat`:}{ equivalent
##'     results from fitting a Ricker model.}
##'   }
##' }
##' Remaining objects of list are
##' \describe{
##'   \item{fit_edm_full_series, fit_lar_full_series,
##'   fit_ric_full_series:}{tibbles of full
##'   fitted time series from each of the three methods; each is a tibble with
##'   realisation `m` in the first columns, and remaining columns referring to
##'   the time step from 1:T+1. Note that second column has name "1" referring
##'   to first time step; so refer to columns by name ("1") not number (1).}
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
                                     R_switch = "R_prime_t",
                                     pbsEDM_args = list(
                                       lags = list(R_switch = 0,
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

  message("** NOTE ** Larking and Ricker fits not set up yet to do R_switch = R_t, and will just use R_prime_t")
  tictoc::tic("model run time")#start_time <- Sys.time()

  stopifnot(R_switch %in% c("R_t", "R_prime_t"))
  stopifnot(names(pbsEDM_args$lags)[1] == "R_switch")
  names(pbsEDM_args$lags)[1] <- R_switch

  # Need explicit values for these various values here
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

  if(is.null(salmon_sim_args$sigma_nu)){
    sigma_nu <- eval(formals(salmon_sim)$sigma_nu)
  } else {
    sigma_nu <- salmon_sim_args$sigma_nu
  }

  T_total <- T_transient + T

  # Set up results tibbles to be filled in. Call things R_switch... here then
  #  rename at the end with R_t or R_prime_t before returning (to avoid
  #  confusion with the saved output that would arise with having it called R_switch)

  res_realisations <- dplyr::tibble(m = numeric(),
                                    R_switch_T_sim = numeric(),
                                    R_switch_T_edm_fit = numeric(),
                                    E = numeric(),   # Though will need specific
                                        # lags also kept track of or specified
                                    N_rho = numeric(),
                                    N_rmse = numeric(),
                                    X_rho = numeric(),
                                    X_rmse = numeric(),
                                    R_switch_T_lar_fit = numeric(),
                                    lar_5 = numeric(),
                                    lar_95 = numeric(),
                                    lar_sd = numeric(),
                                    lar_rhat = numeric(),
                                    R_switch_T_ric_fit = numeric(),
                                    ric_5 = numeric(),
                                    ric_95 = numeric(),
                                    ric_sd = numeric(),
                                    ric_rhat = numeric()
                                    )

  tbl_colnames <- tbl_colnames <- c("m", 1:(T+1))
  # Save the full time series as fit by each method, each row is realisation m
  #  followed but R_switch_t for each t
  fit_edm_full_series <- tibble::as_tibble(matrix(NA,
                                                  nrow = M,
                                                  ncol = length(tbl_colnames)),
                                           .name_repair = ~ tbl_colnames)  # Empty tibble correctly named; columns are logical
                                                                           # but will get changed to double/numeric when they get filled in.
                                                                           #  TODO prob faster to build it the right size straight away
  fit_lar_full_series <- fit_edm_full_series
  fit_ric_full_series <- fit_edm_full_series


  # First do all the simulations, to then paraellise the fitting.
  all_sims <- vector("list", M)     # create list

  for(m in 1:M){
    set.seed(m)

    epsilon_tg <- matrix(rnorm(T_total * length(p_prime),
                               0,
                               1),
                         T_total,
                         length(p_prime))

    nu_t <- rnorm(T_total,
                  -sigma_nu^2 / 2,
                  sigma_nu)

    simulated <- do.call(salmon_sim,
                         c(salmon_sim_args,
                           list(epsilon_tg = epsilon_tg,
                                nu_t = nu_t)))
                                        # will still get ignored
                                        # if use deterministic = TRUE TODO add
                                        # as test
                                        # Returns values for years 1:T (so we
                                        #  are done with T_transient from here
                                        #  on)

    R_switch_T_sim <- dplyr::pull(simulated,
                                  R_switch)[T]
                                # Value we are testing the forecast of. Does not
                                # return a tibble like simulated[T, "R_t"]
                                # does. Then replace in simulated by NA:
    simulated[T, R_switch] <- NA    # Ensure no knowledge of it for pbsEDM() (as
                                # neighbour etc., though our code ensure that
                                # anyway) or Larkin or Ricker.

    res_realisations[m, "m"] <- m
    res_realisations[m, "R_switch_T_sim"] <- R_switch_T_sim

    all_sims[[m]] <- simulated
  }

  # Carrie can parallelise this loop TODO
  for(m in 1:M){
    cat(m, " of ", M, " realisations")

    if(edm_fit){
      fit_edm <- do.call(pbsEDM::pbsEDM,
                         c(list(N = all_sims[[m]]),
                           pbsEDM_args))

      testthat::expect_equal(dplyr::pull(all_sims[[m]], R_switch),
                             fit_edm$N_observed[-(T+1)])  # Extra check

      fit_edm_full_series[m, ]  <- t(c(m,
                                       fit_edm$N_forecast))

      res_realisations[m, "R_switch_T_edm_fit"] = fit_edm$N_forecast[T] # TODO
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
                         c(list(data = all_sims[[m]],
                                recruits = "R_prime_t", 
                         			 # This is simply the label for recruitment, and 
                         			 # does not specify the type of forecats (recruits 
                         			 # vs returns)
                                spawners = "S_t"), 
                         			 #This is the label for spawners
                           larkin_args))

      # TODO: Carrie to replace *** by the forecast values from Larkin estimation:
      # fit_ric_full_series[m, ]  <- t(c(m,
      #                                  ***))

      names(fit_lar)[names(fit_lar) == paste0("forecasts_", R_switch)] <- 
      	"forecasts"
      res_realisations[m, "R_switch_T_lar_fit"] <-  
      	fit_lar$forecasts$median
      res_realisations[m, "lar_5"]  <- fit_lar$forecasts$q5
      res_realisations[m, "lar_95"] <- fit_lar$forecasts$q95
      res_realisations[m, "lar_sd"] <- fit_lar$forecasts$sd
      res_realisations[m, "lar_rhat"] <-  fit_lar$forecasts$max_rhat 
     # AE: Note max_rhat TODO check with Carrie
    }

    if(ricker_fit){
      fit_ric <- do.call(larkin::forecast,
                         c(list(data = all_sims[[m]],
                                recruits = "R_prime_t",  
                                spawners = "S_t"),
                           ricker_args))

      # TODO Carrie to replace *** by the forecast values from Ricker.
      # fit_ric_full_series[m, ]  <- t(c(m,
      #                                  ***))
      
      names(fit_ric)[names(fit_ric) == paste0("forecasts_", R_switch)] <- 
      	"forecasts"
      res_realisations[m, "R_switch_T_ric_fit"] <- 
      	fit_ric$forecasts$median
      res_realisations[m, "ric_5"]  <-  fit_ric$forecasts$q5
      res_realisations[m, "ric_95"] <-  fit_ric$forecasts$q95
      res_realisations[m, "ric_sd"] <-  fit_ric$forecasts$sd
      res_realisations[m, "ric_rhat"] <- fit_ric$forecasts$max_rhat 
      
      
    }
  }

  stringr::str_sub(R_switch,
                   -1,
                   -1) <- ""     # This modifies R_switch (takes off final t so it's
                                 # either R_ or R_prime_)

  names(res_realisations)[names(res_realisations) == "R_switch_T_sim"] <-
    paste0(R_switch, "T_sim")
  names(res_realisations)[names(res_realisations) == "R_switch_T_edm_fit"] <-
    paste0(R_switch, "T_edm_fit")
  names(res_realisations)[names(res_realisations) == "R_switch_T_lar_fit"] <-
    paste0(R_switch, "T_lar_fit")
  names(res_realisations)[names(res_realisations) == "R_switch_T_ric_fit"] <-
    paste0(R_switch, "T_ric_fit")

  # end_time <- Sys.time()
  # cat("runtime = ", round(end_time-start_time, 2), "minutes")
  tictoc::toc()

  return(list(res_realisations = res_realisations,
              fit_edm_full_series = fit_edm_full_series,
              fit_lar_full_series = fit_lar_full_series,
              fit_ric_full_series = fit_ric_full_series))
}
