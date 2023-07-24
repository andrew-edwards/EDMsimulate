##' @title Simulation of multiple realisaztions of Larkin population dynamics 
##' model with estimation
##' 
##' @description Simulate a population and fit it using pbsEDM::pbsEDM(), 
##' doing multiple realisations for a given set of parameters (so the only 
##' difference is the stochasticity)
##'
##' This only returns the simulated recruitment `R_prime_T_sim` for the final time step, the
##' forecasted `R_prime_T_edm_fit` calculated using EDM (obviously taking `R_prime_T_sim`
##' out of the input to `pbsEDM::pbsEDM()`, and the EDM summary results. Use
##' `sim_and_fit()` with a particular seed to get full results for any
##' specific realisation. The seed for each simulated data set is given by
##' `set.seed(m)` where `m` is the simulation number, going from 1 to `M`.
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
##' @param first_difference
##' @param centre_and_scale
##' @param larkin_args List of arguments to pass onto `larkin::forecast()`. 
##' @param M number of realisations
##'
##' @return Tibble with row `m` corresponding to realisation `m` and giving
##' the simulated recruitment from year-T spawners, `R_prime_T_sim`, and fitted recruitment `R_prime_T_edm_fit` for
##'   year `T`, followed by the values reported in the `results`
##'   object in output from `pbsEDM:pbsEDM()`, namely `E N_rho N_rmse X_rho
##'   X_rmse` (exact columns may change in future). Probably won't need all
##'   those, but save them for now as good for understanding.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' TODO res <- sim_and_fit(pbsEDM_args = list(lags = list(R_prime_t = 0,
##'                                                   S_t = 0:3),
##'                                       first_difference = TRUE))
##' res$fit$results$X_rho
##' # Think of year 80 as being 2019ish, not 2023.
##' }
sim_and_fit_realisations <- function(salmon_sim_args = list(),
                                     pbsEDM_args = list(
                                       lags = list(R_prime_t = 0,
                                                   S_t = 0:3),
                                       first_difference = TRUE,
                                       centre_and_scale = TRUE),
																		 larkin_args = list(
																		 	prior_mean_alpha = 2,
																		 	prior_mean_beta = -rep(1,4),
																		 	prior_mean_sigma = 0.5,
																		 	prior_sd_alpha = 0.5,
																		 	prior_sd_beta = rep(0.25,4),
																		 	prior_sd_sigma = 0.25),
                                     M = 5) {

	start_time <- Sys.time()
	
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
  																	lar_q5 = numeric(),
  																	lar_95 = numeric(),
  																	lar_sd = numeric(),
  																	lar_rhat = numeric()
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
    simulated[T, "R_prime_t"] = NA    # Ensure no knowledge of it for pbsEDM() (as
                                # neighbour etc., though our code ensure that anyway).

    fit.edm <- do.call(pbsEDM::pbsEDM,
                   c(list(N = simulated),
                     pbsEDM_args))

    stopifnot("First lags argument in pbsEDM_args must relate to R_prime_t with no lag" =
                  names(as.data.frame(fit.edm$N))[1] == "R_prime_t")

    testthat::expect_equal(simulated$R_prime_t,
                           fit.edm$N_observed[-(T+1)])  # Extra check, above one
                                        # should catch lagging misnaming.

    fit.lar <- do.call(larkin::forecast,
    									 c(list(data = simulated, 
    									 			 recruits = "R_prime_t",
    									 			 spawners = "S_t"), larkin_args))

    
    res_realisations[m, ] <- c(m,
                               R_prime_T_sim,
                               fit.edm$N_forecast[T],
                               fit.edm$results,
    													 fit.lar$forecasts$median,
    													 fit.lar$forecasts$q5,
    													 fit.lar$forecasts$q95,
    													 fit.lar$forecasts$sd,
    													 fit.lar$forecasts$max_rhat
    													 )
  }

  end_time <- Sys.time()
  cat("runtime = ", round(end_time-start_time, 0), "minutes")
  return(res_realisations)
}
