##' Simulate a population and fit it using pbsEDM::pbsEDM(), doing multiple realisations for a given set of parameters (so the only difference is the stochasticity)
##'
##' This only returns the simulated recruitment `R_T_sim` for the final time step, the
##' forecasted `R_T_edm_fit` calculated using EDM (obviously taking `R_T_sim`
##' out of the input to `pbsEDM::pbsEDM()`, and the EDM summary results. Use
##' `sim_and_fit()` with a particular seed to get full results for any
##' specific realisation. The seed for each simulated data set is given by
##' `set.seed(m)` where `m` is the simulation number, going from 1 to `M`.
##'
##' @param salmon_sim_args List of arguments to pass onto `salmon_sim()`,
##'   including `T` for the final time step.
##' @param pbsEDM_args List of arguments to pass onto `pbsEDM::pbsEDM()`. Note
##'   that `lags` has no default, so needs to be specified here, and that `R_t`
##'   has to be the first one (with a lag of zero, so `R_t = 0` or `R_t = 0:1`
##'   etc.) to be the response variable.
##' @param M number of realisations
##'
##' @return Tibble with row `m` corresponding to realisation `m` and giving
##' the simulated recruitment `R_T_sim` and fitted recruitment `R_T_edm_fit` for
##'   year `T`, followed by the values reported in the `results`
##'   object in output from `pbsEDM:pbsEDM()`, namely `E N_rho N_rmse X_rho
##'   X_rmse` (exact columns may change in future). Probably won't need all
##'   those, but save them for now as good for understanding.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' TODO res <- sim_and_fit(pbsEDM_args = list(lags = list(R_t = 0,
##'                                                   S_t = 0:3),
##'                                       first_difference = TRUE))
##' res$fit$results$X_rho
##' }
sim_and_fit_realisations <- function(salmon_sim_args = list(
                                       p_prime = c(0.003, 0.917, 0.080),
                                       T = 80),
                                     pbsEDM_args = list(
                                       lags = list(R_t = 0,
                                                   S_t = 0:3),
                                       first_difference = TRUE),
                                     M = 2){

  T <- salmon_sim_args$T
  p_prime <- salmon_sim_args$p_prime

  res_realisations <- dplyr::tibble(m = numeric(),
                                    R_T_sim = numeric(),
                                    R_T_edm_fit = numeric(),
                                    E = numeric(),   # Though will need specific
                                        # lags also kept track of or specified
                                    N_rho = numeric(),
                                    N_rmse = numeric(),
                                    X_rho = numeric(),
                                    X_rmse = numeric())
  for(m in 1:M){
    set.seed(m)

    epsilon_tg <- matrix(rnorm(T * length(p_prime),
                               0,
                               1),
                         T,
                         length(p_prime))

    simulated <- do.call(salmon_sim,
                         c(salmon_sim_args,
                           list(epsilon_tg = epsilon_tg)))   # will still get ignored
                                        # if use deterministic = TRUE TODO add
                                        # as test

    R_T_sim <- simulated$R_t[T] # Value we are testing the forecast of. Does not
                                # return a tibble like simulated[T, "R_t"] does.
    simulated[T, "R_t"] = NA    # Ensure no knowledge of it for pbsEDM() (as
                                # neighbour etc., though our code ensure that anyway).

    fit <- do.call(pbsEDM::pbsEDM,
                   c(list(N = simulated),
                     pbsEDM_args))

    stopifnot("First lags argument in pbsEDM_args must relate to R_t with no lag" =
                  names(as.data.frame(fit$N))[1] == "R_t")

    testthat::expect_equal(simulated$R_t,
                           fit$N_observed[-(T+1)])  # Extra check, above one
                                        # should catch lagging misnaming.

    res_realisations[m, ] <- c(m,
                               R_T_sim,
                               fit$N_forecast[T],
                               fit$results)
  }

  return(res_realisations)
}
