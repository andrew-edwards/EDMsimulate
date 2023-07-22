##' Simulate a population and fit it using pbsEDM::pbsEDM(), for getting
##'  full results for any specific simulation. Removes knowledge of the final
##'  `R_prime_T` since knowing that affects the rho calculations.
##'
##' @return List object with components:
##'  - `simulated:` the simulated tibble
##'  - `fit:` the full output from pbsEDM::pbsEDM().
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' res <- sim_and_fit(salmon_sim_args = list(deterministic = TRUE))
##' plot(res$fit$N_observed, res$fit$N_forecast)
##' abline(a = 0, b = 1)   # looks good for deterministic run, which is good
##'
##' res <- sim_and_fit(pbsEDM_args = list(lags = list(R_prime_t = 0,
##'                                                   S_t = 0:3),
##'                                       first_difference = TRUE))
##' res$fit$results$X_rho
##' }
##' @param salmon_sim_args List of arguments to pass onto `salmon_sim()`.
##' @param pbsEDM_args  List of arguments to pass onto `pbsEDM::pbsEDM()`. Note
##'   that `lags` has no default, so needs to be specified here.
sim_and_fit <- function(salmon_sim_args = list(),
                        pbsEDM_args = list(
                          lags = list(R_prime_t = 0,
                                      S_t = 0:3))){
  # want to set seed in a wrapper function to this one, and then include the
  # epsilton_tg in salmon_sim_args
  simulated <- do.call(salmon_sim,
                       salmon_sim_args)   #


  simulated_use <- simulated
  simulated_use[nrow(simulated_use), "R_prime_t"] = NA    # Ensure no
                                        # knowledge of it for pbsEDM(), so it
                                        # doesn't affect the rho values (else it does)

  fit <- do.call(pbsEDM::pbsEDM,
                 c(list(N = simulated_use),
                   pbsEDM_args))

  to_return <- list(simulated = simulated,
                    fit = fit)
  # concatenated list, with simulated tibble first then list of all results from pbsEDM
  return(to_return) # for now, probably want to scale down or have option to
                    # just return basic results for when doing many simulations
}
