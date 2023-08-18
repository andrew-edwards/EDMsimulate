##' Simulate a population and fit it using pbsEDM::pbsEDM(), for getting
##'  full results for any specific simulation. Allows estimation based on `R_t`
##'  or `R_prime_t`. Removes knowledge of the final
##'  value since knowing that affects the rho calculations.
##'
##' @param salmon_sim_args List of arguments to pass onto `salmon_sim()`.
##' @param pbsEDM_args List of arguments to pass onto `pbsEDM::pbsEDM()`. Note
##'   that `lags` has no default, so needs to be specified here. First one
##'   must be called `R_switch` to then get automatically changed to what is
##'   specified by the `R_switch` parameter
##' @param R_switch either `R_prime_t` or `R_t` to specify which one
##'   the calculations are based on.
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
sim_and_fit <- function(salmon_sim_args = list(),
                        pbsEDM_args = list(
                          lags = list(R_switch = 0,
                                      S_t = 0:3)),
                        R_switch = "R_prime_t"){

  stopifnot(R_switch %in% c("R_t", "R_prime_t"))
  stopifnot(names(pbsEDM_args$lags)[1] == "R_switch")
  names(pbsEDM_args$lags)[1] <- R_switch

  simulated <- do.call(salmon_sim,
                       salmon_sim_args)

  simulated_use <- simulated
  simulated_use[nrow(simulated_use), R_switch] = NA    # Ensure no
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
