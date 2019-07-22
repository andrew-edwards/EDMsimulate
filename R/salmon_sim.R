##' Simulate salmon population dynamics.
##'
##' Simulation Larkin and Ricker simulation models, based on Appendix F of Holt et al. 2018/011 Res Doc... content for \details{} ..  Currently just Larkin, then for Ricker call the Larkin one with parameters set to 0. Or do it's own one.
##' @title
##' @param a
##' @param b
##' @param p_prime
##' @param omega
##' @param sigma_nu
##' @param sigma_epsilon
##' @return
##' @author
salmon_sim <- function(a = 1,
                       b = 1,
                       p_prime = c(0.01, 0.98, 0.01)
                       rho = 0.6,
                       omega = 0.8,
                       sigma_nu = 0.75,
                       sigma_epsilon = 1

                       ){
  R_dash[y] <- S[y] * exp(a * (1 - S[y]) / b  + phi[y]) # fake for now
}
