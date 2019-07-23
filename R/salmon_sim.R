##' @title Simulation of Larkin population dynamics model
##'
##' @description Simulates Larkin population dynamics model, based on Appendix F of Holt
##'   et al. 2018/011 Res Doc (and what's in our draft manuscript), Ricker
##'   Appendix, and Grant 2010/042 Res. Doc.
##' Currently just Ricker, then extend for Larkin.
##'
##' @param a
##' @param b
##' @param p_prime Vector of typical proportion of recruits spawned in a year that will
##'   come back to freshwater as age-3, age-4 and age-5 (each of three elements
##'   in the vector).
##' @param omega
##' @param sigma_nu
##' @param sigma_epsilon Standard deviation of annual normal deviates on the
##'     proportions returning at each age.
##' @param init Vector of five years of initial spawner abundance (units of
##'   10,000 fish?).
##' @param T Number of years to run the simulation.
##' @return Data frame of years (rows) with columns (plus their notation):
##'   R_t: total recruits returning in year t;
##'   R_prime_t: number of adult recruits generated from spawners in year t that
##'     will return to freshwater (and then be subject to fishing and can then
##'     spawn);
##'   p_3t, p_4t, p_5t: proportion of R_prime in year t that later return at age 3,
##'     4 and 5.
salmon_sim <- function(a = 1,
                       b = 1,
                       p_prime = c(0.01, 0.98, 0.01),
                       rho = 0.6,
                       omega = 0.8,
                       sigma_nu = 0.75,
                       sigma_epsilon = 1,
                       init = c(0.6, 0.01, 0.01, 0.01, 0.6),
                       T = 100
                       ){

  init_years <- length(init)       # Initial number of years

  # Generate stochastic variation in p_{g,t}
  epsilon_gt =
  p_gt_unnormalized = p_prime * exp(omega * epsilon)

  # Initialize - run for five years to get some randomness


  # Loop of full run

  # Return data frame of results
}
