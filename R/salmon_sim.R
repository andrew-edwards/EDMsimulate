##' @title Simulation of Larkin population dynamics model
##'
##' @description Simulates Larkin population dynamics model, based on Appendix F of Holt
##'   et al. 2018/011 Res Doc (and what's in our draft manuscript), Ricker
##'   Appendix, and Grant 2010/042 Res. Doc.
##' Currently just Ricker, then extend for Larkin. Notation matches that of our
##'   write up.
##'
##' @param alpha Ratio of recruits to spawners at low spawner abundance in the
##'   absence of noise.
##' @param beta Vector [beta_0, beta_1, beta_2, beta_3] that scales
##'   the magnitude of density dependence based on the current spawning stock
##'   (beta_0) and the previous three years (beta_1, beta_2 and beta_3).
##'   beta_1 = beta_2 = beta_3 = 0 reduces the model to a Ricker model.
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
##'   p_t3, p_t4, p_t5: proportion of R_prime in year t that later returned at age 3,
##'     4 and 5.
salmon_sim <- function(a = 1,
                       beta = c(0.8, 0.2, 0.1, 0.1)   # Andy made up
                       p_prime = c(0.01, 0.98, 0.01),
                       rho = 0.6,
                       omega = 0.8,
                       sigma_nu = 0.75,
                       sigma_epsilon = 1,
                       init = c(0.6, 0.01, 0.01, 0.01, 0.6),
                       T = 100
                       ){

  init_years <- length(init)       # Initial number of years

  # Generate stochastic variation in p_{t,g}
  epsilon_tg = matrix(rnorm(T * length(p_prime), 0, sigma_epsilon),
                      T, length(p_prime) )
                                        # 0 should be -sigma_eplison^2 / 2?

  p_tg_unnormalized = exp(omega * epsilon_tg) * p_prime
  p_tg = p_tg_unnormalized / rowSums(p_tg_unnormalized)

  # Initialize - run for five years to get some randomness


  # Loop of full run

  # Return data frame of results
}
