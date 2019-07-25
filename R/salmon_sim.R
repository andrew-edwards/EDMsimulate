##' @title Simulation of Larkin population dynamics model
##'
##' @description Simulates Larkin population dynamics model, based on Appendix F of Holt
##'   et al. 2018/011 Res Doc (and what's in our draft manuscript), Ricker
##'   Appendix, and Grant 2010/042 Res. Doc.
##'   Notation matches that of our write up.
##'   The harvest rate has to be prescribed for all years, and to initialize the
##'   model the recruitment R_t has to be given for the first eight years.
##' @param alpha Ratio of recruits to spawners at low spawner abundance in the
##'   absence of noise.
##' @param beta Vector [beta_0, beta_1, beta_2, beta_3] that scales
##'   the magnitude of density dependence based on the current spawning stock
##'   (beta_0) and the previous three years (beta_1, beta_2 and beta_3).
##'   beta_1 = beta_2 = beta_3 = 0 reduces the model to a Ricker model.
##' @param p_prime Vector of typical proportion of recruits spawned in a year that will
##'   come back to freshwater as age-3, age-4 and age-5 (each of three elements
##'   in the vector).
##' @param sigma_epsilon Standard deviation of annual normal deviates on the
##'     proportions returning at each age.
##' @param sigma_nu Standard deviation of process noise.
##' @param rho Autocorrelation parameter for process noise.
##' @param phi_1 Initial value of process noise.
##' @param T Number of years to run the simulation, including the eight years
##'   needed for initialising the simulation.
##' @param h_t Vector of harvest rate for each year 1, 2, 3, ..., T.
##' @param R_t_init Vector of eight years of recruit abundance (units of
##'   10,000 fish??) to initialize the model.
##' @return Matrix of years (rows) with named columns:
##'   t: year;
##'   R_t: total recruits returning in year t;
##'   R_prime_t: number of adult recruits generated from spawners in year t that
##'     will return to freshwater (and then be subject to fishing and can then
##'     spawn);
##'   S_t: number of fish that spawn in year t;
##'   h_t: harvest rate in year t;
##'   p_t3, p_t4, p_t5: proportion of R_prime in year t that later returned at age 3,
##'     4 and 5.
##' @export
salmon_sim <- function(alpha = 0.8,
                       beta = c(0.8, 0.2, 0.1, 0.1),   # Andy made up
                       p_prime = c(0.01, 0.98, 0.01),
                       rho = 0.6,
                       omega = 0.8,
                       sigma_nu = 0.75,
                       sigma_epsilon = 1,
                       phi_1 = 0.1,
                       T = 100,
                       h_t = rep(0.2, T),
                       R_t_init = c(0.6, 0.1, 0.1, 0.1, 0.6, 0.1, 0.1, 0.1)
                       ){
  T_init <- length(R_t_init)
  if(T_init != 8) stop("R_t_init must have length 8.")

  # Generate stochastic variation in p_{t,g}
  epsilon_tg <- matrix(rnorm(T * length(p_prime), 0, sigma_epsilon),
                       T, length(p_prime) )
                                        # 0 should be -sigma_eplison^2 / 2 ?
  p_tg_unnormalized <- exp(omega * epsilon_tg) * p_prime
  p_tg <- p_tg_unnormalized / rowSums(p_tg_unnormalized)
  # names(p_tg) <- c("p_t3", "p_t4", "p_t5")
  # Generate autocorrelated process noise phi_t
  phi_t <-  c(phi_1, rep(NA, T-1))
  nu_t <- rnorm(T, -sigma_nu^2 / 2, sigma_nu)
  for(i in 2:T){
    phi_t[i] <- rho * phi_t[i-1] + nu_t[i]
  }

  # Initialize - depends directly on initial conditions
  R_t <- c(R_t_init, rep(NA, T - length(R_t_init)))
  S_t <- (1 - h_t) * R_t

  R_prime_t <- alpha * S_t * exp(1 -
                                 beta[1] * S_t -
                                 beta[2] * EDMsimulate::shift(S_t, 1) -
                                 beta[3] * EDMsimulate::shift(S_t, 2) -
                                 beta[4] * EDMsimulate::shift(S_t, 3) +
                                 phi_t)        # beta[1] is beta_0

  # Loop of full run
  for(i in (T_init+1):T){
    R_t[i] <- p_tg[i-3,1] * R_prime_t[i-3] +
              p_tg[i-4,2] * R_prime_t[i-4] +
              p_tg[i-5,3] * R_prime_t[i-5]
    S_t[i] <- (1 - h_t[i]) * R_t[i]
    R_prime_t[i] <- alpha * S_t[i] * exp(1 -
                                         beta[1] * S_t[i] -
                                         beta[2] * S_t[i-1] -
                                         beta[3] * S_t[i-2] -
                                         beta[4] * S_t[i-3] +
                                         phi_t[i])         # beta[1] is beta_0
  }

  # Returns data frame of results
  as.data.frame(
                cbind("t" = 1:T,
                      "R_t" = R_t,
                      "R_prime_t" = R_prime_t,
                      "S_t" = S_t,
                      "h_t" = h_t,
                      "p_t3" = p_tg[, 1],
                      "p_t4" = p_tg[, 2],
                      "p_t5" = p_tg[, 3],
                      "epsilon_t3" = epsilon_tg[, 1],
                      "epsilon_t4" = epsilon_tg[, 2],
                      "epsilon_t5" = epsilon_tg[, 3]))
}


##' Plot output from salmon_sim()
##'
##' @param x Output data frame from salmon_sim().
##' @param new_plot Start a new plot or not.
##' @param ...
##' @return Plot of simulated spawners against time.
plot_sim <- function(x, new_plot=TRUE, ...){
  if(new_plot){
               plot(x$t,
               x$S_t,
               xlab = "Time, years",
               ylab = "Spawners",
               type = "o",
               ...)
  } else {
               points(x$t,
               x$S_t,
               xlab = "Time, years",
               ylab = "Spawners",
               type = "o",
               col = "red",
               ...)}
}


##' Simulate Larkin model and plot results
##'
##' @param ... Inputs for salmon_sim().
##' @param new_plot Start a new plot or not.
##' @return Plot of results.
salmon_run <- function(..., new_plot=TRUE){
  x <- salmon_sim(...)
  plot_sim(x,
           new_plot = new_plot)
}

# Tried this but was giving dim(xx) = 1000,11; seems okay now. It's using an
# exising value of T to set h_t I think, it doesn't get overwritten here. Fix that.
#xx <- salmon_sim(alpha = 0.8,
 #                                 beta = c(0.8, 0.2, 0.1, 0.1),
  #                                p_prime = c(0.01, 0.98, 0.01),
   #                               rho = 0.6,
    #                              omega = 0.8,
     #                             sigma_nu = 0.75,
      #                            sigma_epsilon = 1,
       #                           phi_1 = 0.1,
        #                        R_t_init = c(0.6, 0.1, 0.1, 0.1, 0.6, 0.1, 0.1, 0.1))
