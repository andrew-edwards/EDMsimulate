##' @title Simulation of Larkin population dynamics model
##'
##' @description Simulates Larkin population dynamics model, based on Appendix F of Holt
##'   et al. 2018/011 Res Doc (and what's in our draft manuscript), Ricker
##'   Appendix, and Grant 2010/042 Res. Doc.
##'   Notation matches that of our write up.
##'   The harvest rate has to be prescribed for all years, and to initialize the
##'   model the recruitment R_t has to be given for the first eight years. All
##'   parameters must be >=0, some >0. Units of recruits and spawners are
##'   nominally millions of fish, and is the only absolute scale because the
##'   beta_i sum to 1. Simulation runs for a transient time of `T_transient`
##'   years and then saves ouputs values for the next `T` years.
##' @param alpha Number or vector of length (T+T_transient), where each element
##'   is the annual ratio of recruits to spawners at low spawner abundance in
##'   the absence of noise.
##' @param beta Vector [beta_0, beta_1, beta_2, beta_3] that scales
##'   the relative magnitude of density dependence based on the current spawning stock
##'   (beta_0) and the previous three years (beta_1, beta_2 and beta_3), with
##'   beta_0 + beta_1 + beta_2 + beta_3 = 1. Setting
##'   beta_1 = beta_2 = beta_3 = 0 reduces the model to a Ricker model. Be aware
##'   that `beta[1]` in code is $beta_0$ from the write up.
##' @param p_prime Vector of typical proportion of recruits spawned in a year that will
##'   come back to freshwater as age-3, age-4 and age-5 (each of three elements
##'   in the vector, which must sum to 1).
##' @param rho Autocorrelation parameter for process noise, >= 0.
##' @param omega Scales the annual normal deviates on the proportions returning
##'   at each age. If omega = 0 then there is no stochasticity for the
##'   proportions (and if `deterministic = 0` then `omega` is set to 0.
##' @param sigma_nu Standard deviation of process noise. If sigma_nu = 0 and
##'   rho = 0 then no process noise. Ignored if `nu_t` specified.
##' @param nu_t Explicit specification of the process noise term to allow
##'   reproducible simulations when testing different methods (see
##'   `epsilon_tg` also). Must be a numeric vector of length `T + T_transient`, can have
##'   negative values. If not specified then a default is created.
##' @param phi_1 Initial value of process noise.
##' @param T Number of years of the simulation to return.
##' @param T_transient Number of years to first run the model for a transient
##'   time; for later fitting using the `T` years from
##'   `(T_transient + 1):(T_transient + T)`. Total time includes the eight years
##'   needed for initialising the simulation, so `T_transient + T` must be >=9.
##' @param h_t Vector of harvest rate for each year 1, 2, 3, ..., `T_transient`. Or if a
##'   single value then this will be the constant rate for all years. If NULL
##'   then harvest rate will be set to 0.2 for all years.
##' @param R_t_init Vector of eight years of recruit abundance to initialize the
##'   model.
##' @param epsilon_tg Specified matrix of random variation in the `p_tg`. Must
##'   be of dimension `T_transient * length(p_prime)` (returns error if not), where
##'   `length(p_prime) = 3` for the default. Each column of `epsilon_tg`
##'   corresponds to the respective element of `p_prime`, so column 1 refers to age-3,
##'   etc (see equations in write up). For running multiple simulations, this
##'   overall setup will allow seeds
##'   to be specified and thus specific values of stochasticity to be
##'   explicitly input as the matrix `epsilon_tg` (so they can be
##'   the same when testing different models). Experience tells us this should
##'   be clearer than creating the stochasticity within this function. If
##'   `epsilon_tg` is not specified then a default is created (which obviously
##'   depends on the seed). This is ignored if `deterministic` is TRUE.
##' @param deterministic If TRUE then include no stochasticity and any
##'   given values of rho, omega, sigma_nu, phi_1, nu_t, and epsilon_tg are redundant
##'   and set to 0 (nu_t is just ignored).
##' @param extirp Value below which we consider the population extirpated, in
##'   same units as recruits and spawners (so if those are assumed to change,
##'   this value should be changed also).
##' @return Tibble of years (rows) with named columns:
##'   t: year, covering the non-transient years;
##'   R_t: total recruits returning in year t;
##'   R_prime_t: number of adult recruits generated from spawners in year t that
##'     will return to freshwater (and then be subject to fishing and can then
##'     spawn);
##'   S_t: number of fish that spawn in year t;
##'   h_t: harvest rate in year t;
##'   p_t3, p_t4, p_t5: proportion of R_prime in year t that later returned at age 3,
##'     4 and 5;
##'   epsilon_t3, epsilon_t4, epsilon_t5: realised stochastic variation in the epsilons
##'   phi_t: realised stochastic variation in `phi_t`
##'   alpha: alpha productivity values, since we now allow the option for time-varying
##' @export
salmon_sim <- function(alpha = 7,  # Carrie's updated defaults
                       beta = c(0.25, 0.25, 0.25, 0.25),
                       p_prime = c(0.003, 0.917, 0.080),
                       rho = 0,
                       omega = 0.4,
                       sigma_nu = 1,
                       nu_t = NULL,
                       phi_1 = 0,
                       T = 80,
                       T_transient = 100,
                       h_t = NULL,
                       R_t_init = c(25, 5, 1, 1, 25.01, 5.02, 1.01, 1.02)*0.05,
                                   # not exactly repeated to avoid locking for
                                   #  deterministic runs
                       epsilon_tg = NULL,
                       deterministic = FALSE,
                       extirp = 2e-6){

  if(!is.numeric(c(alpha,
                   beta,
                   p_prime,
                   rho,
                   omega,
                   sigma_nu,
                   phi_1,
                   T,
                   T_transient,
                   R_t_init,
                   extirp))){
    stop("all arguments (except deterministic) must be numeric")
  }

  T_total <- T + T_transient        # total time to run the simulation for

  if(!is.logical(deterministic) | length(deterministic) != 1){
    stop("deterministic must be TRUE or FALSE")
  }
  if(is.na(deterministic)) stop("deterministic must be TRUE or FALSE, not NA")

  if(T_total < 9) stop("T + T_transient must be >=9")

  if(length(beta) != 4){
    stop("beta must have length 4")
  }

  if(sum(beta) != 1) stop("beta values must sum to 1")

  if(is.null(h_t)){
    h_t <- rep(0.2, T_total)
  } else {
    if(!is.numeric(h_t)){
      stop("h_t must be numeric")
    }
  }

  if(length(h_t) == 1){    # Repeat a single given value
    h_t <- rep(h_t, T_total)
  }

  if(min(c(alpha,
           beta,
           p_prime,
           rho,
           omega,
           sigma_nu,
           phi_1,
           T,
           T_transient,
           h_t,
           extirp)) < 0 ) {
    stop("alpha, beta, p_prime, rho, omega, sigma_nu, phi_1, T, T_transient, h_t, and extirp parameters must be >=0")
  }

  if(min(alpha,
         c(max(R_t_init))) == 0 ) {
    stop("alpha and at least one value of R_t_init must be >0")
   }

  if(length(c(rho,
              omega,
              sigma_nu,
              phi_1,
              T,
              T_transient,
              extirp)) != 7){
    stop("rho, omega, sigma_nu, phi_1, T, and T_transient must all have length 1")
  }

  if(length(alpha) != 1 & length(alpha) != (T + T_transient)){
  	stop("alpha must all have length 1 or (T + T_transient)")
  }

  if(length(p_prime) != 3 | sum(p_prime) != 1){
    stop("p_prime must have length 3 and sum to 1")
  }

  if(length(h_t) != T_total) stop("h_t must have length T + T_transient.")
  if(max(h_t) >= 1) stop("h_t values must be <1.")

  T_init <- length(R_t_init)
  if(T_init != 8) stop("R_t_init must have length 8.")

  # If deterministic then can run simpler model
  if(deterministic){
    rho <- 0
    omega <- 0
    sigma_nu <- 0
    phi_1 <- 0
    # Generate no stochastic variation in p_{t,g}
    epsilon_tg <- matrix(0,
                         T_total,
                         length(p_prime) )  # Not used but is returned
    p_tg <- matrix(p_prime,
                   nrow = T_total,
                   ncol=length(p_prime), byrow=TRUE)

    # Generate no process noise phi_t
    phi_t <- rep(0, T_total)

  } else {

    # Generate stochastic variation in p_{t,g}
    if(is.null(epsilon_tg)){
      # If not specified then use the previous default
      epsilon_tg <- matrix(rnorm(T_total * length(p_prime),
                                 0,
                                 1),
                           T_total,
                           length(p_prime))
    } else {
      stopifnot("Matrix of specified epsilon_tg must have dimension (T + T_transient) * length(p_prime)" =
                  dim(epsilon_tg) == c(T_total, length(p_prime)))
    }

    p_tg_unnormalized <- t( t(exp(omega * epsilon_tg)) * p_prime)

    p_tg <- p_tg_unnormalized / rowSums(p_tg_unnormalized)  # repeats columnwise
    # names(p_tg) <- c("p_t3", "p_t4", "p_t5")

    # Generate autocorrelated process noise phi_t
    if(is.null(nu_t)){
      nu_t <- rnorm(T_total,
                    -sigma_nu^2 / 2,
                    sigma_nu)
    } else {
      stopifnot("Vector of specified nu_t must have length T + T_transient" =
                  length(nu_t) == T_total)
    }

    phi_t <-  c(phi_1,
                rep(NA, T_total - 1))

    for(i in 2:T_total){
      phi_t[i] <- rho * phi_t[i-1] + nu_t[i]
    }
  }

  # Create vector of alpha values if only a single value is iprovided
  if(length(alpha) ==1) {
  	alpha <- rep(alpha, T_total)
  }

   # Initialize - depends directly on initial conditions
  R_t <- c(R_t_init, rep(NA, T_total - length(R_t_init)))
  S_t <- (1 - h_t) * R_t


  R_prime_t <- alpha[1:T_total] * S_t * exp(- beta[1] * S_t -
                                 beta[2] * EDMsimulate::shift(S_t, 1) -
                                 beta[3] * EDMsimulate::shift(S_t, 2) -
                                 beta[4] * EDMsimulate::shift(S_t, 3) +
                                 phi_t)        # beta[1] is beta_0

  # Loop of full run
  for(i in (T_init + 1):T_total){
    R_t[i] <- p_tg[i-3,1] * R_prime_t[i-3] +
              p_tg[i-4,2] * R_prime_t[i-4] +
              p_tg[i-5,3] * R_prime_t[i-5]

    S_t[i] <- (1 - h_t[i]) * R_t[i]
    if(S_t[i] < extirp){
      S_t[i] <- 0
    }

    R_prime_t[i] <- alpha[i] * S_t[i] * exp(- beta[1] * S_t[i] -
                                         beta[2] * S_t[i-1] -
                                         beta[3] * S_t[i-2] -
                                         beta[4] * S_t[i-3] +
                                         phi_t[i])         # beta[1] is beta_0
  }

  non_transient <- (T_transient + 1):T_total    # indices to return T years
  # Returns data frame of results
  dplyr::tibble("t" = 1:T,
                "R_t" = R_t[non_transient],
                "R_prime_t" = R_prime_t[non_transient],
                "S_t" = S_t[non_transient],
                "h_t" = h_t[non_transient],
                "p_t3" = p_tg[non_transient, 1],
                "p_t4" = p_tg[non_transient, 2],
                "p_t5" = p_tg[non_transient, 3],
                "epsilon_t3" = epsilon_tg[non_transient, 1],
                "epsilon_t4" = epsilon_tg[non_transient, 2],
                "epsilon_t5" = epsilon_tg[non_transient, 3],
                "phi_t" = phi_t[non_transient],
                "alpha" = alpha[non_transient])
}


##' Plot output from salmon_sim()
##'
##' @param x Output data frame from salmon_sim().
##' @param new_plot Start a new plot or not.
##' @param ... Further inputs for plot() or points().
##' @return Plot of simulated spawners against time.
##' @export
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
##' @return Plot of results, and matrix of results from salmon_sim().
##' @examples \dontrun{
##'   salmon_run(alpha = 1.5,
##'              beta = c(0, 0.8, 0, 0),
##'              p_prime=c(0, 1, 0),
##'              T = 1000,
##'              deterministic = TRUE)# no densitity dependence on alternate
##'                                   #  years, so unconstrained increase in
##'                                   #  population
##' }
##' @export
salmon_run <- function(..., new_plot=TRUE){
  x <- salmon_sim(...)
  plot_sim(x,
           new_plot = new_plot)
  return(x)
}
