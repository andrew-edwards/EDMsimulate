# Larkin and Ricker simulation models, based on Appendix F of Holt et al. 2018/011 Res Doc.
# Do for Larkin, then for Ricker call the Larkin one with parameters set to 0. Or do it's own one.

# input - parameter values, initial conditions, number of years, harvest rate h


salmon_sim <- function(a,
                       b,
                       c,
                       rho,
                       sigma
                       ){
  R_dash[y] <- S[y] * exp(a * (1 - S[y]) / b  + phi[y]) # fake for now
}
