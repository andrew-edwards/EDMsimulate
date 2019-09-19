#' Results of salmon_sim() with original default inputs, and set.seed(42)
#'
#' A dataframe containing the results of the original simulation.
#'
#' @format A data frame with 100 rows and 11 columns, obtained from running
#'   set.seed(42)
#'   default_sim_seed_42 <- salmon_sim()
#'   usethis::use_data(default_sim_seed_42, overwrite=TRUE)
#'   using early defaults of the inputs (which are specified in the test).
#'
"default_sim_seed_42"

#' Results of simple spawner simulation that shows Issue #15 in rEDM
#'
#' @format A vector containing 99 first-differenced values of spawners from a simple
#' simulation. Using it here as it shows Issue #15 in rEDM.
#' It is originally from sockeye-sim-edm.rnw that used Carrie's original
#' function:
#'  set.seed(42)
#'  set.seed(42)
#'  simulated = salmonTraj() # Simulated annual spawner abundances and recruitments
                             #  (as a list object)
#'  N = simulated$S             # Just the simulated spawners
#'  T = length(N)               # Corresponding years
#'  tvec = 1:T
#'  x = N[-1] - N[-length(N)]   # x = first-differences
#'  simple_ts <- x
"simple_ts"
