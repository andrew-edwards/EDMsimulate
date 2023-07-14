##' Simulate a population and fit it using pbsEDM::pbsEDM()
##'
##'
##'
##' @return List object with first component the simulated tibble, and the
##'   remaining components the output from pbsEDM::pbsEDM(). TODO create an
##'   option to return less output since probably not needed for simulations,
##'   but need now.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
##' @param salmon_sim_args List of arguments to pass onto `salmon_sim()`.
##' @param pbsEDM_args  List of arguments to pass onto `pbsEDM::pbsEDM()`. Note
##'   that `lags` has no default, so needs to be specified here.
sim_and_fit <- function(salmon_sim_args = list(),
                        pbsEDM_args = list()){
  # want to set seed in a wrapper function to this one, and then include the
  # epsilton_tg in salmon_sim_args
  simulated <- do.call(salmon_sim,
                       salmon_sim_args)   #

  fit <- do.call(pbsEDM::pbsEDM,
                 c(list(N = simulated),
                   pbsEDM_args))

  to_return <- list(simulated = simulated,
                    fit = fit)
  # concatenated list, with simulated tibble first then list of all results from pbsEDM
  return(to_return) # for now, probably want to scale down or have option to
                    # just return basic results for when doing many simulations
}



## foo <- function(x, y, sum = list(), grep = list()) {
##  list(sum = do.call("sum", c(x, sum)), grep = do.call("grep", c("abc", y, grep)))
## }

## # test

## X <- c(1:5, NA, 6:10)
## Y <- "xyzabcxyz"
## foo(X, Y, sum = list(na.rm = TRUE), grep = list(value = TRUE))

## ## $sum
## ## [1] 55
## ##
## $grep
## [1] "xyzabcxyz"
