## Descriptions of saved data objects.

##' Example simulation from running `salmon_sim()` with some specified (not
##' default) values, before beta values were changed to be proportions and sum
##' to 1. See tests for compring this and `default_sim_seed_42_new`.
##'
##' @format Tibble - see `?salmon_sim()`.
##'
##' @source Generated from running code before the betas were changed to sum to 1.
"default_sim_seed_42"


##' Example simulation from running `salmon_sim()` with some specified (not
##' default) values.
##'
##' @format Tibble - see `?salmon_sim()`.
##'
##' @source Generated from running `data-raw/default_sim_seed_42_new.R`.
"default_sim_seed_42_new"

##' Example simulation and fit from running `salmon_sim()` with some specified (not
##' default) values and then `pbsEDM::pbsEDM()`, to test with use of `sim_and_fit()`.
##'
##' @format List of fitting results - see `?pbsEDM::pbsEDM()`.
##'
##' @source Generated from running `data-raw/default_sim_seed_42_new.R`.
"default_sim_seed_42_new_fit"
