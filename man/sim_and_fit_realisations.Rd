% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_and_fit_realisations.R
\name{sim_and_fit_realisations}
\alias{sim_and_fit_realisations}
\title{Simulation of multiple realisations of Larkin population dynamics
model with estimation using EDM (Simplex algorithm), multiview embedding, and Larkin and Ricker models.}
\usage{
sim_and_fit_realisations(
  salmon_sim_args = list(),
  target_hr = 0.2,
  sigma_ou = 0,
  edm_fit = TRUE,
  mvs_fit = FALSE,
  larkin_fit = TRUE,
  ricker_fit = TRUE,
  R_switch = "R_prime_t",
  pbsEDM_args = list(lags = list(R_switch = 0, S_t = 0:3), first_difference = TRUE,
    centre_and_scale = TRUE),
  mve_args = list(lags = list(R_t = 0:4, S_t = 0:8)),
  larkin_args = list(run_stan = FALSE, prior_mean_alpha = 2, prior_mean_beta = -rep(1,
    4), prior_mean_sigma = 0.5, prior_sd_alpha = 0.5, prior_sd_beta = rep(0.25, 4),
    prior_sd_sigma = 0.25),
  ricker_args = list(run_stan = FALSE, prior_mean_alpha = 2, prior_mean_beta = -1,
    prior_mean_sigma = 0.5, prior_sd_alpha = 0.5, prior_sd_beta = 0.25, prior_sd_sigma =
    0.25),
  M = 10,
  do_parallel = TRUE
)
}
\arguments{
\item{salmon_sim_args}{List of arguments to pass onto `salmon_sim()`,
If any of `p_prime`, `T`,
`T_transient`, or `sigma_nu` are not specified then they are given the
default values from `salmon_sim()`. So we simulate for `T_transient` time steps,
then another `T+1` and discard the transients. Use knowledge of `1:T` to give a
forecast for the `T+1`th value, using each method. This agrees with the
EDM manusript notation, but because `T` in `salmon_sim()` is the length of
the simulation, we add 1 to `salmon_sim_args$T` when sending to
`salmon_sim()` from the current function.
TODO make p_prime standalone.}

\item{target_hr}{Target harvest rate to generate time-series of harvest
rates with outcome uncertainty (uncertainty in outcomes form
implementing the target). If omitted a default of constant hr = 0.2 used,
the same default assumption in salmon_sim()}

\item{sigma_ou}{Standard deviation in outcome uncertainty. Default = 0.}

\item{R_switch}{either `R_prime_t` or `R_t` to specify which one
the calculations are based on. This is used as response variable for
multiview embedding.  TODO NOT IMPLEMENTED YET FOR LARKIN OR
RICKER, ALWAYS DOES R_prime_t DESPITE WHAT IS NAMED IN THE OUTPUT. Remove
message command when done.}

\item{pbsEDM_args}{List of arguments to pass onto `pbsEDM::pbsEDM()`. Note
that `lags` has no default, so needs to be specified here, and that `R_switch_t`
has to be the first one (with a lag of zero, so `R_switch_t = 0` or `R_switch_t = 0:1`
etc.) to be the response variable. Note that the default list here is just
to run examples, and if the list is different then `first_difference` and
`centre_and_scale` need to be explicictly specified in the new list (since
their defaults in `pbsEDM()` are FALSE, not TRUE like here.}

\item{mve_args}{List of arguments to pass onto `pbsEDM::multiview_embedding()`.}

\item{larkin_args}{List of arguments to pass onto `larkin::forecast()`.}

\item{ricker_args}{List of arguments to pass onto `larkin::forecast()`.}

\item{M}{number of realisations.}

\item{do_parallel}{logical, if TRUE then run fitting models in parallel
(easier to debug when doing sequentially.}
}
\value{
List containing `res_realisations`, `fit_edm_full_series`,
  `fit_mve_full_series`, `fit_lar_full_series`,
  and `fit_ric_full_series`. These are:
\describe{
  \item{res_realisations:}{Tibble with row `m` corresponding to realisation `m`, for which
the seed for the simulated data set is given by `set.seed(m)`. Columns of
  which are:}
  \describe{
    \item{m:}{realisation, from 1 to `M`}
    \item{R_prime_T_plus_1_sim:}{the simulated `R_prime_T` value (i.e. simulated
    recruitment from year-T spawners) for the final time TODO update R_wicth
    step), which was not used for any of the fitting methods}
    \item{R_prime_T_plus_1_edm_fit`:}{forecasted value of the final recruitment calculated using EDM}
    \item{`E`, `N_rho`, `N_rmse`, `X_rho`, `X_rmse`:}{standard output from
  EDM}
    \item{R_prime_T_plus_1_mve_fit`:}{forecasted value of the final recruitment
  calculated using multiview embedding}
    \item{R_prime_T_plus_1_lar_fit`:}{forecasted value of the final recruitment calculated using fitting a Larkin
    model}
    \item{`lar_5`, `lar_95`, `lar_sd`, `lar_rhat`:}{5th and 95th percentiles
    and standard deviation of `R_prime_t_lar_fit`, and max rhat TODO what is
    that, from fitting the Larkin model}
    \item{`R_prime_T_plus_1_ric_fit`, `ric_5`, `ric_95`, `ric_sd`, `ric_rhat`:}{ equivalent
    results from fitting a Ricker model.}
  }
}
Remaining objects of list are
\describe{
  \item{fit_edm_full_series, fit_mve_full_series, fit_lar_full_series,
  fit_ric_full_series:}{tibbles of full
  fitted time series from each of the four methods; each is a tibble with
  realisation `m` in the first columns, and remaining columns referring to
  the time step from 1:T+1. Note that second column has name "1" referring
  to first time step; so refer to columns by name ("1") not number (1).}
}
}
\description{
Simulate a population and fit it using `pbsEDM::pbsEDM()` for
  EDM, pbsEDM::multiview_embedding()` and `larkin::forecast()` for Larkin and Ricker model. Can switch off
  running of any of the methods. Doing multiple realisations for a given set
  of parameters (so the only difference is the stochasticity)

For each realisation `m` (each being a different simulation), this uses each
  method to estimate the return or recruitment (depending on `R_switch`) at
  time step `T+1` (discarding the transients); see `salmon_sim_args` definition below for
  details. Calculates how well each method performs, and also
  returns the estimated fitted value at each time step for all four
  methods.

For EDM use `sim_and_fit()` with a particular seed to get full results for any
specific realisation.
}
\examples{
\dontrun{
res <- sim_and_fit_realisations(M=2)
res$fit_edm_full_series \%>\% as.data.frame()

res_2 <- sim_and_fit_realisations(M=2, do_parallel = FALSE)

TODO res <- sim_and_fit(pbsEDM_args = list(lags = list(R_prime_t = 0,
                                                  S_t = 0:3),
                                      first_difference = TRUE))
res$fit$results$X_rho
# Think of year 80 as being 2019ish, not 2023. Unless we change it to
  returns ;)
sim_and_fit_realisations(M=1)
res <- sim_and_fit_realisations(M=2, larkin_fit = TRUE, ricker_fit = TRUE)
}
}
\author{
Andrew Edwards
}
