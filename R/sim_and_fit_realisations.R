##' @title Simulation of multiple realisations of Larkin population dynamics
##' model with estimation using EDM (Simplex algorithm), multiview embedding, and Larkin and Ricker models.
##'
##' @description Simulate a population and fit it using `pbsEDM::pbsEDM()` for
##'   EDM, pbsEDM::multiview_embedding()` and `larkin::forecast()` for Larkin and Ricker model. Can switch off
##'   running of any of the methods. Doing multiple realisations for a given set
##'   of parameters (so the only difference is the stochasticity)
##'
##' For each realisation `m` (each being a different simulation), this uses each
##'   method to estimate the return or recruitment (depending on `R_switch`) at
##'   time step `T+1` (discarding the transients); see `salmon_sim_args` definition below for
##'   details. Calculates how well each method performs, and also
##'   returns the estimated fitted value at each time step for all four
##'   methods.
##'
##' For EDM use `sim_and_fit()` with a particular seed to get full results for any
##' specific realisation.
##'
##' @param salmon_sim_args List of arguments to pass onto `salmon_sim()`,
##'   If any of `p_prime`, `T`,
##'   `T_transient`, or `sigma_nu` are not specified then they are given the
##'   default values from `salmon_sim()`. So we simulate for `T_transient` time steps,
##'   then another `T+1` and discard the transients. Use knowledge of `1:T` to give a
##'   forecast for the `T+1`th value, using each method. This agrees with the
##'   EDM manusript notation, but because `T` in `salmon_sim()` is the length of
##'   the simulation, we add 1 to `salmon_sim_args$T` when sending to
##'   `salmon_sim()` from the current function.
##'   TODO make p_prime standalone.
##' @param target_hr Target harvest rate to generate time-series of harvest
##'   rates with outcome uncertainty (uncertainty in outcomes form
##'   implementing the target). If omitted a default of constant hr = 0.2 used,
##'   the same default assumption in salmon_sim()
##' @param sigma_ou Standard deviation in outcome uncertainty. Default = 0.
##' @param R_switch either `R_prime_t` or `R_t` to specify which one
##'   the calculations are based on. This is used as response variable for
##'   multiview embedding.  TODO NOT IMPLEMENTED YET FOR LARKIN OR
##'   RICKER, ALWAYS DOES R_prime_t DESPITE WHAT IS NAMED IN THE OUTPUT. Remove
##'   message command when done.
##' @param pbsEDM_args List of arguments to pass onto `pbsEDM::pbsEDM()`. Note
##'   that `lags` has no default, so needs to be specified here, and that `R_switch_t`
##'   has to be the first one (with a lag of zero, so `R_switch_t = 0` or `R_switch_t = 0:1`
##'   etc.) to be the response variable. Note that the default list here is just
##'   to run examples, and if the list is different then `first_difference` and
##'   `centre_and_scale` need to be explicictly specified in the new list (since
##'   their defaults in `pbsEDM()` are FALSE, not TRUE like here.
##' @param mve_args List of arguments to pass onto `pbsEDM::multiview_embedding()`.
##' @param larkin_args List of arguments to pass onto `larkin::forecast()`.
##' @param ricker_args List of arguments to pass onto `larkin::forecast()`.
##' @param M number of realisations.
##' @param do_parallel logical, if TRUE then run fitting models in parallel
##'   (easier to debug when doing sequentially.
##' @return List containing `res_realisations`, `fit_edm_full_series`,
##'   `fit_mve_full_series`, `fit_lar_full_series`,
##'   and `fit_ric_full_series`. These are:
##' \describe{
##'   \item{res_realisations:}{Tibble with row `m` corresponding to realisation `m`, for which
##' the seed for the simulated data set is given by `set.seed(m)`. Columns of
##'   which are:}
##'   \describe{
##'     \item{m:}{realisation, from 1 to `M`}
##'     \item{R_prime_T_plus_1_sim:}{the simulated `R_prime_T` value (i.e. simulated
##'     recruitment from year-T spawners) for the final time TODO update R_wicth
##'     step), which was not used for any of the fitting methods}
##'     \item{R_prime_T_plus_1_edm_fit`:}{forecasted value of the final recruitment calculated using EDM}
##'     \item{`E`, `N_rho`, `N_rmse`, `X_rho`, `X_rmse`:}{standard output from
##'   EDM}
##'     \item{R_prime_T_plus_1_mve_fit`:}{forecasted value of the final recruitment
##'   calculated using multiview embedding}
##'     \item{R_prime_T_plus_1_lar_fit`:}{forecasted value of the final recruitment calculated using fitting a Larkin
##'     model}
##'     \item{`lar_5`, `lar_95`, `lar_sd`, `lar_rhat`:}{5th and 95th percentiles
##'     and standard deviation of `R_prime_t_lar_fit`, and max rhat TODO what is
##'     that, from fitting the Larkin model}
##'     \item{`R_prime_T_plus_1_ric_fit`, `ric_5`, `ric_95`, `ric_sd`, `ric_rhat`:}{ equivalent
##'     results from fitting a Ricker model.}
##'   }
##' }
##' Remaining objects of list are
##' \describe{
##'   \item{fit_edm_full_series, fit_mve_full_series, fit_lar_full_series,
##'   fit_ric_full_series:}{tibbles of full
##'   fitted time series from each of the four methods; each is a tibble with
##'   realisation `m` in the first columns, and remaining columns referring to
##'   the time step from 1:T+1. Note that second column has name "1" referring
##'   to first time step; so refer to columns by name ("1") not number (1).}
##' }
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' res <- sim_and_fit_realisations(M=2)
##' res$fit_edm_full_series %>% as.data.frame()
##'
##' res_2 <- sim_and_fit_realisations(M=2, do_parallel = FALSE)
##'
##' TODO res <- sim_and_fit(pbsEDM_args = list(lags = list(R_prime_t = 0,
##'                                                   S_t = 0:3),
##'                                       first_difference = TRUE))
##' res$fit$results$X_rho
##' # Think of year 80 as being 2019ish, not 2023. Unless we change it to
##'   returns ;)
##' sim_and_fit_realisations(M=1)
##' res <- sim_and_fit_realisations(M=2, larkin_fit = TRUE, ricker_fit = TRUE)
##' }
sim_and_fit_realisations <- function(salmon_sim_args = list(),
                                     target_hr = 0.2,
                                     sigma_ou = 0,
                                     edm_fit = TRUE,
																		 mvs_fit = FALSE,
                                     larkin_fit = TRUE,  # mostly ignored though
                                     ricker_fit = TRUE,
                                     R_switch = "R_prime_t",
                                     pbsEDM_args = list(
                                       lags = list(R_switch = 0,
                                                   S_t = 0:3),
                                       first_difference = TRUE,
                                       centre_and_scale = TRUE),
                                     mve_args = list(
                                       lags = list(R_t = 0:4,
                                                   S_t = 0:8)),
                                     larkin_args = list(
                                       run_stan = FALSE,
                                       prior_mean_alpha = 2,
                                       prior_mean_beta = -rep(1,4),
                                       prior_mean_sigma = 0.5,
                                       prior_sd_alpha = 0.5,
                                       prior_sd_beta = rep(0.25,4),
                                       prior_sd_sigma = 0.25),
                                     ricker_args = list(
                                       run_stan = FALSE,
                                       prior_mean_alpha = 2,
                                       prior_mean_beta = -1,
                                       prior_mean_sigma = 0.5,
                                       prior_sd_alpha = 0.5,
                                       prior_sd_beta = 0.25,
                                       prior_sd_sigma = 0.25),
                                     M = 10,
                                     do_parallel = TRUE){

  tictoc::tic("model run time")#start_time <- Sys.time()

  stopifnot(R_switch %in% c("R_t", "R_prime_t"))
  stopifnot(names(pbsEDM_args$lags)[1] == "R_switch")    # This is okay for mve
  names(pbsEDM_args$lags)[1] <- R_switch                 # Simplex has to have
                                                         # R_switch as lag

  # Need explicit values for these various values here
  if(is.null(salmon_sim_args$p_prime)){
    p_prime <- eval(formals(salmon_sim)$p_prime)
  } else {
    p_prime <- salmon_sim_args$p_prime
  }

  # T is the EDM defintion - we have 1:T and want to forecast T+1.
  if(is.null(salmon_sim_args$T)){
    T <- eval(formals(salmon_sim)$T)
  } else {
    T <- salmon_sim_args$T
  }

  if(is.null(salmon_sim_args$T_transient)){
    T_transient <- eval(formals(salmon_sim)$T_transient)
  } else {
    T_transient <- salmon_sim_args$T_transient
  }

  if(is.null(salmon_sim_args$sigma_nu)){
    sigma_nu <- eval(formals(salmon_sim)$sigma_nu)
  } else {
    sigma_nu <- salmon_sim_args$sigma_nu
  }

  T_total <- T_transient + T + 1     # Simulating the extra value to then
                                     # compare with the forecast from each method

  # Set up results tibbles to be filled in. Call things R_switch... here then
  #  rename at the end with R_t or R_prime_t before returning (to avoid
  #  confusion with the saved output that would arise with having it called R_switch)

  res_realisations <- dplyr::tibble(m = numeric(),
                                    R_switch_T_plus_1_sim = numeric(),
                                    R_switch_T_plus_1_edm_fit = numeric(),
                                    E = numeric(),   # Though will need specific
                                        # lags also kept track of or specified
                                    N_rho = numeric(),
                                    N_rmse = numeric(),
                                    X_rho = numeric(),
                                    X_rmse = numeric(),
                                    R_switch_T_plus_1_mve_fit = numeric(),
                                    mve_response_rho = numeric(),
  																	R_switch_T_plus_1_mvs_fit = numeric(),
  																	mvs_response_rho = numeric(),
  																	R_switch_T_plus_1_lar_fit = numeric(),
                                    lar_5 = numeric(),
                                    lar_95 = numeric(),
                                    lar_sd = numeric(),
                                    lar_rhat = numeric(),
                                    R_switch_T_plus_1_ric_fit = numeric(),
                                    ric_5 = numeric(),
                                    ric_95 = numeric(),
                                    ric_sd = numeric(),
                                    ric_rhat = numeric()
                                    )

  tbl_colnames <- tbl_colnames <- c("m", 1:(T+1))
  # Save the full time series as fit by each method, each row is realisation m
  #  followed by R_switch_t for each t
  fit_edm_full_series <- tibble::as_tibble(matrix(NA,
                                                  nrow = M,
                                                  ncol = length(tbl_colnames)),
                                           .name_repair = ~ tbl_colnames)  # Empty tibble correctly named; columns are logical
                                                                           # but will get changed to double/numeric when they get filled in.
  fit_mve_full_series <- fit_edm_full_series

  fit_mvs_full_series <- fit_edm_full_series

  fit_lar_full_series <- fit_edm_full_series

  fit_ric_full_series <- fit_lar_full_series


  # First do all the simulations, to then paraellise the fitting.
  all_sims <- vector("list", M)     # create list

  for(m in 1:M){
    set.seed(m)

    epsilon_tg <- matrix(rnorm(T_total * length(p_prime),
                               0,
                               1),
                         T_total,
                         length(p_prime))

    nu_t <- rnorm(T_total,
                  -sigma_nu^2 / 2,
                  sigma_nu)

    # Beta-distributed outcome uncertainty (uncertainty in outcomes of
    # implementing target harvest rate), as applied in samSim
    # https://github.com/Pacific-salmon-assess/samSim
    if(target_hr != 0){
    	if(sigma_ou != 0){
    		location <- pmax(0.00001,
                                 target_hr^2 * (((1 - target_hr) / sigma_ou^2) -
                                                (1 / sigma_ou)))
    		shape <- pmax(0.00001,
                              location * (1 / target_hr - 1))
    		h_t <- rbeta(n = T_total,
                             shape1 = location,
                             shape2 = shape)
    	}
    	if(sigma_ou == 0){
    		# make blank draw with dummy pars to balance random number generator
    		# Perhaps not needed because of set.seed at start of each trial, except
    		# if more uncertainties are added in the future below
    		blank <- rbeta(T_total, 0.5, 0.5, ncp = 0)
    		h_t <- rep(T_total, target_hr)    # TODO this seems to get
                                        # ignored with target_hr = 0.2 and
                                        # sigma_ou = 0, but it shouldn't.
    	}
    }
    if(target_hr == 0){
     	# make blank draw with dummy pars to balance random number generator
    	blank <- rbeta(T_total, 0.5, 0.5, ncp = 0)
    	h_t <- rep(T_total, target_hr)
    }

    salmon_sim_args$T <- T + 1          # Add 1 to simulate the extra year. Note
                                        # that T_toal in salmon_sim gets defined
                                        # as T + T_transient (hence fiddling here).
   # print(paste0("length(h_t) is ", length(h_t)))

    simulated <- do.call(salmon_sim,
                         c(salmon_sim_args,
                           list(epsilon_tg = epsilon_tg,
                                nu_t = nu_t)))
                                # h_t = h_t)))  AE tried adding this, but see
                                # TODO above, h_t specification was getting ignored.
                                        # will still get ignored
                                        # if use deterministic = TRUE TODO add
                                        # as test
                                        # Returns values for years 1:(T+1) (so we
                                        #  are done with T_transient from here
                                        #  on)

    R_switch_T_plus_1_sim <- dplyr::pull(simulated,
                                  R_switch)[T+1]
                                # Value we are testing the forecast of. Does not
                                # return a tibble like simulated[T+1, "R_t"]
                                # does. Then replace in simulated by NA:
    simulated[T+1, R_switch] <- NA    # Ensure no knowledge of it for pbsEDM() (as
                                # neighbour etc., though our code ensure that
                                # anyway) or Larkin or Ricker.

    # Add a column to the df, simulated that identifies the realisation, m, as
    # the list order (and hence realisation number) gets lost in parallelisation
    simulated['m'] <- m
    res_realisations[m, "m"] <- m
    res_realisations[m, "R_switch_T_plus_1_sim"] <- R_switch_T_plus_1_sim

    all_sims[[m]] <- simulated
  }

  # Carrie has parallelised this loop below. Andy not updated T here with new defn.
  # for(m in 1:M){
  #   cat(m, " of ", M, " realisations")
  #
  #   if(edm_fit){
  #   	fit_edm <- do.call(pbsEDM::pbsEDM,
  #                        c(list(N = all_sims[[m]]),
  #                          pbsEDM_args))
  #
  #     testthat::expect_equal(dplyr::pull(all_sims[[m]], R_switch),
  #                            fit_edm$N_observed[-(T+1)])  # Extra check
  #
  #     # TO DO AE: Check that the time-series is aligned correctly
  #     fit_edm_full_series[m, ]  <- t(c(m,
  #                                      fit_edm$N_forecast))
  #
  #     # TO DO AE: Check that this call below to T=80 is correct, as the time-
  #     # series fit_edm$N_forecast is 81 years long, and when predictions are
  #     # aligned 1:81 (or 2:82 in fit_edm_full_series[m, ]) then it aligns well
  #     # with sims 1:80 (see plots below)
  #     res_realisations[m, "R_switch_T_edm_fit"] = fit_edm$N_forecast[T] # TODO
  #                                       # double check what to do when pbsedm
  #                                       # arguments change
  #     res_realisations[m, "E"] = fit_edm$results$E  # Though will need specific
  #                                                   # lags also kept track of or specified
  #     res_realisations[m, "N_rho"] = fit_edm$results$N_rho
  #     res_realisations[m, "N_rmse"] = fit_edm$results$N_rmse
  #     res_realisations[m, "X_rho"] = fit_edm$results$X_rho
  #     res_realisations[m, "X_rmse"] = fit_edm$results$X_rmse
  #   }
  #
  #   if(larkin_fit){
  #     fit_lar <- do.call(larkin::forecast,
  #                        c(list(data = all_sims[[m]],
  #                               recruits = "R_prime_t",
  #                        			 # This is simply the label for recruitment, and
  #                        			 # does not specify the type of forecasts.
  #                               spawners = "S_t"),
  #                          larkin_args))
  #
  #     # Give the full time-series of predicted values:
  #     # For the stan version, all MCMC draws are provided for both R_prime_t
  #     # and R_t from larkin::forecast. First, the median of posteriors is
  #     # calculated for either R_prime_t or R_t
  #     if(larkin_args$run_stan){
  #     	if (R_switch == "R_prime_t"){
  #     		predR_med <- apply(fit_lar$predR_prime_t, 2, median, na.rm=T)
  #     	}
  #     	if (R_switch == "R_t"){
  #     		predR_med <- apply(fit_lar$predR_t, 2, median, na.rm=T)
  #     	}
  #     	fit_lar_full_series[m, ]  <- t( c(m, rep(NA, length(
  #     		larkin_args$prior_mean_beta ) - 1), predR_med) )
  #     	# Note,the first 3 years are NAs, since the Larkin model cannot be
  #     	# calculated until year 4 of time-series as it requires lagged spawner
  #     	# abundances at t=-1, t=-2 and =-3 years.
  #     }
  #
  #     # For MLE version, the best estimates are provided for R_prime_t
  #     # and R_t from larkin::forecast.
  #     if(!larkin_args$run_stan){
  #     	if (R_switch == "R_prime_t"){
  #     		fit_lar_full_series[m, ]  <- t( c(m, rep(NA, length(
  #     			larkin_args$prior_mean_beta)  - 1),	fit_lar$predR_prime_t))
  #     	}
  #
  #     	if (R_switch == "R_t"){
  #     		fit_lar_full_series[m, ]  <- t( c(m, rep(NA, length(
  #     			larkin_args$prior_mean_beta ) - 1),	fit_lar$predR_t) )
  #     	}
  #     }
  #
  #     names(fit_lar)[names(fit_lar) == paste0("forecasts_", R_switch)] <-
  #     	"forecasts"
  #     res_realisations[m, "R_switch_T_lar_fit"] <-
  #     	fit_lar$forecasts$median
  #     res_realisations[m, "lar_5"]  <- fit_lar$forecasts$q5
  #     res_realisations[m, "lar_95"] <- fit_lar$forecasts$q95
  #     res_realisations[m, "lar_sd"] <- fit_lar$forecasts$sd
  #     res_realisations[m, "lar_rhat"] <-  fit_lar$forecasts$max_rhat
  #    # AE: Note max_rhat TODO check with Carrie
  #   }
  #
  #   if(ricker_fit){
  #     fit_ric <- do.call(larkin::forecast,
  #                        c(list(data = all_sims[[m]],
  #                               recruits = "R_prime_t",
  #                               spawners = "S_t"),
  #                          ricker_args))
  #
  #
  #     # Give the full time-series of predicted values:
  #     # For the stan version, all MCMC draws are provided for both R_prime_t
  #     # and R_t from larkin::forecast. First, the median of posterior is
  #     # calculated.
  #     if(ricker_args$run_stan){
  #     	if (R_switch == "R_prime_t"){
  #     		predR_med <- apply(fit_ric$predR_prime_t, 2, median, na.rm=T)
  #     	}
  #     	if (R_switch == "R_t"){
  #     		predR_med <- apply(fit_ric$predR_t, 2, median, na.rm=T)
  #     	}
  #     	fit_ric_full_series[m, ]  <- t( c(m, predR_med) )
  #     }
  #
  #     # For MLE version, the best estimates are provided for R_prime_t
  #     # and R_t from larkin::forecast.
  #     if(!ricker_args$run_stan){
  #     	if (R_switch == "R_prime_t"){
  #     		fit_ric_full_series[m, ]  <- t( c(m, fit_ric$predR_prime_t) )
  #     	}
  #
  #     	if (R_switch == "R_t"){
  #     		fit_ric_full_series[m, ]  <- t( c(m,	fit_ric$predR_t) )
  #     	}
  #     }
  #
  #     names(fit_ric)[names(fit_ric) == paste0("forecasts_", R_switch)] <-
  #     	"forecasts"
  #     res_realisations[m, "R_switch_T_ric_fit"] <-
  #     	fit_ric$forecasts$median
  #     res_realisations[m, "ric_5"]  <-  fit_ric$forecasts$q5
  #     res_realisations[m, "ric_95"] <-  fit_ric$forecasts$q95
  #     res_realisations[m, "ric_sd"] <-  fit_ric$forecasts$sd
  #     res_realisations[m, "ric_rhat"] <- fit_ric$forecasts$max_rhat
  #   }
  # }

  if(do_parallel){
    numCores <- parallel::detectCores() - 1 # number of cores to use

    cl<- parallel::makeCluster(numCores, type = "PSOCK") # type of cluster
    parallel::clusterEvalQ(cl, c(library(EDMsimulate), library(pbsEDM), library(larkin),
                                 library(testthat), library(dplyr)))

    parallel::clusterExport(cl, c("res_realisations", "all_sims", "R_switch",
                                  "T", "pbsEDM_args", "mve_args", "larkin_args", 
    															"ricker_args", "fit_models", "mvs_fit", 
    															"larkin_fit", "ricker_fit"), 
    												envir=environment())

    outputs <- parallel::parLapply(cl, all_sims, function(x) {
      fit_models(x,
                 # res_realisations = res_realisations[m, ],
      					 res_realisations = res_realisations,
      					 R_switch = R_switch,
                 T = T,
                 pbsEDM_args = pbsEDM_args,
                 mve_args = mve_args,
                 larkin_args = larkin_args,
                 ricker_args = ricker_args,
      					 mvs_fit = mvs_fit,
      					 larkin_fit = larkin_fit,
      					 ricker_fit = ricker_fit
      					 )
    }
    )

    parallel::stopCluster(cl)
  } else {                          # Not parallel
    outputs <- vector("list", M)    # create empty list
    for(m in 1:M){
     outputs[[m]] <- fit_models(all_sims[[m]],         # TODO But all_sims in
                                        # fit_models is what??
                                res_realisations = res_realisations,    # TODO
                                        # not sure exactly what should go here
     																		# CH: this is the entire dataframe, each core slots in the results for the row corresponding to that realisation, which are aggregated below
                                R_switch = R_switch,
                                T = T,
                                pbsEDM_args = pbsEDM_args,
                                mve_args = mve_args,
                                larkin_args = larkin_args,
                                ricker_args = ricker_args)
    }
  }

  for(m in 1:M){
    res_realisations[m,] <- outputs[[m]]$single_realisation
    fit_edm_full_series[m,] <- outputs[[m]]$fit_edm_single
    fit_mve_full_series[m,] <- outputs[[m]]$fit_mve_single
    if(mvs_fit) fit_mvs_full_series[m,] <- outputs[[m]]$fit_mvs_single
    # if(!mvs_fit) fit_mvs_full_series[m,] <- NA #This may be redudant
    if(larkin_fit) fit_lar_full_series[m,] <- outputs[[m]]$fit_lar_single
    # if(!larkin_fit) fit_lar_full_series[m,] <- NA #This may be redudant
    if(ricker_fit) fit_ric_full_series[m,] <- outputs[[m]]$fit_ric_single
    # if(!ricker_fit) fit_lar_full_series[m,] <- NA #This may be redudant

  }


  plot_realisation <- FALSE
  if(plot_realisation & larkin_fit & ricker_fit){
    m_plot <- 2 #which realisation to plot
    # if(m==M){
    # PLot simulated and predicted values for one realisation
    # First get simulated values
    sim <- all_sims[[m_plot]] %>% dplyr::pull(R_switch)
    sim[T+1] <- R_switch_T_plus_1_sim # Add last years value back in

    # Then add predicted values
    df <- data.frame(Year = 1:(T+1),
                     Abundance= sim,
                     Series="Simulated",
                     EstimationBias = NA)
    df <- df %>% tibble::add_row(Year = 1:(T+1),
                                 Abundance =  t(fit_mve_full_series[m_plot,
    c(2:(T+2))]),   # TODO AE guessing that last bit a little, and two lines down:
    # CH: I have realigned so that years 1-81 are lincluded in sim and fits and cor
                                 Series = "MVE",
                                 EstimationBias = t(fit_mve_full_series[m_plot,
    c(2:(T+2))]) - sim) %>%
      tibble::add_row(Year = 1:(T+1),
                      Abundance =  t(fit_lar_full_series[m_plot, c(2:(T+2))]),
                      Series = "Larkin",
                      EstimationBias = t(fit_lar_full_series[m_plot, c(2:(T+2))]) - sim) %>%
      tibble::add_row(Year = 1:(T+1),
                      Abundance = t(fit_ric_full_series[m_plot, c(2:(T+2))]),
                      Series = "Ricker",
                      EstimationBias = t(fit_ric_full_series[m_plot, c(2:(T+2))]) - sim)
# TODO TODO check these manually: CH- both times-series are now 81
    cor.mve <- cor(sim, as.vector(t(fit_mve_full_series[m_plot, 2:(T+2)])),
                   use="pairwise.complete.obs")
    cor.lar <- cor(sim, as.vector(t(fit_lar_full_series[m_plot, 2:(T+2)])),
                   use="pairwise.complete.obs")
    cor.ric <- cor(sim, as.vector(t(fit_ric_full_series[m_plot, 2:(T+2)])),
                    use="pairwise.complete.obs")

    yMax <- max(sim, na.rm=TRUE)
# TODO Andy not updated yet with T+1 idea
    
    df$Series <- 	factor( df$Series, 
    											levels = c("Ricker", "Larkin", "MVE", "Simulated") )
    
      plot.timeseries <- df %>% ggplot(aes(x=Year, y=Abundance, group=Series)) +
      geom_line(aes(colour=Series, linewidth=Series)) +
      # scale_linewidth_manual(values = c(0.5,0.5,0.5,1)) +
    	scale_linewidth_manual(values = c(0.5, 0.5, 0.5,1)) +
    	scale_colour_manual(values = 
    												c(scales::hue_pal()(3),
    												# c(brewer.pal(n=3, name ="Dark2")[c(1,2,3)],
                                     grey(0.5))) #+
      # geom_text(x=(T-15), y=yMax*0.8,
      #           label=paste0("MVE cor = ", round(cor.mve,2)),
      #           colour = brewer.pal(n=3, name ="Dark2")[1]) +
      # geom_text(x=(T-15), y=yMax*0.7,
      #           label=paste0("Larkin cor = ", round(cor.lar,2)),
      #           colour = brewer.pal(n=3, name ="Dark2")[2]) +
      # geom_text(x=(T-15), y=yMax*0.6,
      #           label=paste0("Ricker cor = ", round(cor.ric,2)),
      #           colour = brewer.pal(n=3, name ="Dark2")[3]) #+
      # # geom_text(x=5, y=yMax*0.,
      #           label=paste0("realisation = ", m_plot))

    plot.errors <- df %>% dplyr::filter(Series!="Simulated") %>%
      ggplot(aes(x=Year, y=EstimationBias, group=Series)) +
      geom_line(aes(colour=Series)) +
      scale_colour_manual(values = c(brewer.pal(n=3, name ="Dark2")))

    p1 <- gridExtra::grid.arrange(plot.timeseries, plot.errors, ncol=1)
    p1
    ggsave(filename = paste0("realisation", m_plot, ".png"), p1)
    # }

  }


  stringr::str_sub(R_switch,
                   -1,
                   -1) <- ""     # This modifies R_switch (takes off final t so it's
                                 # either R_ or R_prime_)

  names(res_realisations)[names(res_realisations) == "R_switch_T_plus_1_sim"] <-
    paste0(R_switch, "T_plus_1_sim")
  names(res_realisations)[names(res_realisations) == "R_switch_T_plus_1_edm_fit"] <-
  	paste0(R_switch, "T_plus_1_edm_fit")
  names(res_realisations)[names(res_realisations) == "R_switch_T_plus_1_mve_fit"] <-
    paste0(R_switch, "T_plus_1_mve_fit")
  names(res_realisations)[names(res_realisations) == "R_switch_T_plus_1_mvs_fit"] <-
  	paste0(R_switch, "T_plus_1_mvs_fit")
  names(res_realisations)[names(res_realisations) == "R_switch_T_plus_1_lar_fit"] <-
    paste0(R_switch, "T_plus_1_lar_fit")
  names(res_realisations)[names(res_realisations) == "R_switch_T_plus_1_ric_fit"] <-
    paste0(R_switch, "T_plus_1_ric_fit")

  # end_time <- Sys.time()
  # cat("runtime = ", round(end_time-start_time, 2), "minutes")
  tictoc::toc()

  return(list(res_realisations = res_realisations,
              fit_edm_full_series = fit_edm_full_series,
              fit_mve_full_series = fit_mve_full_series,
  						fit_mvs_full_series = fit_mvs_full_series,
  						fit_lar_full_series = fit_lar_full_series,
              fit_ric_full_series = fit_ric_full_series))
}
