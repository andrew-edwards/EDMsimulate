##' @title Calculate forecasting errors 
##' @description 
##' @param run output from fit_models() for EDM, Larkin and Ricker models
##' @param label name of sensitivity run for plotting purposes
##' @return List object call output with components:
##'  - `forecasts` forecast for the last year, from all models for each 
##'  realisation 
##'  - `absErr` absolute error in forecasts in the last year, from all models 
##'  for each realisation
##'  
##' @export
##' @author Carrie Holt and Andrew Edwards
##' @examples
##' \dontrun{
##' }

forecast_errors <- function(run, label){
	
	# Rename predicted variables (e.g., "R_primeT_sim" or "R_T_sim" for ease of 
	# handling)
	if(names(run$res_realisations)[2]=="R_prime_T_plus_1_sim"){
		res_realisations_plot <- run$res_realisations %>% 
			dplyr::rename( R_sim = R_prime_T_plus_1_sim) %>% 
			dplyr::rename( R_edm = R_prime_T_plus_1_edm_fit) %>% 
			dplyr::rename( R_mve = R_prime_T_plus_1_mve_fit) %>% 
			dplyr::rename( R_lar = R_prime_T_plus_1_lar_fit) %>% 
			dplyr::rename( R_ric = R_prime_T_plus_1_ric_fit)
	}
	if(names(run$res_realisations)[2]=="R_T_plus_1_sim"){
		res_realisations_plot <- run$res_realisations %>% 
			dplyr::rename( R_sim = R_T_plus_1_sim) %>% 
			dplyr::rename( R_edm = R_T_plus_1_edm_fit) %>% 
			dplyr::rename( R_mve = R_T_plus_1_mve_fit) %>% 
			dplyr::rename( R_lar = R_T_plus_1_lar_fit) %>% 
			dplyr::rename( R_ric = R_T_plus_1_ric_fit)
	}
	
	# Calculate absolute error in forecast for EDM, Larkin and Ricker
	out.plot <- res_realisations_plot %>% 
		dplyr::mutate(Err_edm = (R_edm - R_sim), 
									Err_mve = (R_mve = R_sim),
									Err_lar = (R_lar - R_sim),
									Err_ric = (R_ric - R_sim)) %>% 
		dplyr::mutate(absErr_edm = abs(R_edm - R_sim), 
									absErr_mve = abs(R_mve - R_sim),
									absErr_lar = abs(R_lar - R_sim),
									absErr_ric = abs(R_ric - R_sim)) %>% 
		dplyr::mutate(perErr_edm = 100*(R_edm - R_sim)/R_sim, 
									perErr_mve = 100*(R_mve - R_sim)/R_sim,
									perErr_lar = 100*(R_lar - R_sim)/R_sim,
									perErr_ric = 	100*(R_ric - R_sim)/R_sim) %>%
		dplyr::mutate(se_edm = (R_edm - R_sim)^2, 
									se_mve = (R_mve - R_sim)^2,
									se_lar = (R_lar - R_sim)^2,
									se_ric = (R_ric - R_sim)^2)
		
	# Pivot absolute errors into a long table for plotting with ggplot
	errors <- tidyr::pivot_longer(out.plot, c(Err_edm, Err_mve, Err_lar, Err_ric), 
																names_to="forMethod", 
																names_prefix = "Err_", 
																values_to="value")
	errors <- errors %>% dplyr::select(m,forMethod, value) %>% 
		dplyr::mutate(model_label = label) %>% 
		dplyr::mutate(error="Err")
	
	ae <- tidyr::pivot_longer(out.plot, c(absErr_edm, absErr_mve, absErr_lar, 
																				absErr_ric), 
														names_to="forMethod", 
														names_prefix = "absErr_", 
														values_to="value")
	ae <- ae %>% dplyr::select(m,forMethod, value) %>% 
		dplyr::mutate(model_label = label) %>% 
		dplyr::mutate(error="absErr")
	
	pe <- tidyr::pivot_longer(out.plot, c(perErr_edm, perErr_mve, perErr_lar, 
																				perErr_ric), 
																names_to="forMethod", 
																names_prefix = "perErr_", 
																values_to="value")
	pe <- pe %>% dplyr::select(m,forMethod, value) %>% 
		dplyr::mutate(model_label = label) %>% 
		dplyr::mutate(error="perErr")
	
	se <- tidyr::pivot_longer(out.plot, c(se_edm, se_mve, se_lar, se_ric), 
														names_to="forMethod", 
														names_prefix = "se_", 
														values_to="value")
	se <- se %>% dplyr::select(m,forMethod, value) %>% 
		dplyr::mutate(model_label = label) %>% 
		dplyr::mutate(error="se")
	
	errors <- errors %>% dplyr::add_row(ae) %>% dplyr::add_row(pe) %>% 
		dplyr::add_row(se)
	
	
	# Pivot forcasted estimates into a long table for plotting with ggplot
	forecasts <- tidyr::pivot_longer(out.plot, c(R_edm, 
																							 R_mve,
																							 R_lar,
																							 R_ric), 
																			 names_to="forMethod", 
																			 names_prefix = "R_", 
																			 values_to="R_fit") 
	forecasts <- forecasts %>% 
		dplyr::select(m,forMethod, R_fit, R_sim) %>% 
		dplyr::mutate(model_label = label)
	
	return(list(forecasts = forecasts,
							errors = errors)	)
}
