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
	if(names(run$res_realisations)[2]=="R_prime_T_sim"){
		res_realisations_plot <- run$res_realisations %>% 
			dplyr::rename( R_sim = R_prime_T_sim) %>% 
			dplyr::rename( R_edm = R_prime_T_edm_fit) %>% 
			dplyr::rename( R_lar = R_prime_T_lar_fit) %>% 
			dplyr::rename( R_ric = R_prime_T_ric_fit)
	}
	if(names(run$res_realisations)[2]=="R_T_sim"){
		res_realisations_plot <- run$res_realisations %>% 
			dplyr::rename( R_sim = R_T_sim) %>% 
			dplyr::rename( R_edm = R_T_edm_fit) %>% 
			dplyr::rename( R_lar = R_T_lar_fit) %>% 
			dplyr::rename( R_ric = R_T_ric_fit)
	}
	
	# Calculate absolute error in forecast for EDM, Larkin and Ricker
	out.plot <- res_realisations_plot %>% dplyr::mutate(absErr_edm = 
																										 	(R_edm - R_sim), 
																										 absErr_lar = 
																										 	(R_lar - R_sim),
																										 absErr_ric = 
																										 	(R_ric - R_sim))
	# add percent error, mse, and make absErr absolute (only positive)
	
	# Pivot absolute errors into a long table for plotting with ggplot
	absErr <- tidyr::pivot_longer(out.plot, c(absErr_edm, absErr_lar, absErr_ric), 
																names_to="forMethod", 
																names_prefix = "absErr_", 
																values_to="absErr")
	absErr <- absErr %>% select(m,forMethod, absErr) %>% 
		dplyr::mutate(model_label = label)
	
	# Pivot forcasted estimates into a long table for plotting with ggplot
	forecasts <- tidyr::pivot_longer(out.plot, c(R_edm, 
																								 R_lar,
																								 R_ric), 
																			 names_to="forMethod", 
																			 names_prefix = "R_", 
																			 values_to="R_fit") 
	forecasts <- forecasts %>% 
		dplyr::select(m,forMethod, R_fit, R_sim) %>% 
		dplyr::mutate(model_label = label)
	
	return(list(forecasts = forecasts,
							absErr = absErr)	)
}
