##' @title Calculate forecasting errors 
##' @description 
##' @param run output from fit_models() for EDM, Larkin and Ricker models
##' @return List object call output with components:
##'  - `absErr` absolute error in forecasts from all models in last year
##' @export
##' @author Carrie Holt and Andrew Edwards
##' @examples
##' \dontrun{
##' }

forecast_errors <- function(run){
	
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
	run.out <- res_realisations_plot %>% dplyr::mutate(absErr_edm = 
																										 	(R_edm - R_sim), 
																										 absErr_lar = 
																										 	(R_lar - R_sim),
																										 absErr_ric = 
																										 	(R_ric - R_sim))
	
	# Pivot absolute errors into a long table for plotting with ggplot
	absErr <- tidyr::pivot_longer(run.out, c(absErr_edm, absErr_lar, absErr_ric), 
																names_to="forMethod", 
																names_prefix = "absErr_", 
																values_to="absErr")
	absErr <- absErr %>% select(m,forMethod, absErr)
	
	return(list(absErr=absErr)	)
}
