##' Create calculations for bifurcation-type plot for deterministic simulations
##'
##' Calculates the last values for a deterministic run for different values of (for
##'   now) alpha.
##' @param alpha_vec Vector of alpha values to use.
##' @param last How many of the final time steps to save.
##' @param T Number of years to run the simulation.
##' @param new_plot Start a new plot or not.
##' @param ... Further inputs for salmon_sim().
##' @return matrix of final R_t values as columns, with each column
##'   corresponding to a value of alpha
##' @export
salmon_bif <- function(alpha_vec = seq(0.01, 10, by=0.01),
                       last = 50,
                       T = 10000,
                       new_plot=TRUE,
                       ...){
  R_last <- matrix(nrow = last,
                   ncol = length(alpha_vec))
                                    # Each column is last R_t for a
                                    #  value of alpha
  for(j in 1:(length(alpha_vec))){
    R_last[,j] <- salmon_sim(alpha = alpha_vec[j],
                             T = T,
                             deterministic = TRUE,
                             ...
                             )$R_t[(T-last+1):T]
  }
  return(R_last)
}
##' Plot a bifurcation-type diagram
##'
##' Plot the last values of recruitment against alpha.
##' @param x Output from salmon_bif()
##' @param alpha_vec Vector of alpha values to try
##' @return Figure gets plotted
##' @export
plot_salmon_bif <- function(alpha_vec,
                            x,
                            col = "black",
                            ...){
  matplot(alpha_vec,
          t(x),
          pch = 20,
          col=col,
          cex=0.1,
          xlab = "alpha",
          ylab = "Final R_t values",
          ...)
}
##' Calculate and plot bifurcation diagram for Larkin model
##'
##' Defaults give great looking bifurcation diagram (need to work out what they
##'   reduce the model to). Bifurcation diagram calculated by simple simulation.
##' @param alpha_vec Vector of alpha values to use.
##' @param col Colour of plotted points (can be "red" etc., or 1:6).
##' @param ... Further inputs for salmon_bif().
##' @param p_prime Vector of typical proportion of recruits spawned in a year that will
##'   come back to freshwater as age-3, age-4 and age-5, for input into salmon_sim().
##' @param T Number of years to run the simulation. May need longer to see full structure.
##' @return (invisible) matrix of final R_t values as columns, with each column
##'   corresponding to a value of alpha.
##' @examples
##' \dontrun{
##'   # beautiful bifurcation diagram:
##'   salmon_bif_run(alpha_vec = seq(0.01, 40, by = 0.01), beta = c(0.8, 0, 0,
##'     0), T = 1000, col = 1:6)
##'   salmon_bif_run(alpha_vec = seq(0.01, 40, by = 0.01), beta = c(0.8, 0, 0,
##'     0), T = 100, col = 1:6)  # quicker, contains some nice transients
##'   salmon_bif_run()           # the mixing up of density dependence smears
##'                              #   the plot a litte
##'
##'   salmon_bif_run(T=100, col=1:6)   # squid plot, contains transients
##'   salmon_bif_run(col=1:6)   # squid plot, but blurry than one above (!?)
##'
##'   alpha_vec <- seq(1.231, 1.27, by = 0.001)
##'   x <- salmon_bif_run(alpha_vec = seq(1.231, 1.27, by = 0.001), T = 10000)
##'   tail(x[,17:23])            # shows bifurcation at 1.25
##'
##'   salmon_bif_run(beta = c(0.8, 0, 0, 0), p_prime=c(1, 0, 0))
##'   abline(v=exp(2)/0.8)     # Should reduce to simple Ricker model with
##'   harvesting, with analytically calculation bifurcation point at
##'   the vertical line alpha = exp(2)/(1-h).
##'
##'   salmon_bif_run(p_prime = c(0.1, 0.8, 0.1), T = 10000)  # shows some
##'     structure, bifurcation point has moved to the right
##'   salmon_bif_run(alpha_vec = seq(25, 27, by = 0.001), p_prime = c(0.1, 0.8,
##'     0.1), T = 10000)   # Blow up an interesting region
##' }
##' @export
salmon_bif_run <- function(alpha_vec = seq(0.01, 30, by=0.01),
                           # beta = c(0, 0.8, 0, 0),
                           p_prime = c(0, 1, 0),
                           T = 1000,
                           col = "black",
                           # col = 1:6 shows up some rich structure
                           ...){
  x <- salmon_bif(alpha_vec = alpha_vec,
                  # beta = beta,
                  p_prime = p_prime,
                  T = T,
                  ...)
  plot_salmon_bif(alpha_vec,
                  x,
                  col = col)
  invisible(x)
}
