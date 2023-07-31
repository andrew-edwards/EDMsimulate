#' Generate log-normal error associated with proportions data
#'
#' This function generates proporations with multivariate logistic error
#' (Schnute and Richards 1995, eqns.S.9 and S.10). Includes an adjustment to
#' skip calculations when dealing with populations that have fixed age struc-
#' tures (e.g. pink salmon).
#'
#' @param ppnVec A numeric vector of proportions
#' \code{ns = length(ppnVec)}.
#' @param tau A numeric specifying the parameter representing average variation
#' in proportions (e.g. observed interannual variation ).
#' @param error A numeric vector of length \code{n} that specifies random
#' values to deviate proportions by (generally generated from a uniform dis-
#' tribution bounded by 0 and 1).
#' @return Returns a numeric vector of length \code{n} representing pro-
#' portions for each class.
#'
#' @examples
#' ppnVec <- c(0.2, 0.4, 0.3, 0.1)
#' n <- length(ppnVec)
#' tau <- 0.7
#' error <- runif(n, 0.0001, 0.9999)
#' ppnErr(ppnVec, tau, error)
#'
#' @export

ppnErr <- function(ppnVec, tau, error) {
	n <- length(ppnVec)
	#NAs produced when dividing 0 by 0 for ppn of recruits at age; replace w/ 0s
	ppnVec[is.na(ppnVec)] <- 0
	p <- rep(0, length.out = n)
	#if any of the average proportions equals one, assume the structure is fixed
	#and adjust output accordingly
	if (max(ppnVec) == 1) {
		pos <- which(ppnVec == 1)
		p[pos] <- 1
	} else {
		ppnVec[ppnVec == 0] <- 0.0000001
		dum <- 0.
		x <- 0.
		dum <- log(ppnVec) + tau * qnorm(error, 0, 1) #vectorized
		x <- log(ppnVec)+ tau * qnorm(error, 0, 1) - (1 / n) * sum(dum)
		p <- exp(x) / sum(exp(x))
	}
	return(p)
}

