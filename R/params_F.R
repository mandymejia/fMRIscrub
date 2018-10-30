#' Estimates the parameters of the F distribution of MCD distances.
#'
#' This estimates the parameters c and m required to determine the distribution
#'  of robust MCD distances as derived by Hardin and Rocke (2005), The Distribution
#'  of Robust Distances.
#'
#' @param Q The number of variables in dataset used to compute MCD distances.
#' @param n The total number of observations.
#' @param h The number of observations included in estimation of MCD center and scale.
#'
#' @return A list containing the estimated F distribution's c, m, and df.
#' @export
fit.F <- function(Q, n, h){
	# Estimate c.
	c <- pchisq(q=qchisq(df=Q, p=h/n), df=Q+2)/(h/n)

	# Estimate asymptotic m.
	alpha <- (n-h)/n
	q_alpha <- qchisq(p=1-alpha, df=Q)
	c_alpha <- (1-alpha)/(pchisq(df=Q+2, q=q_alpha))
	c2 <- -1*pchisq(df=Q+2, q=q_alpha)/2
	c3 <- -1*pchisq(df=Q+4, q=q_alpha)/2
	c4 <- 3*c3
	b1 <- c_alpha*(c3-c4)/(1-alpha)
	b2 <- 0.5 + (c_alpha/(1-alpha))*(c3-q_alpha/Q*(c2 + (1-alpha)/2))
	v1 <- (1-alpha)*b1^2*(alpha*(c_alpha*q_alpha/Q - 1)^2 - 1) -
		2*c3*c_alpha^2*(3*(b1-Q*b2)^2 + (Q+2)*b2*(2*b1-Q*b2))
	v2 <- n*(b1*(b1-Q*b2)*(1-alpha))^2*c_alpha^2
	v <- v1/v2
	m <- 2/(c_alpha^2*v)

	# Corrected m for finite samples.
	m <- m * exp(0.725 - 0.00663*Q - 0.078*log(n))
	df <- c(Q, m-Q+1)

	result <- list(c=c, m=m, df=df)
	return(result)
}

