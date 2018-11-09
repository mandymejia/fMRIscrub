#' Computes the robust kurtosis measure of a set of values.
#'
#' @param x A vector of values.
#'
#' @return A scalar representing the robust kurtosis.
#' @export
#'
#' @examples
#' n = 10
#' x = rnorm(10)
#'
#' rob_kurtosis(x)
rob_kurtosis <- function(x){
	n <- length(x)
	mad_x <- 1.4826*median(abs(x-median(x)))
	kurt <- abs(( ((1/n)*sum((x-median(x))^4)) /  # divide by MAD
				(mad_x^4) )-3)
	return(kurt)
}
