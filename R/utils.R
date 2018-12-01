#' Centers and scales a matrix robustly for the purpose of covariance estimation.
#'
#' Centers each column on its median, and scales each column by its median absolute deviation (MAD).
#'  If any column MAD is zero, its values become zero and a warning is raised. If all MADs are 
#'  zero, an error is raised. 
#'
#' @param mat A numerical matrix.
#'
#' @return The input matrix centered and scaled. 
scale_med <- function(mat){
	# X is nxp; we want to scale the columns.
	n <- nrow(mat)
	p <- ncol(mat)

	ctr <- colMedians(mat, na.rm=TRUE)
	mad <- 1.4826 * colMedians(abs(t(t(mat) - ctr)), na.rm=TRUE)

	# Check for voxels with MAD == 0.
	zero_mad <- mad == 0
	if(any(zero_mad)){
		if(all(zero_mad)){
		stop("All voxels are zero-variance.\n")
		} else {
			warning(cat("Warning: ", sum(zero_mad),
				" zero-variance voxels (out of ", length(zero_mad),
				"). These will be set to zero for estimation of the covariance.\n", sep=""))
		}
	}

	mat_scaled <- t((t(mat)-ctr)/mad)
	return(mat_scaled)
}

#' Computes the median absolute deviation for columns in a matrix.
#'
#' @param mat A numerical matrix.
#'
#' @return A vector with the median absolute deviations of each column in the input matrix.
get_mad <- function(mat){
	n <- nrow(mat)
	p <- ncol(mat)

	ctr <- colMedians(mat, na.rm=TRUE)
	ctr_mat <- matrix(ctr, nrow=n, ncol=p, byrow=TRUE)
	mad <- 1.4826 * colMedians(abs(mat - ctr_mat), na.rm=TRUE)
	return(mad)
}

#' Computes the log likelihood of a sample of values from an F distribution. 
#'
#' @param par A vector of length two which contains the degrees of freedom values.
#' @param vals A vector of values for which log likelihood will be calculated.
#' @param cutoff Values greater than the cutoff are removed before log likelihood calculation.
#'
#' @return A scalar which represents the log likelihood.
logL.F <- function(par, vals, cutoff){
	df1 <- par[1]
	df2 <- par[2]
	vals <- vals[vals <= cutoff]
	return(-1*sum(log(df(vals, df1, df2)) - log(pf(cutoff, df1, df2))))
}

#' Computes the log likelihood of a sample of values from a log normal distribution. 
#'
#' @param par A vector of length two which contains the degrees of freedom values.
#' @param vals A vector of values for which log likelihood will be calculated.
#' @param cutoff Values greater than the cutoff are removed before log likelihood calculation.
#'
#' @return A scalar which represents the log likelihood.
logL.lnorm <- function(par, vals, cutoff){
	mean <- par[1]
	sd <- par[2]
	vals <- vals[vals <= cutoff]
	return(-1*sum(log(dlnorm(vals, mean, sd)) - log(plnorm(cutoff, mean, sd))))
}


