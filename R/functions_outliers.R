#' Identifies outliers based on leverage.
#'
#' Observations with leverage greater than 3, 4, and 5 times the median are labeled as outliers. 
#'
#' @param leverage A vector of length n with the leverage of each observation.
#'
#' @return A list with components
#' \describe{
#' 	\item{outliers}{
#'		An n x 3 data.frame indicating if each observation is an outlier at the 
#'		3*median, 4*median, and 5*median levels.
#'	}
#'	\item{cutoffs}{The leverage cutoff values: 3*median, 4*median, and 5*median.}
#' }
#' @export
id_out.leverage <- function(leverage){
	cutoffs <- 3:5 * median(leverage)
	names(cutoffs) <- paste0(as.character(3:5), 'med')
	out.lev3 <- (leverage > cutoffs[[1]])
	out.lev4 <- (leverage > cutoffs[[2]])
	out.lev5 <- (leverage > cutoffs[[3]])

	out <- data.frame(out.lev3, out.lev4, out.lev5)
	names(out) <- c('3med_outlier','4med_outlier','5med_outlier')
	result <- list(outliers=out, cutoffs=cutoffs)
	return(result)
}

#' Identifies outliers based on robust distance, with adjustment for the subset method.
#'
#' Observations whose robust distance lies in the top 1e-2, 1e-3, and 1e-4
#'  quantiles of the estimated F distribution are labeled as outliers.
#'
#' @param distance A vector of length n with the robust distance estimate of each observation.
#' @param inMCD A vector of length n indicating if each observation is within the MCD subset.
#' @param Q The dimension of each observation. (Remove this?)
#' @param Fparam The estimated parameters of the F distribution of MCD distances.
#'
#' @return A list with components
#' \describe{
#' 	\item{outliers}{
#'		An n x 3 data.frame indicating if each observation is an outlier at the 
#'		1e-2, 1e-3, and 1e-4 quantile levels.
#'	}
#'	\item{cutoffs}{The robust distance cutoff values: the 1e-2, 1e-3, and 1e-4th quantiles}
#' }
#' @export
id_out.robdist_subset <- function(distance, inMCD, Q, Fparam){

	# Distance Outliers (discontiguous time series).
	t <- length(distance)
	t2 <- floor(t/3)*3
	h3 <- sum(inMCD)/3
	c <- Fparam$c
	m <- Fparam$m
	df <- Fparam$df

	# Label outliers.
	cutoffs <- qf(p=c(0.99,0.999,0.9999), df1=df[1], df2=df[2])
	out.mah99 <- out.mah999 <- out.mah9999 <- rep(FALSE, t)
	out.mah99[!inMCD] <- (distance[!inMCD] > cutoffs[1])
	out.mah999[!inMCD] <- (distance[!inMCD] > cutoffs[2])
	out.mah9999[!inMCD] <- (distance[!inMCD] > cutoffs[3])
	out <- data.frame(out.mah99, out.mah999, out.mah9999)
	names(out) <- c('0.99 quantile outlier','0.999 quantile outlier','0.9999 quantile outlier')

	result <- list(outliers=out, cutoffs=cutoffs)
	return(result)
}

#' Identifies outliers based on robust distance.
#'
#' Observations whose robust distance lies in the top 1e-2, 1e-3, and 1e-4
#'  quantiles of the estimated F distribution are labeled as outliers.
#'
#' @param distance A vector of length n with the robust distance estimate of each observation.
#' @param inMCD A vector of length n indicating if each observation is within the MCD subset.
#' @param Q The dimension of each observation. (Remove this?)
#' @param Fparam The estimated parameters of the F distribution of MCD distances.
#'
#' @return A list with components
#' \describe{
#' 	\item{outliers}{
#'		An n x 3 data.frame indicating if each observation is an outlier at the 
#'		1e-2, 1e-3, and 1e-4 quantile levels.
#'	}
#'	\item{cutoffs}{The robust distance cutoff values: the 1e-2, 1e-3, and 1e-4th quantiles}
#' }
#' @export
id_out.robdist <- function(distance, inMCD, Q, Fparam){

	# Distance Outliers (discontiguous time series).
	t <- length(distance)
	c <- Fparam$c
	m <- Fparam$m
	df <- Fparam$df

	# Label outliers.
	cutoffs <- qf(p=c(0.99,0.999,0.9999), df1=df[1], df2=df[2])
	out.mah99 <- out.mah999 <- out.mah9999 <- rep(FALSE, t)
	out.mah99[!inMCD] <- (distance[!inMCD] > cutoffs[1])
	out.mah999[!inMCD] <- (distance[!inMCD] > cutoffs[2])
	out.mah9999[!inMCD] <- (distance[!inMCD] > cutoffs[3])

	out <- data.frame(out.mah99, out.mah999, out.mah9999)
	names(out) <- c('0.99 quantile outlier','0.999 quantile outlier','0.9999 quantile outlier')

	result <- list(outliers=out, cutoffs=cutoffs)
	return(result)
}

