#' Selects the principle components of greatest variance from a SVD.
#'
#' At most, all PCs with above-average variance are kept. Less may be kept if
#'  outliers will be identified by robust distance, since n_PCs/n_timepoints < 1/2
#'  is required to compute the MCD covariance estimate. (1/6 for the subset method,
#'  since the estimate is computed for each third of the dataset.) Furthermore, 
#'  breakdown point increases as n_PCs-n_timepoints decreases. We required n_PCS
#'  <= n_timepoints/3 to ensure computability and a maximal breakdown point of 1/3, 
#'  if outliers will be identified by robust distance.
#'
#' @param svd An SVD decomposition; i.e. a list containing u, d, and v. 
#' @param id_out if outliers will be identified. 
#' @param method The method which will be used to identify outliers.
#'
#' @return The subsetted u matrix with only the columns (PCs) of above-average mean.
#' @export
#'
#' @examples
choosePCs_mean <- function(svd, id_out = NULL, method = NULL){
	U <- svd$u
	var <- svd$d
	keep <- which(var > mean(var))
	if((id_out == TRUE) & (method %in% c('robdist','robdist_subset'))){
		# Let q = n_PCs/n_timepoints (ncol(U)/nrow(U)).
		# covMcd() requires q <= approx. 1/2.
		# Higher q will use more components for estimation,
		#  thus retaining a higher resolution of information.
		# Lower q will have higher breakdown points,
		#  thus being more resistant to outliers.
		# (The maximal breakdown value is (n_PCs - n_timepoints + 2)/2 )
		# Here, we select q = 1/3 to yield a breakdown value of approx. 1/3.
		q <- 1/3
		n_keep <- min(1, max(ncol(U), floor(switch(method, 
			robdist=nrow(U)*q, robdist_subset=nrow(U)*q/3))))
		keep <- keep[n_keep]
	} 

	U <- U[,keep]
	return(U)
}

#' Selects the principle components of greatest kurtosis from a SVD.
#' 
#' First, PCs with small variance are removed: as many as possible while still
#' retaining 99% of the original variance. Then, at most, all PCs with kurtosis
#' greater than 2 are kept. Less may be kept if outliers will be identified by
#' robust distance, since n_PCs/n_timepoints < 1/2 is required to compute the
#' MCD covariance estimate. (1/6 for the subset method, since the estimate is
#' computed for each third of the dataset.) Furthermore, breakdown point 
#' increases as n_PCs - n_timepoints decreases. We required n_PCS
#' <= n_timepoints/3 to ensure computability and a maximal breakdown point of 1/3, 
#' if outliers will be detected by robust distance.
#'
#' @param svd An SVD decomposition; i.e. a list containing u, d, and v. 
#' @param id_out if outliers will be identified. 
#' @param method The method which will be used to identify outliers.
#'
#' @return The subsetted u matrix with only the columns (PCs) of high kurtosis.
#' @export
#'
#' @examples
choosePCs_kurtosis <- function(svd, id_out = NULL, method = NULL){
	U <- svd$u

	# First remove components that explain less than 99% of variation.
	cumvarexp <- cumsum(svd$d/sum(svd$d))
	keep <- min(which((cumvarexp > .99)))
	U <- U[,keep] 

	# Compute kurtosis of remaining PCs.
	kurt <- apply(U, 2, rob_kurtosis)
	keep <- which(kurt > 2)
	if((id_out == TRUE) & (method %in% c('robdist','robdist_subset'))){
		# Let q = n_PCs/n_timepoints (ncol(U)/nrow(U)).
		# covMcd() requires q <= approx. 1/2.
		# Higher q will use more components for estimation,
		#  thus retaining a higher resolution of information.
		# Lower q will have higher breakdown points,
		#  thus being more resistant to outliers.
		# (The maximal breakdown value is (n_PCs - n_timepoints + 2)/2 )
		# Here, we select q = 1/3 to yield a breakdown value of approx. 1/3.
		q <- 1/3
		n_keep <- min(1, max(ncol(U), floor(switch(method, 
			robdist=nrow(U)*q, robdist_subset=nrow(U)*q/3))))
		keep <- keep[n_keep]
	}

	U <- U[,keep]
	return(U)
}
