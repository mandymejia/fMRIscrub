#' Selects the principle components of greatest variance from a SVD.
#'
#' PCs with above-average variance are kept. The total number kept can be
#' constrained within a range using the `max_keep` and min_keep` arguments.
#' PCs with greater variance will be prioritized for keeping. 
#'
#' @param svd An SVD decomposition; i.e. a list containing u, d, and v. 
#' @param max_keep If specified, the total number kept will be at most this value.
#' @param min_keep If specified, the total number kept will be at least this value.
#'
#' @return The subsetted u matrix with only the chosen columns (PCs).
#' @export
#'
#' @examples
choosePCs_mean <- function(svd, max_keep=NULL, min_keep=NULL){
	U <- svd$u
	var <- svd$d

	# Identify how many PCs will be kept.
	n_keep <- sum(var > mean(var))

	# Constrain this number kept between the minumum and maximum, if specified.
	if(!is.null(max_keep)){n_keep <- min(n_keep, max_keep)}
	if(!is.null(min_keep)){n_keep <- max(n_keep, min_keep)}

	# PCs are already ordered by decreasing variance.
	U <- U[,1:n_keep]
	return(U)
}

#' Selects the principle components of greatest kurtosis from a SVD.
#' 
#' PCs with kurtosis greater than 2 are kept. The total number kept can be
#' constrained within a range using the `max_keep` and min_keep` arguments.
#' PCs with greater kurtosis will be prioritized for keeping. 
#'
#' @param svd An SVD decomposition; i.e. a list containing u, d, and v. 
#' @param max_keep If specified, the total number kept will be at most this value.
#' @param min_keep If specified, the total number kept will be at least this value.
#'
#' @return The subsetted u matrix with only the chosen columns (PCs).
#' @export
#'
#' @examples
choosePCs_kurtosis <- function(svd, max_keep=NULL, min_keep=NULL){
	U <- svd$u

	# First remove components that explain less than 99% of variation.
	cumvarexp <- cumsum(svd$d/sum(svd$d))
	n_keep <- min(which((cumvarexp > .99)))

	U <- U[,1:n_keep] 

	# Compute kurtosis of remaining PCs.
	kurt <- apply(U, 2, rob_kurtosis)

	# Identify how many PCs will be kept.
	n_keep <- sum(kurt > 2)

	# Constrain the number kept between the minumum and maximum, if specified.
	if(!is.null(max_keep)){n_keep <- min(n_keep, max_keep)}
	if(!is.null(min_keep)){n_keep <- max(n_keep, min_keep)}

	# The PCs with greatest kurtosis are chosen.
	to_keep = order(-kurt)[1:n_keep]
	U <- U[,to_keep]
	return(U)
}
