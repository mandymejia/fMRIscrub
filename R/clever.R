#' Calculates PCA leverage or robust distance and identifies outliers.
#'
#' @param x A wide (obs x vars) data matrix of values.
#' @param choosePCs The method to be utilized in choosing which PCs to retain.
#' @param method The method to be utilized in measuring outlyingness.
#' @param id_out If TRUE (default), will label outliers based on leverage or distance.
#'
#' @return A clever object, i.e. a list with components
#' \describe{
#' 	\item{params}{A list with the `choosePCs` and `method` arguments used in the clever call.}
#'  \item{PCs}{The PCs used to calculate leverage or robust distance, i.e. the subsetted U matrix from the SVD.}
#'  \item{leverage}{The leverage of each observation. NULL if `method` is not PCA leverage.}
#'  \item{robdist}{The robust distance of each observation. NULL if `method` is not robust distance (subset).}
#'  \item{inMCD}{Whether each observation is within the MCD subset. NULL if `method` is not robust distance (subset).}
#'  \item{outliers}{An n x 3 data.frame indicating if each observation is an outlier at each of the three levels.}
#' }
#' @export
#'
#' @import stats
#' @importFrom robustbase covMcd
#' @importFrom miscTools colMedians
#'
#' @examples
#' n_voxels = 1e4
#' n_timepoints = 100
#' x = matrix(rnorm(n_timepoints*n_voxels), ncol = n_voxels)
#'
#' lev = clever(x)
clever = function(
	x,
	choosePCs = c('mean','kurtosis'),
	method = c('leverage','robdist_subset','robdist'),
	id_out = TRUE) {

	choosePCs <- match.arg(choosePCs)  # return error if choosePCs arg not one of the acceptable options
	method <- match.arg(method)  # return error if method arg not one of the acceptable options

	x <- as.matrix(x)
	p <- ncol(x)
	n <- nrow(x)
	if(p < n) warning('Data matrix has more rows than columns. 
		Check that observations are in rows and variables are in columns.')

	# Center and scale robustly.
	x <- scale_med(x)

	# Perform dimension reduction.
	XXt <- (x %*% t(x))
	SVDi <- svd(XXt)

	# Choose which PCs to retain.
	choosePCs_fun <- switch(choosePCs, mean=choosePCs_mean, kurtosis=choosePCs_kurtosis)
	if((id_out == TRUE) & (method %in% c('robdist','robdist_subset'))){
		# Let q = n_PCs/n_timepoints (ncol(U)/nrow(U)). robustbase::covMcd() 
		#  requires q <= approx. 1/2 for computation of the MCD covariance estimate.
		#  Higher q will use more components for estimation, thus retaining a 
		#  higher resolution of information. Lower q will have higher breakdown 
		#  points, thus being more resistant to outliers. (The maximal breakdown 
		#  value is (n_PCs - n_timepoints + 2)/2.) Here, we select q = 1/3 to yield 
		#  a breakdown value of approx. 1/3. Since the subset method splits 
		#  n_timepoints into thirds, it must further reduce n_PCs by 1/3. 
		q <- 1/3
		max_keep <- max(1, floor(switch(method, 
			robdist=nrow(SVDi$u)*q, 
			robdist_subset=nrow(SVDi$u)*q/3)))
		U <- choosePCs_fun(SVDi, max_keep=max_keep)
	} else {
		U <- choosePCs_fun(SVDi)
	}
	Q <- ncol(U)

	# Compute PCA leverage or robust distance.
	method_fun <- switch(method, leverage=PCleverage, 
		robdist_subset=PCrobdist_subset, robdist=PCrobdist)
	measure <- method_fun(U)

	if(method %in% c('robdist_subset','robdist')){
		inMCD <- measure$inMCD
		Fparam <- measure$Fparam
		measure <- measure$robdist
	}

	params <- list(choosePCs=choosePCs, method=method)
	if(method == 'leverage'){
		result <- list(params=params, PCs=U, leverage=measure, robdist=NULL, inMCD=NULL, outliers=NULL)
	} else {
		result <- list(params=params, PCs=U, leverage=NULL, robdist=measure, inMCD=inMCD, outliers=NULL)
	}

	# Label outliers, if requested.
	if(id_out){
		if(method=='leverage') result$outliers <- id_out.leverage(measure)
		if(method=='robdist_subset') result$outliers <- id_out.robdist_subset(measure, inMCD, Fparam)
		if(method=='robdist') result$outliers <- id_out.robdist(measure, inMCD, Fparam)
	}

	class(result) <- c('clever', class(result))
	return(result)