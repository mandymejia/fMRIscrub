#' Calculates PCA leverage or robust distance and identifies outliers
#'
#' @param x wide (obs x vars) data matrix of values
#' @param choosePCs method to be utilized in choosing which PCs to retain
#' @param method method to be utilized in measuring outlyingness
#' @param id_out if TRUE (default), will label outliers based on leverage or distance
#'
#' @return A list containing PC scores, number of PCs retained, leverage or distance and (if id.out=TRUE) outlier indicator vectors
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

  choosePCs <- match.arg(choosePCs) #return error if choosePCs arg not one of the acceptable options
  method <- match.arg(method) #return error if method arg not one of the acceptable options

  x <- as.matrix(x)
  p <- ncol(x)
  n <- nrow(x)
  if(p < n) warning('Data matrix has more rows than columns.  Check that observations are in rows and variables are in columns.')

  #center and scale robustly
  x <- scale_med(x)

  #perform dimension reduction
  XXt <- (x %*% t(x))
  SVDi <- svd(XXt)

  #choose which PCs to retain
  choosePCs_fun <- switch(choosePCs, mean=choosePCs_mean, kurtosis=choosePCs_kurtosis)
  U <- choosePCs_fun(SVDi, method)
  Q <- ncol(U)

  #compute PCA leverage or robust distance
  method_fun <- switch(method, leverage=PCleverage, robdist_subset=PCrobdist_subset, robdist=PCrobdist)
  if(method == 'robdist_subset'){
    if(nrow(U)/3 < ncol(U)){
      stop("Not enough time points to estimate MCD for this many voxels.")
    }
  } else if(method == 'robdist') {
    if(nrow(U) < ncol(U)){
      stop("Not enough time points to estimate MCD for this many voxels.")
    }
  }
  measure <- method_fun(U)

  if(method %in% c('robdist_subset','robdist')){
    inMCD <- measure$inMCD
    Fparam <- measure$Fparam
    measure <- measure$robdist
  }

  params = list(choosePCs=choosePCs, method=method)
  if(method == 'leverage'){
    result <- list(params=params, PCs=U, leverage=measure, robdist=NULL, inMCD=NULL)
  } else {
    result <- list(params=params, PCs=U, leverage=NULL, robdist=measure, inMCD=inMCD)
  }

  #label outliers
  if(id_out){
    if(method=='leverage') outliers <- id_out.leverage(measure)
    if(method=='robdist_subset') outliers <- id_out.robdist_subset(measure, inMCD, Q, Fparam)
    if(method=='robdist') outliers <- id_out.robdist(measure, inMCD, Q, Fparam)
    result <- c(result, outliers)
  }

  return(result)

}