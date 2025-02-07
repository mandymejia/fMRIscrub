#' Estimate SD robustly using the half IQR
#' 
#' Estimates standard deviation robustly using the half IQR (and power trans.).
#'  Used to measure DVARS in Afyouni and Nichols, 2018. 
#'
#' @param x Numeric vector of data to estimate standard deviation for. 
#' @param d The scalar power transformation parameter. \eqn{w = x^{1/d}} is
#'  computed to obtain \eqn{w \sim N(\mu_w, \sigma_w^2)}.
#' 
#' @importFrom stats quantile median
#' 
#' @return Scalar for the robust estimate of standard deviation.
#' 
#' @keywords internal
#' 
sd_hIQR <- function(x, d=1){
  w <- x^(1/d) # Power trans.: w~N(mu_w, sigma_w^2)
  sd <- (quantile(w, .5) - quantile(w, .25)) / (1.349/2) # hIQR
  out <- (d * median(w)^(d - 1)) * sd # Delta method
  # In the paper, the above formula incorrectly has d^2 instead of d.
  # The code on github correctly uses d.
  return(as.numeric(out))
}

#' DVARS
#' 
#' Computes the DSE decomposition and DVARS-related statistics. Based on code
#'  from github.com/asoroosh/DVARS .
#'
#' @param X a \eqn{T} by \eqn{N} numeric matrix representing an fMRI run. There should
#'  not be any missing data (\code{NA} or \code{NaN}).
#' @param normalize Normalize the data? Default: \code{TRUE}. Normalization removes 
#'  constant-zero voxels, scales by 100 / the median of the mean image, and 
#'  then centers each voxel on its mean.
#'
#'  To replicate Afyouni and Nichols' procedure for the HCP MPP data, since the
#'  HCP scans are already normalized to 10,000, just divide the data by 100 and
#'  center the voxels on their means:
#'
#'  \code{Y <- Y/100; DVARS(t(Y - apply(Y, 1, mean)))} where \code{Y} is the 
#'  \eqn{V} by \eqn{T} data matrix.
#' 
#'  Note that while voxel centering doesn't affect DVARS, it does affect
#'  DPD and ZD.
# @param cutoff_DVARS
#' @param cutoff_DPD,cutoff_ZD Numeric outlier cutoffs. Timepoints
#'  exceeding these cutoffs will be flagged as outliers.
#' @param verbose Should occasional updates be printed? Default is \code{FALSE}.
#'
#' @return A list with components
#' \describe{
#'  \item{measure}{A data.frame with \eqn{T} rows, each column being a different variant of DVARS.}
#'  \item{measure_info}{"DVARS"}
#'  \item{outlier_cutoff}{The outlier cutoff value(s).}
#'  \item{outlier_flag}{A logical data.frame with \eqn{T} rows, where \code{TRUE} indicates suspected outlier presence.}
#' }
#' @export
#' @importFrom stats median pchisq qnorm
#' @importFrom fMRItools as.matrix_ifti
#' 
#' @section References:
#'  \itemize{
#'    \item{Afyouni, S. & Nichols, T. E. Insight and inference for DVARS. NeuroImage 172, 291-312 (2018).}
#' }
#' 
DVARS <- function(
  X, normalize=TRUE, 
  cutoff_DPD=5,
  cutoff_ZD=qnorm(1 - .05 / nrow(as.matrix_ifti(X))),
  verbose=FALSE){

  cutoff_DVARS <- NULL

  X <- as.matrix_ifti(X, verbose=verbose)
  T_ <- nrow(X); N_ <- ncol(X)

  cutoff <- list(DVARS=cutoff_DVARS, DPD=cutoff_DPD, ZD=cutoff_ZD)

  if (normalize) {
    # Normalization procedure from original DVARS paper and code.
    # Remove voxels of zeros (assume no NaNs or NAs)
    bad <- apply(X == 0, 2, all)
    if(any(bad)){
      if(verbose){ print(paste0('Zero voxels removed: ', sum(bad))) }
      X <- X[,!bad]
      N_ <- ncol(X)
    }
    
    # Scale the entire image so that the median average of the voxels is 100.
    X <- X / median(apply(X, 2, mean)) * 100

    # Center each voxel on its mean.
    X <- t(t(X) - apply(X, 2, mean))
  }

  # compute D/DVARS
  A_3D <- X^2
  Diff <- X[2:T_,] - X[1:(T_-1),]
  D_3D <- (Diff^2)/4
  A <- apply(A_3D, 1, mean)
  D <- apply(D_3D, 1, mean)
  DVARS_ <- 2*sqrt(D) # == sqrt(apply(Diff^2, 1, mean))

  # compute DPD
  DPD <- (D - median(D))/mean(A) * 100

  # compute z-scores based on X^2 dist.
  DV2 <- 4*D # == DVARS^2
  mu_0 <- median(DV2) # pg 305
  sigma_0 <- sd_hIQR(DV2, d=3) # pg 305: cube root power trans
  v <- 2*mu_0^2/sigma_0^2
  X <- v/mu_0 * DV2 # pg 298: ~X^2(v=2*mu_0^2/sigma_0^2)
  P <- pchisq(X, v)
  ZD <- ifelse(
    abs(P-.5)<.49999, # avoid overflow if P is near 0 or 1
    qnorm(1 - pchisq(X, v)), # I don't understand why they use 1-pchisq(X,v) instead of just pchisq(X,v)
    (DV2-mu_0)/sigma_0  # avoid overflow by approximating
  )

  out <- list(
    measure = data.frame(D=c(0,D), DVARS=c(0,DVARS_), DPD=c(0,DPD), ZD=c(0,ZD)),
    measure_info = setNames("DVARS", "type")
  )

  if ((!is.null(cutoff)) || (!all(vapply(cutoff, is.null, FALSE)))) {
    cutoff <- cutoff[!vapply(cutoff, is.null, FALSE)]
    cutoff <- setNames(as.numeric(cutoff), names(cutoff))
    out$outlier_cutoff <- cutoff
    out$outlier_flag <- out$measure[,names(cutoff)]
    for (dd in seq(ncol(out$outlier_flag))) {
      out$outlier_flag[,dd] <- out$outlier_flag[,dd] > cutoff[colnames(out$outlier_flag)[dd]]
    }
    if (all(c("DPD", "ZD") %in% colnames(out$outlier_flag))) {
      out$outlier_flag$Dual <- out$outlier_flag$DPD & out$outlier_flag$ZD
    }
  }

  structure(out, class="IDvols_DVARS")
}
