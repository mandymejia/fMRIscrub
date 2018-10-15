#' Computes PCA leverage
#'
#' Computes the leverage of each observation, based on its PC scores.
#'
#' @param U n x Q matrix of PC scores
#'
#' @return a vector of length n with leverage of each observation
#' @export
#'
#' @examples
PCleverage <- function(U){
  return(diag(U %*% t(U)))
}



#' Computes MCD distances based on subsetting the data to reduce autocorrelation
#'
#' fMRI timeseries data is known to exhibit autocorrelation.  The distribution
#' of MCD distances assumes independent observations, so to minimize
#' autocorrelation we split the data into subsets consisting of every 3rd
#' observation. For fMRI data with TR of 2 seconds, this will result in
#' data that is nearly independent within each subset. However, for shorter
#' TR there will still be substantial autocorrelation. In this case we
#' recommend using leverage rather than MCD distance.
#'
#' @param U n x Q matrix of PC scores
#'
#' @return a vector of length n with robust distance measure for each observation
#' @export
#'
#' @examples
PCrobdist_subset <- function(U){

  n <- nrow(U)
  Q <- ncol(U)

  # split data into three subsets
  t <- n #total number of time points
  t2 <- floor(t/3)*3 #truncate time series so t divisible by 3
  starts <- 1:3
  ends <- (t2-2):t2

  # COMPUTE ESTIMATES FOR EACH SUBSET
  ctr <- matrix(nrow=Q, ncol=3) #store 3 estimates of center
  cov <- array(dim=c(Q, Q, 3))  #store 3 estimates of covariance
  inMCD <- rep(FALSE, t)
  for(k in 1:3){ # three subsets
    times.k <- seq(starts[k],ends[k],3)
    U.k <- U[times.k,]
    MCD.k <- covMcd(U.k) #from robustbase package #<--Raises error if n/3 < p
    ctr[,k] <- MCD.k$center
    cov[,,k] <- MCD.k$cov
    inMCD.k <- times.k[MCD.k$mcd.wt==1]
    inMCD[inMCD.k] <- TRUE
  }

  # compute average center and scale across subsets
  ctr <- rowMeans(ctr)
  cov <- apply(cov, c(1,2), mean)

  # compute MCD distances
  mah <- mahalanobis(U, ctr, cov) #compute distance for all time points

  #rescale based on F parameter estimates
  Fparam <- fit.F(Q, t2/3, length(inMCD.k))

  #scale left-out observations to follow F-distribution
  scale <- Fparam$c * (Fparam$m - Q + 1) / (Q * Fparam$m)
  mah[!inMCD] <- scale*mah[!inMCD] #apply scale to obs not in MCD

  #match 10th quantile of sample and F distributions
  mah[!inMCD] <- mah[!inMCD] * qf(0.1, df1=Fparam$df[1], df2=Fparam$df[2]) / quantile(mah[!inMCD], 0.1) #match 10th quantile

  result <- list(mah, inMCD, Fparam)
  names(result) <- c('robdist','inMCD','Fparam')
  return(result)
}

#' Computes MCD distances
#'
#' Computes robust minimum covariance determinant (MCD) distances across
#' the observations (rows).  The MCD method selects a subset of h observations
#' whose covariance matrix has minimum determinant across all subsets of size h.
#' The MCD distances are Mahalanobis distances using the estimates of
#' center (mean) and scale (covariance matrix) based on that subset.
#'
#'
#' @param U n x Q matrix of PC scores
#'
#' @return a vector of length n with robust distance measure for each observation
#' @export
#'
#' @examples
PCrobdist <- function(U){

  n <- nrow(U)
  Q <- ncol(U)

  MCD <- covMcd(U)
  mah <- MCD$mah
  inMCD <- (MCD$mcd.wt==1)
  
  #scale left-out observations to follow F-distribution
  Fparam <- fit.F(Q, n, sum(inMCD))

  #scale left-out observations to follow F-distribution
  scale <- Fparam$c * (Fparam$m - Q + 1) / (Q * Fparam$m)
  mah[!inMCD] <- scale*mah[!inMCD]
  mah[!inMCD] <- scale*mah[!inMCD]

  #match 10th quantile of sample and F distributions
  mah[!inMCD] <- mah[!inMCD] * qf(0.1, df1=Fparam$df[1], df2=Fparam$df[2]) / quantile(mah[!inMCD], 0.1)
  #Above line creates NA.

  result <- list(mah, inMCD, Fparam)
  names(result) <- c('robdist','inMCD', 'Fparam')
  return(result)
}