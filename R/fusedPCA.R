#' Check fusedPCA args
#' 
#' Check fusedPCA arguments for both R-core and cpp-core versions
#' 
#' @param X,X.svd,solve_directions,K,lambda,niter_max,TOL,verbose See respective
#'  functions.
#' @return \code{NULL}, invisibly.
#' @keywords internal
fusedPCA_check_kwargs <- function(X, X.svd, solve_directions, K, lambda, niter_max, TOL, verbose){
  stopifnot(is.numeric(X))
  stopifnot(all(sort(tolower(names(X.svd)))  == sort(c("u", "d", "v"))))
  stopifnot(is.logical(solve_directions))
  stopifnot(is.null(K) || (is.numeric(K) && K==round(K)))
  stopifnot(is.numeric(lambda))
  stopifnot(lambda > 0)
  stopifnot(is.numeric(niter_max))
  stopifnot(niter_max==round(niter_max))
  stopifnot(is.numeric(TOL))
  stopifnot(TOL > 0)
  stopifnot(is.logical(verbose))
  NULL
}

#' Fused PCA
#' 
#' From: https://github.com/Lei-D/fusedPCA . Requires the \code{glmgen} package.
#'
#' @param X A numerical data matrix (observations x variables).
#' @param X.svd (Optional) The svd decomposition of X. Save time by providing
#'  this argument if the svd has already been computed. Default NULL.
#' @param solve_directions Should the principal directions be solved for? These
#'	will be needed to display the artifact images for outlying observations.
#' @param K (Optional) The number of fused PCs to solve for. If not
#'  provided, it will be set to the number of regular PCs with variance above
#'	the mean, up to 100 PCs.
#' @param lambda The trend filtering parameter; roughly, the filtering intensity.
#'	Default is 0.5 . Can be NULL (lets algorithm decide).
#' @param niter_max The number of iterations to use for approximating the PC.
#' @param TOL The maximum 2-norm between iterations to accept as convergence.
#' @param verbose Print statements about convergence?
#'
#' @return SVD The fused SVD decomposition of X (list with u, d, v).
#' 
#' @examples
#' \dontrun{
#' if (requireNamespace("glmgen", quietly = TRUE)) {
#'  U = matrix(rnorm(100*3),ncol=3)
#'  U[20:23,1] = U[20:23,1] + 3
#'  U[40:43,2] = U[40:43,2] - 2
#'  U = svd(U)$u
#'  D = diag(c(10,5,1))
#'  V = svd(matrix(rnorm(3*20),nrow=20))$u
#'  X = U %*% D %*% t(V)
#'  out3 = fusedPCA(X, K=3, lambda=.75)
#'  matplot(out3$u, ty='l')
#'  out3$d
#'  plot(rowSums(out3$u^2), ty='l')
#'
#'  # Orthonormalized
#'  out3_svd = svd(out3$u)
#'  matplot(out3_svd$u, ty='l')
#'  out3_svd$d
#'  plot(rowSums(out3_svd$u^2), ty='l')
#' }
#' }
#' @export
#' 
#' @section References:
#'  \itemize{
#'    \item{Kim, S.-J., Koh, K., Boyd, S. & Gorinevsky, D. l1 Trend Filtering. SIAM Rev. 51, 339-360 (2009).}
#'    \item{Pham, D., McDonald, D., Ding, L., Nebel, M. B. & Mejia, A. Less is more: balancing noise reduction and data retention in fMRI with projection scrubbing (2022).}
#'    \item{Tibshirani, R. J. Adaptive piecewise polynomial estimation via trend filtering. The Annals of Statistics 42, 285-323 (2014).}
#' }
#' 
fusedPCA <- function(
  X, X.svd=NULL, solve_directions = TRUE, K=NULL, lambda=.5,
  niter_max = 1000, TOL = 1e-8, verbose=FALSE){

  if (!requireNamespace("glmgen", quietly = TRUE)) {
    stop("Package \"glmgen\" needed. Please install it from github: statsmaths/glmgen/R_pkg/glmgen.", call. = FALSE)
  }

  # Check arguments.
  fusedPCA_check_kwargs(X, X.svd, solve_directions, K, lambda, niter_max, TOL, verbose)
  if(is.null(X.svd)){
    X.svd <- svd(X)
  } else {
    names(X.svd) <- tolower(names(X.svd))
  }

  if(is.null(K)){
    K <- max(1, sum(X.svd$d^2 > mean(X.svd$d^2)))
    K <- min(100, K)
  }
  if(lambda == 0){
    return(
      list(u = X.svd$u[,seq(K),drop=FALSE],
           d = X.svd$d[seq(K)],
           v = X.svd$v[,seq(K),drop=FALSE]
      )
    )
  }

  N_ <- ncol(X)
  T_ <- nrow(X)

  U <- matrix(NA, nrow = T_, ncol = K)
  D <- rep(NA, K)
  if(solve_directions){ V <- matrix(NA, nrow = N_, ncol = K) }

  all_nIters <- vector("numeric", K)
  all_times <- vector("numeric", K)
  time <- Sys.time()
  for(k in 1:K){
    if (k > 1) { all_times[k-1] <- Sys.time() - time; time <- Sys.time() }
    #if (verbose) { cat("\t", k, "of", K, "\n") }
    # Get initial eigenvector from regular svd.
    u <- X.svd$u[, k]
    d <- X.svd$d[k]
    v <- X.svd$v[, k]

    # Iterate between trendfiltering u and orthonormalizing v.
    for(i in 1:niter_max){
      u.last <- u
      tf.kwargs <- list(y = X %*% v, #scale(X %*% v, center = FALSE, scale = d),
                        x = 1:T_, nlambda = 1, k = 0)
      if(!is.null(lambda)){ tf.kwargs$lambda <- lambda }
      u <- do.call(glmgen::trendfilter, tf.kwargs)$beta
      u <- u / sqrt(sum(u^2))
      if(any(is.na(u))){
        u <- u.last
        if (verbose) { cat("PC",k,":",i,"iterations (NA values from trendfilter) \n") }
        break
      }
      v <- crossprod(X, u)
      v <- v / sqrt(sum(v^2))
      diff <- sqrt(mean((u - u.last)^2))
      if(diff < TOL){ if (verbose) { cat("PC",k,":",i,"iterations\n") }; break }
      if(verbose & i == niter_max){
        cat(paste0('PC ', k, ' did not converge.\n'))
      }
    }
    all_nIters[k] <- i

    d <- crossprod(u, X %*% v)[1, 1]
    X <- X - d * tcrossprod(u, v)
    U[, k] <- u
    D[k] <- d
    if(solve_directions){ V[, k] <- v}
  }
  all_times[k] <- Sys.time() - time
  out <- list(d = D, u = U)
  if(solve_directions){ out$v = V }
  out
}