#' Which components have high kurtosis?
#'
#' The kurtosis cutoff is a high quantile (default 0.99) of the sampling distribution
#'  of kurtosis for Normal iid data of the same length as the components; it is
#'  estimated by simulation or calculated from the theoretical asymptotic
#'  distribution if the components are long enough.
#' 
#' The components should not have any strong low-frequency trends, because trends
#'  can affect kurtosis in unpredictable ways unrelated to outlier presence. 
#'
#' @param Comps A matrix; each column is a component. For PCA, this is the U
#'  matrix. For ICA, this is the M matrix.
#' @param kurt_quantile components with kurtosis of at least this quantile are kept.
#' @param n_sim The number of simulation data to use for estimating the sampling
#'  distribution of kurtosis. Only used if a new simulation is performed. (If
#'  \eqn{n<1000} and the quantile is 90%, a pre-computed value is used instead.
#'  If \eqn{n>1000}, the theoretical asymptotic distribution is used instead.)
#' @param min_1 Require at least one component to be selected? In other words, if
#'  no components meet the quantile cutoff, should the component with the highest
#'  kurtosis be returned? Default: \code{FALSE}.
#'
#' @return A two-element list: the first a numeric vector with the computed 
#'  kurtosis values for each component, and the second a logical vector 
#'  indicating whether each component has high kurtosis.
#'
#' @importFrom stats quantile qnorm
#' @importFrom e1071 kurtosis
#' @importFrom MASS mvrnorm
#' 
#' @export
high_kurtosis <- function(Comps, kurt_quantile = 0.99, n_sim = 5000, min_1=FALSE){

  Comps <- as.matrix(Comps)
  m <- nrow(Comps); n <- ncol(Comps)

  kurt <- apply(Comps, 2, kurtosis, type=1)

  # Determine the quantile cutoff.
  if(m < 1000){
    if(kurt_quantile == .99){
      # Use precomputed empirical 0.99 quantile.
      cut <- kurt_99_quant[m]
    } else {
      # Simulate and compute the empirical quantile if not 0.90.
      sim <- apply(t(mvrnorm(n_sim, mu=rep(0, m), diag(m))), 2, kurtosis, type=1)
      cut <- quantile(sim, kurt_quantile)
    }
  } else {
    # Use theoretical quantile.
    cut <- qnorm(kurt_quantile) * sqrt( (24*m*(m-1)^2) / ((m-3)*(m-2)*(m+3)*(m+5)) )
  }

  # Constant components will have NaN kurtosis.
  kurt[kurt %in% c(NA, NaN)] <- -Inf

  high <- kurt > cut

  # Keep at least one component if `min_1`.
  if (all(!high) && min_1) { high[which(kurt==max(kurt))[1]] <- TRUE }

  list(kurt=kurt, high=high)
}
