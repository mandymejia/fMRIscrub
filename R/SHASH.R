#' SHASH to normal data transformation
#'
#' Transform SHASH-distributed data to normal-distributed data.
#'
#' @param x Numeric vector of data to transform.
#' @param mu Parameter that modulates the mean of \code{x}.
#' @param sigma Parameter that modulates the variance of \code{x}.
#'  Must be greater than zero. This parameter is on the logarithm scale.
#' @param nu Parameter that modulates the skewness of \code{x}.
#' @param tau Parameter that modulates the tailweight of \code{x}.
#'  Must be greater than zero. This parameter is on the logarithm scale.
#' @param inverse Transform normal data to SHASH instead? Default: \code{FALSE}.
#'
#' @return The transformed data.
#' @importFrom fMRItools is_1 is_posNum
#'
#' @export
#'
SHASH_to_normal <- function(x, mu, sigma, nu, tau, inverse = FALSE){
  stopifnot(is.numeric(x))
  stopifnot(is_1(mu, "numeric"))
  stopifnot(is_1(sigma, "numeric"))
  stopifnot(is_1(nu, "numeric"))
  stopifnot(is_1(tau, "numeric"))
  stopifnot(is_1(inverse, "logical"))

  sigma <- exp(sigma)
  tau <- exp(tau)

  xtrans <- if (inverse) {
    # normal to SHASH
    (sigma * tau * sinh((asinh(x) + nu ) / tau)) + mu
  } else {
    # SHASH to normal
    sinh((tau * asinh((x - mu)/ (sigma*tau))) - nu)
  }
}

#' Robust outlier detection based on SHASH distribution
#'
#' A robust outlier detection based on modeling the data as coming from a SHASH
#'  distribution.
#'
#' @param x The numeric vector in which to detect outliers.
#' @param maxit The maximum number of iterations. Default: \code{10}.
#' @param out_lim SD threshold for outlier flagging. Default: \code{4}.
#' @param weight_init Initial weights. Default: \code{NULL} (no pre-determined outliers).
#'
#' @return A \code{"SHASH_out"} object, i.e. a list with components
#' \describe{
#'  \item{out_idx}{Indices of the detected outliers.}
#'  \item{x_norm}{The normalized data.}
#'  \item{SHASH_coef}{Coefficients for the SHASH-to-normal transformation.}
#'  \item{indx_iters}{TRUE for the detected outliers for each itertation.}
#'  \item{last_iter}{Last iteration number.}
#'  \item{converged}{Logical indicating whether the convergence criteria was satisfied or not.}
#' }
#'
#' @importFrom gamlss gamlssML coefAll
#'
#' @export
#'
#' @examples
#' x <- rnorm(100) + (seq(100)/200)
#' x[77] <- 13
#' SHASH_out(x)
#'
SHASH_out <- function(x, maxit = 20, out_lim = 3, weight_init = NULL){
  nL <- length(x)
  if(is.null(weight_init)){
    weight_new <- rep(TRUE, nL) # TRUE if not an outlier
  } else{
    weight_new <- as.logical(weight_init) # can initialize the weight of the outliers
  }
  iter <- 0
  success <- FALSE

  indx_iters <- matrix(NA, nrow = nL, ncol = maxit)
  repeat {
    iter <- iter + 1
    weight_old <- weight_new

    # Transform the data.
    mod <- gamlss::gamlssML(
      x~1,
      family = "SHASHo2",
      maxit = 10000,
      weight = as.numeric(weight_new)
    )
    est <- gamlss::coefAll(mod)
    x_norm <- SHASH_to_normal(
      x = x,
      mu = est$mu, sigma = est$sigma, nu = est$nu, tau = est$tau,
      inverse = FALSE
    )

    # Detect outliers.
    # x_norm_med <- median(x_norm)
    # MAD = (1.4826) * median(abs(x_norm - x_norm_med))
    weight_new <- (x_norm > -out_lim) & (x_norm < out_lim) # TRUE for non-outliers

    # Log outliers on `indx_iters`.
    indx_iters[, iter] = 1 - weight_new

    # Check convergence.
    if (all.equal(weight_old, weight_new) == TRUE) {
      success <- TRUE
      break
    } else if (iter >=  maxit) {
      break
    }
  }

  # Return results.
  out <- list(
    out_idx = which(!weight_new),
    x_norm = x_norm,
    SHASH_coef = est[c("mu", "sigma", "nu", "tau")],
    indx_iters = indx_iters,
    last_iter = iter,
    converged = success
  )
  class(out) <- "SHASH_out"
  out
}

#' Robust empirical rule
#'
#' Robust empirical rule outlier detection applicable to approximately Normal data
#'
#' @param x The data
#' @param thr MAD threshold
#'
#' @return Logical vector indicating whether each element in \code{x} is an
#'  outlier (\code{TRUE} if an outlier).
#' @keywords internal
emprule_rob <- function(x, thr=4){
  x_med <- median(x)
  # Detect outliers.
  MAD = (1.4826) * median(abs(x - x_med))
  lim_left = x_med - thr * MAD
  lim_right = x_med + thr * MAD
  out <- (x < lim_left) | (x > lim_right)

}
