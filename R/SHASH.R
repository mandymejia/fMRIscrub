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
#' @param thr0 Initial threshold for weighting. Default: \code{2.58}.
#' @param thr Final threshold for outlier detection. Default: \code{4}.
#' @param symmetric Single scale for the entire data? Default: \code{TRUE}.
#' @param use_huber Use the Huber estimates for center and scale? Default: 
#'  \code{FALSE}.
#' @param upper_only Only consider upper threshold? Default: \code{FALSE}.
#' @param maxit The maximum number of iterations. Default: \code{20}.
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
SHASH_out <- function(
  x, thr0 = 2.58, thr = 4, symmetric = TRUE, use_huber = FALSE, 
  upper_only = FALSE, maxit = 20, weight_init = NULL) {
  
  nL <- length(x)  # Length of univariate data
  
  # Initial weight allocation
  if (is.null(weight_init)) {  # if weight is not initialized by user
    W <- tryCatch(
      1 - emprule_rob(x, thr = thr0, symmetric = symmetric, use_huber = use_huber, upper_only = upper_only),  # Use empirical rule for weights
      error = function(e) rep(TRUE, nL)
    )
    
    weight_new <- as.logical(W)  # Ensure it's a logical vector
    
  } else {
    weight_new <- as.logical(weight_init)  # Initialize from user-provided weights
  }
  
  iter <- 0  # Number of iterations
  success <- FALSE  # Success flag
  indx_iters <- matrix(NA, nrow = nL, ncol = maxit)  # Store index iterations

  repeat {
    iter <- iter + 1
    weight_old <- weight_new

    # Fit SHASH model using weights
    mod <- gamlss::gamlssML(
      x ~ 1,
      family = "SHASHo2",
      maxit = 10000,
      weights = as.numeric(weight_new)
    )
    est <- gamlss::coefAll(mod)

    # Convert data to normal based on SHASH estimates
    x_norm <- SHASH_to_normal(
      x = x,
      mu = est$mu, sigma = est$sigma, nu = est$nu, tau = est$tau,
      inverse = FALSE
    )

    # Apply empirical rule for new weights
    weight_new <- as.logical(1 - emprule_rob(x_norm, thr = thr0, symmetric = symmetric, use_huber = use_huber, upper_only = upper_only))
    
    # Log outliers on `indx_iters`
    indx_iters[, iter] <- 1 - weight_new

    # Check convergence
    if (isTRUE(all.equal(weight_old, weight_new))) {
      success <- TRUE
      break
    } else if (iter >= maxit) {
      break
    }
  }
  
  # **Modify final outlier selection based on `upper_only`**
  if (upper_only) {
    final_out_idx <- which(x_norm > thr)  # Only consider upper threshold outliers
  } else {
    final_out_idx <- which(abs(x_norm) > thr)  # Symmetric thresholding (original behavior)
  }

  # Return results
  out <- list(
    out_idx = final_out_idx,  # Weight final, final threshold weighting based on thr
    x_norm = x_norm,
    SHASH_coef = est[c("mu", "sigma", "nu", "tau")],
    indx_iters = indx_iters,
    last_iter = iter,
    converged = success
  )
  class(out) <- "SHASH_out"
  return(out)
}

#' Robust empirical rule
#'
#' Robust empirical rule outlier detection applicable to approximately Normal data
#'
#' @param x The data
#' @param thr MAD threshold
#' @param symmetric Single scale for the entire data? Default: \code{TRUE}.
#' @param use_huber Use the Huber estimates for center and scale? Default: 
#'  \code{FALSE}.
#' @param upper_only Only consider upper threshold? Default: \code{FALSE}.
#'
#' @return Logical vector indicating whether each element in \code{x} is an
#'  outlier (\code{TRUE} if an outlier).
#' @importFrom MASS huber
#' @keywords internal
emprule_rob <- function(x, thr = 4, symmetric = TRUE, use_huber = FALSE, upper_only = FALSE) {
  
  # Validate inputs
  if (!is.numeric(x)) stop("Input data 'x' must be numeric.")
  if (!is.numeric(thr) || length(thr) != 1 || thr <= 0) {
    stop("Threshold 'thr' must be a positive numeric value.")
  }
  
  # Error handling for upper_only mode
  if (use_huber && upper_only) {
    stop("Cannot use `upper_only = TRUE` when `use_huber = TRUE`. Set `use_huber = FALSE`.")
  }
  
  # Error handling for use_huber mode
  if (use_huber && !symmetric) {
    stop("Cannot use `use_huber = TRUE` when `symmetric = FALSE`. Set `symmetric = TRUE`.")
  }
  
  # Calculate the center and scale
  x_med <- median(x, na.rm = TRUE)
  
  if (use_huber) {
    # Use Huber's estimate for location and scale
    huber_fit <- MASS::huber(x)
    center <- huber_fit$mu
    scale <- huber_fit$s
    
  } else if (symmetric) {
    # Use Median Absolute Deviation (MAD) scaled to standard deviation
    MAD <- mad(x, na.rm = TRUE)
    center <- x_med
    scale <- MAD
    
  } else {
    # Asymmetric: Calculate separate left and right scales
    xl <- x[x < x_med]  # Points to the left of the median
    xr <- x[x > x_med]  # Points to the right of the median
    
    left_mad <- 1.4826 * median(abs(xl - x_med), na.rm = TRUE)
    right_mad <- 1.4826 * median(abs(xr - x_med), na.rm = TRUE)
    center <- x_med
  }

  # Calculate thresholds
  if (symmetric) {
    lim_left <- center - thr * scale
    lim_right <- center + thr * scale
  } else {
    lim_left <- center - thr * left_mad
    lim_right <- center + thr * right_mad
  }

  # Identify outliers
  if (upper_only) {
    out <- x > lim_right  # Only filter upper threshold values
  } else {
    out <- (x < lim_left) | (x > lim_right)  # Default: Detect both upper & lower outliers
  }
  
  return(out)
}