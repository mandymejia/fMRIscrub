#' Robust distance calculation
#'
#' Compute robust distance
#'
#' @param data The dimension reduced and selected data.
#' @param ind_incld Indices of the included subset of \code{data}.
#' @param dist Compute the robust distances? Default: \code{TRUE}.
#'
#' @return A list with parameters and optionally the robust distances.
#'
#' @importFrom expm sqrtm
#' @keywords internal
RD_meas <- function(data, ind_incld, dist=TRUE){
 #test
  t <- dim(data)[1]
  Q <- dim(data)[2]
  h <- length(ind_incld)

  data_incld <- data[ind_incld,]
  xbar_star <- colMeans(data_incld)
  sc_data_incld <- scale(data_incld,  center = TRUE, scale = FALSE)
  S_star <- (t(sc_data_incld) %*% sc_data_incld) / (h-1)
  # [TO DO] if singular? example: robdist(Dat1[seq(70),seq(1000)])
  invcov <- solve(S_star)
  invcov_sqrt <- sqrtm(invcov)

  if (dist) {
    xbar_star_mat <- matrix(xbar_star, nrow = t, ncol = Q, byrow = TRUE)
    temp <- (data - xbar_star_mat) %*% invcov_sqrt
    RD <- rowSums(temp * temp)
  } else {
    RD <- NULL
  }

  list(xbar_star=xbar_star, S_star=S_star, invcov_sqrt=invcov_sqrt, RD=RD)
}

#' Univariate outlier detection for robust distance
#'
#' Identify the univariate outliers with robust distance.
#'
#' @param data The data
#' @param trans Transform the data? Default: \code{"none"}. The other choice is
#'  \code{"robust-YJ"}. The \code{"SHASH"} method has not been implemented yet.
#' @param cutoff Default: \code{4}.
#'
#' @return The univariate outliers.
#'
#' @importFrom cellWise transfo
#' @keywords internal
RD_univOut <- function(
  data,
  cutoff = 4,
  trans = c("none", "robust-YJ", "SHASH")){

  trans <- match.arg(trans, c("none", "robust-YJ", "SHASH"))

  t <- dim(data)[1]
  Q <- dim(data)[2]
  univOut <- matrix(F,t,Q)
  if (trans == "none") {
    for (ii in seq(Q)) {
      temp <- data[,ii]
      temp_med <- median(temp, na.rm = TRUE)
      mad <- 1.4826 * median(abs(temp-temp_med), na.rm = TRUE)
      cutoff1 <- temp_med - (cutoff*mad)
      cutoff2 <- temp_med + (cutoff*mad)
      ind_out <- which(temp <= cutoff1 | temp >= cutoff2)
      univOut[ind_out,ii] <- TRUE
    }
  } else if (trans=="robust-YJ") {
    for (ii in seq(Q)) {
      temp <- data[,ii]
      trans_temp <- (cellWise::transfo(temp, type = "YJ",robust = TRUE, prestandardize = TRUE))$Xt
      medi <- median(trans_temp)
      MAD <- median(abs(trans_temp - medi))
      STD <- 1.4826 * MAD
      cutoff1 <- medi - (cutoff*STD)
      cutoff2 <- medi + (cutoff*STD)
      ind_out <- which(trans_temp <= cutoff1 | trans_temp >= cutoff2)
      univOut[ind_out,ii] <- TRUE
    }
  } else {
    stop("SHASH not implemented yet.")
  }
  univOut
}

#' Impute outliers for robust distance
#'
#' Impute the outliers for robust distance.
#'
#' @param data The data
#' @param univOut The univariate outliers
#'
#' @keywords internal
#' @return The data with imputed outliers.
RD_impData <- function(data, univOut){
  t = nrow(data)
  Q = ncol(data)
  impData <- data

  for (ii in seq(Q)) {
    temp <- data[,ii]
    univOut_ii <- univOut[,ii]
    ind_univOut <- which(univOut_ii == T)
    if (length(ind_univOut) == 0) next
    temp[ind_univOut] <- NA

    univOut[ind_univOut,ii] = TRUE
    for (jj in ind_univOut) {
      temp_L = temp[seq(jj)]
      temp_L_indx = which(!is.na(temp_L))
      L_indx <- max(temp_L_indx, na.rm = TRUE)
      mean_L <- temp_L[L_indx]

      temp_R = temp[seq(jj, length(temp))]
      temp_R_indx = which(!is.na(temp_R))
      R_indx = min(temp_R_indx, na.rm=TRUE)
      mean_R <- temp_R[R_indx]

      if (is.infinite(mean_L)) {mean_L <- mean_R}
      if (is.infinite(mean_R)) {mean_R <- mean_L}

      temp[jj] = mean(c(mean_L, mean_R), na.rm = TRUE)
    }
    impData[,ii] <- temp
  }

  impData
}

#' Robust distance scrubbing
#'
#' Scrubbing with robust distance.
#'
#' @inheritParams pscrub_Params
#' @param RD_cutoff Default: \code{4}.
#' @param RD_quantile Quantile cutoff...?
#' @param trans Apply a transformation prior to univariate outlier detection?
#'  Three options: \code{"none"} (default), \code{"robust-YJ"}, and
#'  \code{"SHASH"}.
#' @param bootstrap_n Use bootstrapping to estimate the robust distance null
#'  distribution? If so, set this to the number of bootstraps. Default:
#'  \code{100}. Use \code{0} (or \code{FALSE}), to use an empirical quantile
#'  instead.
#' @param bootstrap_alpha If using bootstrap (\code{bootstrap > 0}), this is the
#'  level of the bootstrap CI. Default: \code{0.99}.
#' @param projection One of the following: \code{"ICA"} (default) or \code{"PCA"}.
#  or \code{"fusedPCA"}.
# @param R_true The \eqn{T} by \eqn{T} correlation matrix, if known. Used for the bootstrap
#  robust distance measure.
#' @param PESEL Use \code{\link[pesel]{pesel}} to select the number of
#'  components? Default: \code{TRUE}. Otherwise, use the number of principal
#'  components with above-average variance.
#' @param skip_dimred Skip dimension reduction? Default: \code{FALSE}.
#' @return A \code{"robdist"} object, i.e. a list with components
#' \describe{
#'  \item{lwr_50}{...}
#'  \item{lwr_80}{...}
#'  \item{B_quant}{...}
#' }
#'
#' @export
#'
#' @importFrom MASS cov.mcd
#' @importFrom fMRItools is_1
#'
#' @examples
#' library(fastICA)
#' rdx = robdist(Dat1[seq(70),seq(800,950)])
robdist = function(
  X,
  RD_cutoff = 4,
  RD_quantile = 0.99,
  trans = c("none", "robust-YJ", "SHASH"),
  bootstrap_n = 1000,
  bootstrap_alpha = 0.01,
  projection=c(
    "ICA",
    #"fusedPCA",
    "PCA"
  ),
  nuisance="DCT4",
  center=TRUE, scale=TRUE, comps_mean_dt=FALSE, comps_var_dt=FALSE,
  PESEL=TRUE, kurt_quantile=.99,
  #fusedPCA_kwargs=NULL,
  get_dirs=FALSE, full_PCA=FALSE,
  get_outliers=TRUE, cutoff=4, seed=0, ICA_method=c("C", "R"),
  skip_dimred=FALSE,
  verbose=FALSE){

  stopifnot(is_1(skip_dimred, "logical"))

  # Run `pscrub`.
  if (!skip_dimred) {
    projection <- match.arg(projection, c(
      "ICA",
      #"fusedPCA",
      "PCA"
    ))
    data_ps <- pscrub(
      X=X, projection=projection,
      nuisance=nuisance,
      center=center, scale=scale, comps_mean_dt=comps_mean_dt, comps_var_dt=comps_var_dt,
      kurt_quantile=kurt_quantile,
      #fusedPCA_kwargs=fusedPCA_kwargs,
      get_dirs=get_dirs, full_PCA=full_PCA,
      get_outliers=get_outliers, cutoff=cutoff, seed=seed,
      ICA_method=ICA_method,
      verbose=verbose
    )

    # Get projection.
    p_dname <- switch(projection, PCA="U", ICA="M")
    data_ps <- if (kurt_quantile > 0) {
      data_ps[[projection]][[p_dname]][,data_ps[[projection]]$highkurt]
    } else {
      data_ps[[projection]][[p_dname]]
    }

    if (ncol(data_ps)==0) { message("No high-kurtosis comps."); return(rep(FALSE, nrow(X))) }

  } else {
    data_ps <- X
  }

  # Compute robust distances.
  ind_incld <- c(cov.mcd(data_ps)$best)
  rd <- RD_meas(data_ps, ind_incld)

  # Identify univariate outliers.
  univOut <- RD_univOut(data_ps, cutoff = RD_cutoff, trans=trans)

  # Impute.
  impData <- RD_impData(data_ps, univOut)

  # cutoff obtained from RD of imputed data
  ind_incld_imp <- c(cov.mcd(impData)$best)
  rd_imp <- RD_meas(impData, ind_incld_imp)$RD
  empirical = quantile(rd_imp, probs =0.99)

  # Define dims.
  nT = dim(impData)[1]
  nQ = dim(impData)[2]
  nH = length(ind_incld)

  # Do the bootstrap.
  if (bootstrap_n == 0) { stop("Not implemented yet.") }
  ind_excld <- setdiff(seq(nT), ind_incld)
  # Fix the covariance since it is biased (low determinant) in the bootstrap.
  invcov_sqrt <- rd$invcov_sqrt
  RD_boot <- matrix(NA,nT,bootstrap_n)
  for (bb in seq(bootstrap_n)) {
    data_bb <- NA*impData
    if (verbose) { print(bb) }
    ind_incld_bb <- sample(ind_incld, nH, replace = TRUE)
    ind_excld_bb <- sample(ind_excld, (nT-nH), replace = TRUE)

    data_bb[ind_incld,] <- impData[ind_incld_bb,]
    data_bb[ind_excld,] <- impData[ind_excld_bb,]

    # MCD estimates of mean for bootstrap procedure
    xbar_star <- RD_meas(data_bb, ind_incld_bb, dist = FALSE)$xbar_star
    xbar_star_mat <- matrix(xbar_star, nrow = nT, ncol = nQ, byrow = TRUE)
    temp <- (data_bb - xbar_star_mat) %*% invcov_sqrt
    RD_boot[,bb] <- rowSums(temp * temp)
  }

  B_quant <- apply(RD_boot, 2, quantile, probs = 1-bootstrap_alpha)
  # 50% CI - 0.75 coverage
  lwr_50 <- quantile(as.vector(B_quant))[2]
  # 80% CI - 0.9 coverage
  lwr_80 <- quantile(B_quant, c(0.1, 0.9))[1]

  # 95% CI - 0.975 coverage
  lwr_95 <- quantile(B_quant, c(0.025, 0.975))[1]

  # Return results.
  list(
    data = data_ps, # the dimension reduced and high kurtosis selected data
    impData = impData, # imputed data
    dims = c(nT,nQ), # dimension of dimension reduced and high kurtosis selected data
    RD = rd, # RD of data_ps
    ind_incld = ind_incld,
    empirical = empirical,
    lwr_50=lwr_50, lwr_80=lwr_80, lwr_95=lwr_95,
    lwr_quant = quantile(B_quant, RD_quantile/2),
    B_quant=B_quant
  )
}
