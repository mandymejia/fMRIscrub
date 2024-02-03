#' Framewise Displacement
#'
#' Calculate Framewise Displacement (FD) 
#' 
#' The FD formula is taken from Power et al. (2012):
#'
#'  \deqn{FD_i = | \Delta x_i | + | \Delta y_i | + | \Delta z_i | + | \Delta \alpha_i | + | \Delta \beta_i | + | \Delta \gamma_i |} 
#'  
#'  where \eqn{i} is the timepoint; \eqn{x}, \eqn{y} and \eqn{z} are the 
#'  translational realignment parameters (RPs);
#'  \eqn{\alpha}, \eqn{\beta} and \eqn{\gamma} are the rotational RPs;
#'  and \eqn{\Delta x_i = x_{i-1} - x_i} (and similarly for the other RPs).
#' 
#' @param X An \eqn{N} by \eqn{6} matrix in which the first three columns represent the
#'  translational RPs (\code{trans_units}), and the second three columns represent
#'  the rotational RPs (\code{rot_units}). If \code{rot_units} measures an angle,
#'  it will be converted to \code{trans_units} by measuring displacement on a 
#'  sphere of radius \code{brain_radius} \code{trans_units}.
#'
#'  Alternatively, this can be the file path to an \eqn{N} by \eqn{6} matrix which can be
#'  read with \code{\link[utils]{read.table}} (fields separated by white-space; no
#'  header).
#' @param trans_units \code{"mm"} for millimeters (default), \code{"cm"} 
#'  for centimeters, or \code{"in"} for inches.
#' @param rot_units \code{"deg"} for degrees (default), \code{"rad"} for radians,
#'  or one of the \code{trans_units} options.
#' @param brain_radius If \code{rot_units} measures an angle (i.e. if it's
#'  \code{"deg"} or \code{"rad"}), the rotational RPs are transformed to a 
#'  measure of the displacement on a sphere of radius \code{brain_radius} 
#'  \code{trans_units}. So this argument should give the radius of the brain, 
#'  in the units of \code{trans_units}. If \code{NULL} (default), 
#'  \code{brain_radius} will be set to 50 mm (or 5 cm, or 50/25.4 inches).
#' 
#'  If \code{rot_units} does not measure an angle, this argument is ignored.
#' @param detrend Detrend each RP with the DCT before computing FD?
#'  Default: \code{FALSE}. Can be a number of DCT bases to use, or \code{TRUE}
#'  to use 4.
#' @param TR_for_resp_filt Filter out the respiratory frequency? If so, provide
#'  the temporal resolution (TR) in seconds, which is needed to calculate the
#'  filter. Default: \code{NULL} (do not apply a filter).
#' 
#'  Power et al. (2019) and Fair et al. (2020) have shown that in multiband 
#'  data, the RPs may contain head movements which accompany normal respiration,
#'  as well as artifactual variance in the phase encode direction at the 
#'  respiratory frequency. They propose applying a notch filter to the RPs to 
#'  remove variance within the frequency range of respiration; in particular, 
#'  Fair et al. (2020) used a second-order IIR filter between 0.31 and 0.43 Hz. 
#'  Here, we use a 0.31-0.43 Hz Chebyshev Type II stop-gap filter, with 20 dB 
#'  stopband ripple, from the \code{gsignal} package.
#' 
#'  The filter will be applied after any detrending. If filtering, consider 
#'  adjusting \code{cutoff} because the baseline FD value will be lowered. Pham 
#'  (2023) used 0.2 mm.
#' @param lag The difference of indices between which to calculate change in
#'  position. Default: \code{1} (the previous timepoint). Changing this
#'  argument sets \eqn{\Delta x_i = x_{i-lag} - x_i} (and similarly for the 
#'  other RPs).
#' @param cutoff FD values higher than this will be flagged. Default: \code{.4}.
#' @return A list with components
#' \describe{
#'  \item{measure}{A length \eqn{N} vector of FD values in \code{trans_units}.}
#'  \item{measure_info}{"FD"}
#'  \item{outlier_cutoff}{\code{cutoff}}
#'  \item{outlier_flag}{A length-N logical vector, where \code{TRUE} indicates suspected outlier presence.}
#' }
#'
#' @importFrom utils read.table
#' @importFrom fMRItools nuisance_regression dct_bases is_1
#' @export
#' 
#' @section References:
#'  \itemize{
#'    \item{Power, J. D., Barnes, K. A., Snyder, A. Z., Schlaggar, B. L., & Petersen, S. E. (2012). Spurious but systematic correlations in functional connectivity MRI networks arise from subject motion. Neuroimage, 59(3), 2142-2154.}
#'    \item{Power, J. D., Lynch, C. J., Silver, B. M., Dubin, M. J., Martin, A., & Jones, R. M. (2019). Distinctions among real and apparent respiratory motions in human fMRI data. NeuroImage, 201, 116041.}
#'    \item{Fair, D. A., Miranda-Dominguez, O., Snyder, A. Z., Perrone, A., Earl, E. A., Van, A. N., ... & Dosenbach, N. U. (2020). Correction of respiratory artifacts in MRI head motion estimates. Neuroimage, 208, 116400.}
#'    \item{Pham, D. D., McDonald, D. J., Ding, L., Nebel, M. B., & Mejia, A. F. (2023). Less is more: balancing noise reduction and data retention in fMRI with data-driven scrubbing. NeuroImage, 270, 119972.}
#' }
#' 
FD <- function(
  X, trans_units = c("mm", "cm", "in"), 
  rot_units = c("deg", "rad", "mm", "cm", "in"), 
  brain_radius=NULL, 
  detrend=FALSE, TR_for_resp_filt=NULL,
  lag=1, cutoff=.4) {

  if (is.character(X)) { X <- read.table(X) }
  X <- as.matrix(X); stopifnot(is.matrix(X))
  stopifnot(nrow(X) > 1); stopifnot(ncol(X) >= 6)
  if (ncol(X) > 6) { 
    warning(paste(
      "`X` has more than 6 columns.",
      "using the first 3 as translation RPs,",
      "the second 3 as rotation RPs, and discarding the rest.\n"
    ))
    X <- X[,1:6]
  }

  # Convert translational RPs to mm.
  trans_units <- match.arg(trans_units, trans_units)
  X[,1:3] <- X[,1:3] * switch(trans_units, mm=1, cm=10, `in`=25.4)

  # Convert rotational RPs to mm.
  # Get the brain radius. Note that this is actually only used if `rot_units`
  #   measures an angle.
  if (!is.null(brain_radius)) {
    brain_radius <- brain_radius * switch(trans_units, mm=1, cm=10, `in`=25.4)
  } else {
    brain_radius <- 50
  }
  # Convert.
  rot_units <- match.arg(rot_units, rot_units)
  X[,4:6] <- X[,4:6] * switch(rot_units, 
    rad=brain_radius, deg=brain_radius*2*pi/360, 
    mm=1, cm=10, `in`=25.4
  )

  # Detrend if requested.
  if (!isFALSE(detrend)) { 
    if (isTRUE(detrend)) { detrend <- 4 }
    X <- nuisance_regression(X, cbind(1, dct_bases(nrow(X), detrend)))
  }

  # Filter if requested.
  if (!is.null(TR_for_resp_filt)) {
    if (!requireNamespace("gsignal", quietly = TRUE)) {
      stop("Package \"gsignal\" needed for filtering. Please install it.", call. = FALSE)
    }

    stopifnot(fMRItools::is_1(TR_for_resp_filt, "numeric"))
    stopifnot(TR_for_resp_filt > 0)

    # Data sampled at 1/x Hz (TR = x s) has a Nyquist frequency of 1/x/2 Hz. 
    # cheby2: "w must be between 0 and 1, where 1 is the Nyquist frequency"
    # so w in [0, 1] maps to [0 Hz, 1/x/2 Hz].
    # Fair (2020) used a filter of [0.31 Hz and 0.43 Hz]. 
    # so w = [0.31 Hz, 0.43 Hz] /(1/x/2 Hz).
    #    w = [0.31, 0.43] * x * 2.
    filt_w <- c(0.31, 0.43) * TR_for_resp_filt * 2

    X <- gsignal::filtfilt(gsignal::cheby2(2, Rs=20, w=filt_w, type="stop"), X)
  }

  # Compute FD.
  Xdiff <- apply(X, 2, diff, lag=lag)
  FD <- c(rep(0, lag), rowSums(abs(Xdiff)))

  # Revert units to `trans_units`.
  attr(FD, "units") <- trans_units
  FD <- switch(trans_units, mm=FD, cm=FD/10, `in`=FD/25.4)

  out <- list(
    measure = FD,
    measure_info = setNames(c("FD", trans_units), c("type", "units"))
  )

  if (!is.null(cutoff)) {
    cutoff <- setNames(as.numeric(cutoff), "FD")
    out$outlier_cutoff <- cutoff
    out$outlier_flag <- out$measure > out$outlier_cutoff
  }

  structure(out, class="scrub_FD")
}