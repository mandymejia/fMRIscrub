#' Framewise Displacement
#'
#' Calculate Framewise Displacement (FD) 
#' 
#' The FD formula is taken from Power et. al. (2012):
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
#'  measurement representing the displacement on a sphere of radius 
#'  \code{brain_radius} \code{trans_units}. So this argument should give the
#'  radius of the brain, in the units of \code{trans_units}.
#' 
#'  If \code{brain_radius} is \code{NULL} (default), it will be set to 
#'  50 mm (regardless of \code{trans_units}).
#' 
#'  If \code{rot_units} does not measure an angle, this argument is completely
#'  ignored.
#' @param detrend Detrend each RP with the DCT before computing FD?
#'  Default: \code{FALSE}. Can be a number of DCT bases to use, or \code{TRUE}
#'  to use 4.
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
#' @importFrom fMRItools nuisance_regression dct_bases
#' @export
#' 
#' @section References:
#'  \itemize{
#'    \item{Power, J. D., Barnes, K. A., Snyder, A. Z., Schlaggar, B. L. & Petersen, S. E. Spurious but systematic correlations in functional connectivity MRI networks arise from subject motion. Neuroimage 59, 2142-2154 (2012).}
#' }
#' 
FD <- function(
  X, trans_units = c("mm", "cm", "in"), 
  rot_units = c("deg", "rad", "mm", "cm", "in"), 
  brain_radius=NULL, detrend=FALSE, lag=1, cutoff=.4) {

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

  # Convert RPs and brain radius to mm.
  # Note that brain radius will only actually be used if `rot_units` measures an
  #   angle.
  trans_units <- match.arg(trans_units, trans_units)
  X[,1:3] <- X[,1:3] * switch(trans_units, mm=1, cm=10, `in`=25.4)

  if (!is.null(brain_radius)) {
    brain_radius <- brain_radius * switch(trans_units, mm=1, cm=10, `in`=25.4)
  } else {
    brain_radius <- 50
  }

  # Convert `rot_units` to mm.
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