#' Data-driven scrubbing
#' 
#' Performs projection scrubbing or DVARS scrubbing, and optionally thresholds
#'  to identify artifactual time points.
#'
#' @param X A \eqn{T} by \eqn{V} numeric matrix representing an fMRI run. There should
#'  not be any missing data (\code{NA} or \code{NaN}). 
#' @param method \code{"projection"} (default) or \code{"DVARS"}
#' @param ... Additional arguments to the specific scrubbing function: see
#'  \code{\link{pscrub}} or \code{\link{DVARS}}.
#' 
#' @return A list with components
#' \describe{
#'  \item{measure}{A length \eqn{T} vector or data.frame with \eqn{T} rows, giving the outlyingness measure(s)}
#'  \item{measure_info}{Describes the outlyingness measure(s)}
#'  \item{outlier_cutoff}{The outlier cutoff value(s).}
#'  \item{outlier_flag}{A length \eqn{T} vector or data.frame with \eqn{T} rows,  where \code{TRUE} indicates suspected outlier presence.}
#' }
#' 
#' @export
#' 
IDvols <- function(X, method=c("projection", "DVARS"), ...) {
  method <- match.arg(method, c("projection", "DVARS"))
  FUN <- switch(method, projection=pscrub, DVARS=DVARS)
  FUN(X, ...)
}

#' Scrub fMRI data in CIFTI format
#' 
#' Performs projection scrubbing or DVARS scrubbing, and optionally thresholds
#'  to identify artifactual time points. Requires \code{ciftiTools} and the 
#'  Connectome Workbench.
#' 
#' @param X Path to a CIFTI file, or a \code{"xifti"} object. 
#' @param method \code{"projection"} or \code{"DVARS"}
#' @param brainstructures Character vector indicating which brain structure(s) 
#'  to use: \code{"left"} (left cortical surface), \code{"right"} (right 
#'  cortical surface) and/or \code{"subcortical"} (subcortical and cerebellar
#'  gray matter). Can also be \code{"all"} (obtain all three brain structures). 
#'  Default: \code{c("left", "right")} (excludes the subcortex). 
#' @param ... Additional arguments to each specific scrubbing function: 
#'  \code{\link{pscrub}} or \code{\link{DVARS}}.
#' 
#' @return A list with components
#' \describe{
#'  \item{measure}{A length \eqn{T} vector or data.frame with \eqn{T} rows, giving the outlyingness measure(s)}
#'  \item{measure_info}{Describes the outlyingness measure(s)}
#'  \item{outlier_cutoff}{The outlier cutoff value(s).}
#'  \item{outlier_flag}{A length \eqn{T} vector or data.frame with \eqn{T} rows,  where \code{TRUE} indicates suspected outlier presence.}
#' }
#' 
#' @export 
#' 
IDvols_xifti <- function(X, method=c("projection", "DVARS"), brainstructures=c("left", "right"), ...) {
  method <- match.arg(method, c("projection", "DVARS"))
  FUN <- switch(method, projection=pscrub, DVARS=DVARS)

  if (!requireNamespace("ciftiTools", quietly = TRUE)) {
    stop("Package \"ciftiTools\" needed. Please install it.", call. = FALSE)
  }

  if (!ciftiTools::is.xifti(X, messages=FALSE)) {
    is.fname <- length(X) == 1 && is.character(X) && file.exists(X)
    if (!is.fname) {stop("`X` must be a path to a CIFTI file or `xifti` object.")}
    X <- ciftiTools::read_xifti(X, brainstructures=brainstructures)
  } else {
    brainstructures <- match.arg(
      brainstructures,
      c("left","right","subcortical","all"),
      several.ok=TRUE
    )
    if ("all" %in% brainstructures) { 
      brainstructures <- c("left","right","subcortical")
    }
    if (!("left" %in% brainstructures)) { X <- ciftiTools::remove_xifti(X, "cortex_left") }
    if (!("right" %in% brainstructures)) { X <- ciftiTools::remove_xifti(X, "cortex_right") }
    if (!("subcortical" %in% brainstructures)) { X <- ciftiTools::remove_xifti(X, "subcortical") }
  }

  FUN(t(as.matrix(X)), ...)
}