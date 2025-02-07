#' Artifact images
#' 
#' Visualize artifact patterns from the results of \code{\link{pscrub}}.
#'  Requires \code{pscrub(..., get_dirs=TRUE)}. 
#' 
#' Computes two types: "mean" artifact images based on a weighted sum of the 
#'  projection directions, with weights determined by the scores for each 
#'  component at the flagged timepoint, and "top" artifact images based on the 
#'  projection direction with the greatest score at the flagged timepoint.
#'
#' @param psx A \code{"IDvols_projection"} object containing projection scrubbing
#'  results.
#' @param idx The timepoints or column indexes for which to compute artifact
#'  images. If \code{NULL} (default), use the outlying timepoints. 
#' @param use_dt If detrended components are available (the "U" matrix of PCA 
#'  or "M" matrix of ICA), should they be used to compute the artifact images?
#'  Otherwise, use the non-detrended components. Default: \code{TRUE}.
#'
#' @return A list of three: \code{idx}, the timepoints for which the artifact images
#'  were computed; \code{mean}, the "mean" artifact images; and \code{top}, the
#'  "top" artifact images. The row names of the \code{top} artifact images
#'  matrix give the index of the top component ("V" in PCA and "S" in ICA) at
#'  each timepoint.
#'
#' @importFrom fMRItools scale_med
#' @export
artifact_images <- function(psx, idx=NULL, use_dt=TRUE){

  # TO DO: only high-kurtosis components?

  # Check idx.
  if (is.null(idx)) {
    idx <- which(psx$outlier_flag)
    if (!(length(idx) > 0)) {
      warning(
        "`idx=NULL` will get artifact images at each flagged timepoint, ",
        "but no timepoints were flagged."
      )
      return(NULL)
    }
  } else {
    stopifnot(length(idx) > 0)
  }

  # Get PCA scores and directions (or ICA mixing and source matrices).
  if ("PCA" %in% names(psx)) {
    U <- psx$PCA$U
    if (!("V" %in% names(psx$PCA))) { 
      stop("No directions. Run `pscrub` again with `get_dirs=TRUE`.") 
    }
    V <- psx$PCA$V
  # } else if ("fusedPCA" %in% names(psx)) {
  #   U <- psx$fusedPCA$U
  #   if (!("V" %in% names(psx$PCA))) { 
  #     stop("No directions. Run `pscrub` again with `get_dirs=TRUE`.")
  #   }
  #   V <- psx$fusedPCA$V
  } else if ("ICA" %in% names(psx)) {
    U <- fMRItools::scale_med(psx$ICA$M)
    if (!("S" %in% names (psx$ICA))) {
      stop("No directions. Run `pscrub` again with `get_dirs=TRUE`.")
    }
    V <- psx$ICA$S
  }

  stopifnot(all(idx %in% seq(nrow(U))))

  if (is.null(psx$mask)) {
    const_mask = rep(TRUE, nrow(V))
  } else {
    const_mask <- psx$mask > 0
  }
  N_ <- length(const_mask)
  n_imgs <- length(idx)

  lev_imgs <- list(
    idx = idx,
    mean = matrix(NA, n_imgs, N_),
    top = matrix(NA, n_imgs, N_)
  )

  lev_imgs$mean[,const_mask] <- U[idx,,drop=FALSE] %*% t(V)
  dimnames(lev_imgs$mean) <- NULL

  for (ii in seq(length(idx))) {
    tt <- idx[ii]
    tt_top <- which.max(abs(U[tt,]))[1]
    lev_imgs$top[ii, const_mask] <- V[,tt_top]
  }
  rownames(lev_imgs$top) <- paste("t", as.character(idx))
  colnames(lev_imgs$top) <- NULL

  lev_imgs
}