#' Convert \code{"pscrub_multi"} to \code{"pscrub"}
#'
#' @param psx The \code{"pscrub_multi"} object to convert
#' @return The resulting \code{"pscrub"} object
#'
#' @keywords internal
pscrub_from_multi <- function(psx) {
  class(psx) <- "IDvols_projection"
  if (ncol(psx$measure) > 1) { 
    warning("The input was not from a `pscrub` call, since there are more than one leverage measures.")
    return(psx)
  }

  psx$measure <- as.numeric(psx$measure[,1])
  psx$measure_info <- setNames(as.character(psx$measure_info[1,]), colnames(psx$measure_info))
  if ("outlier_cutoff" %in% names(psx)) { psx$outlier_cutoff <- as.numeric(psx$outlier_cutoff) }
  if ("outlier_flag" %in% names(psx)) { psx$outlier_flag <- as.logical(psx$outlier_flag[,1]) }

  # For all projections
  if (grepl("PCA2|ICA2", psx$measure_info["name"])) {
    nComps <- psx$PCA$nPCs_avgvar
  } else {
    nComps <- psx$PCA$nPCs_PESEL
  }
  
  # For PCA
  if (!is.null(psx$PCA$U)) {
    if (nrow(psx$PCA$U) != ncol(psx$PCA$U)) {
      psx$PCA$U <- psx$PCA$U[, seq(nComps), drop=FALSE]
      psx$PCA$D <- psx$PCA$D[seq(nComps), drop=FALSE]
      if ("V" %in% names(psx$PCA)) {
        psx$PCA$V <- psx$PCA$V[, seq(nComps), drop=FALSE]
      }
      if ("highkurt" %in% names(psx$PCA)) {
        psx$PCA$highkurt <- psx$PCA$highkurt[seq(nComps)]
      }
    }
    psx$PCA$nPCs_avgvar <- psx$PCA$nPCs_PESEL <- NULL
  # # For fusedPCA
  # } else if (!is.null(psx$fusedPCA)) {
  #   if (nrow(psx$fusedPCA$U) != ncol(psx$fusedPCA$U)) {
  #     psx$fusedPCA$U <- psx$fusedPCA$U[, seq(nComps), drop=FALSE]
  #     psx$fusedPCA$D <- psx$fusedPCA$D[seq(nComps), drop=FALSE]
  #     if ("V" %in% names(psx$fusedPCA)) {
  #       psx$fusedPCA$V <- psx$fusedPCA$V[, seq(nComps), drop=FALSE]
  #     }
  #     if ("highkurt" %in% names(psx$fusedPCA)) {
  #       psx$fusedPCA$highkurt <- psx$fusedPCA$highkurt[seq(nComps)]
  #     }
  #   }
  #   psx$PCA <- NULL
  # For ICA
  } else if (!is.null(psx$ICA)) {
    if (nrow(psx$ICA$M) != ncol(psx$ICA$M)) {
      psx$ICA$S <- psx$ICA$S[, seq(nComps), drop=FALSE]
      psx$ICA$M <- psx$ICA$M[, seq(nComps), drop=FALSE]
      if ("highkurt" %in% names(psx$ICA)) {
        psx$ICA$highkurt <- psx$ICA$highkurt[seq(nComps)]
      }
    }
    psx$PCA <- NULL
  }

  psx
}
