#' Compare projection scrubbing measures with \code{pscrub_multi}
#' 
#' Calculates leverage to identify outliers in high-dimensional data. 
#'  Can get results using multiple kinds of projections.
#' 
#' @inheritParams pscrub_Params
#' @param projection Projection scrubbing projects the data onto directions 
#'  likely to contain outlier information. Choose at least one of the following:
#' 
#'  \describe{
#'    \item{\code{"PCA"}}{PCA using the top \eqn{k} PCs.}
#'    \item{\code{"PCA_kurt"}}{PCA using the high-kurtosis PCs among the top \eqn{k}.}
#'    \item{\code{"PCA2"}}{PCA using the top \eqn{k2} PCs.}
#'    \item{\code{"PCA2_kurt"}}{PCA using the high-kurtosis PCs among the top \eqn{k2}.}
#    \item{\code{"fusedPCA"}}{fusedPCA using the top \eqn{k} fused PCs.}
#    \item{\code{"fusedPCA_kurt"}}{fusedPCA using the high-kurtosis fused PCs among the top \eqn{k}.}
#    \item{\code{"fusedPCA2"}}{fusedPCA using the top \eqn{k2} fused PCs.}
#    \item{\code{"fusedPCA2_kurt"}}{fusedPCA using the high-kurtosis fused PCs among the top \eqn{k2}.}
#'    \item{\code{"ICA"}}{ICA using the top \eqn{k} ICs.}
#'    \item{\code{"ICA_kurt"}}{ICA using the high-kurtosis ICs among the top \eqn{k}.}
#'    \item{\code{"ICA2"}}{ICA using the top \eqn{k2} ICs.}
#'    \item{\code{"ICA2_kurt"}}{ICA using the high-kurtosis ICs among the top \eqn{k2}.}
#'  }
#' 
#'  where \eqn{k} is the number of components determined by PESEL, and \eqn{k2}
#'  is the number of principal components with above-average variance.
#'  
#'  Use \code{"all"} to use all projection methods. Default: \code{"ICA_kurt"}.
#' @return A \code{"pscrub_multi"} object, i.e. a list with components
#' \describe{
#'  \item{measure}{A \eqn{T} by \eqn{P} data.frame of numeric leverage values, each column being the leverage values for a projection method in \code{projection}.}
#'  \item{measure_info}{A data.frame with \eqn{P} rows listing information about each projection used.}
#'  \item{outlier_cutoff}{A \eqn{1} by \eqn{P} data.frame of numeric outlier cutoff values for each projection (\code{cutoff} times the median leverage).}
#'  \item{outlier_flag}{A \eqn{T} by \eqn{P} data.frame of logical values where \code{TRUE} indicates where leverage exceeds the cutoff, signaling suspected outlier presence.}
#'  \item{mask}{
#'    A length \eqn{P} numeric vector corresponding to the data locations in \code{X}. Each value indicates whether the location was masked:
#'    \describe{
#'      \item{1}{The data location was not masked out.}
#'      \item{-1}{The data location was masked out, because it had at least one \code{NA} or \code{NaN} value.}
#'      \item{-2}{The data location was masked out, because it was constant.}
#'    }
#'  }
#'  \item{PCA}{
#'    This will be a list with components:
#'    \describe{
#'      \item{U}{The \eqn{T} by \eqn{Q} PC score matrix.}
#'      \item{D}{The standard deviation of each PC.}
#'      \item{V}{The \eqn{P} by \eqn{Q} PC directions matrix. Included only if \code{get_dirs}.}
#'      \item{highkurt}{The length \code{Q} logical vector indicating scores of high kurtosis.}
#'      \item{U_dt}{Detrended components of \code{U}. Included only if components were mean- or variance-detrended.}
#'      \item{highkurt}{The length \code{Q} logical vector indicating detrended scores of high kurtosis.}
#'      \item{nPCs_PESEL}{The number of PCs selected by PESEL.}
#'      \item{nPCs_avgvar}{The number of above-average variance PCs.}
#'    }
#'    where \code{Q} is the number of PCs selected by PESEL or of above-average variance (or the greater of the two if both were used). 
#'    If PCA was not used, all entries except \code{nPCs_PESEL} and/or \code{nPCs_avgvar} will not be included, depending on which
#'    method(s) was used to select the number of PCs.
#'  }
#  \item{fusedPCA}{
#    If fusedPCA was used, this will be a list with components:
#    \describe{
#      \item{U}{The \eqn{T} by \eqn{Q} PC score matrix.}
#      \item{D}{The standard deviation of each PC.}
#      \item{V}{The \eqn{P} by \eqn{Q} PC directions matrix. Included only if \code{get_dirs}}
#      \item{highkurt}{The length \code{Q} logical vector indicating scores of high kurtosis.}
#      \item{U_dt}{Detrended components of \code{U}. Included only if components were mean- or variance-detrended.}
#      \item{highkurt}{The length \code{Q} logical vector indicating detrended scores of high kurtosis. Included only if components were mean- or variance-detrended.}
#    }
#  }
#'  \item{ICA}{
#'    If ICA was used, this will be a list with components:
#'    \describe{
#'      \item{S}{The \eqn{P} by \eqn{Q} source signals matrix. Included only if \code{get_dirs}} 
#'      \item{M}{The \eqn{T} by \eqn{Q} mixing matrix.}
#'      \item{highkurt}{The length \code{Q} logical vector indicating mixing scores of high kurtosis.}
#'      \item{M_dt}{Detrended components of \code{M}. Included only if components were mean- or variance-detrended.}
#'      \item{highkurt}{The length \code{Q} logical vector indicating detrended mixing scores of high kurtosis. Included only if components were mean- or variance-detrended.}
#'    }
#'  }
#' }
#'
#' @importFrom pesel pesel
#' @importFrom robustbase rowMedians
#' @importFrom stats mad qnorm var setNames
#' @importFrom fMRItools is_integer is_constant nuisance_regression as.matrix_ifti dct_bases validate_design_mat is_1 is_posNum
#' 
#' @keywords internal
#' 
pscrub_multi = function(
  X, projection = "ICA_kurt", 
  nuisance="DCT4",
  center=TRUE, scale=TRUE, comps_mean_dt=FALSE, comps_var_dt=FALSE,
  kurt_quantile=.99, #fusedPCA_kwargs=NULL,
  get_dirs=FALSE, full_PCA=FALSE,
  get_outliers=TRUE, cutoff=4, seed=0, ICA_method=c("C", "R"),
  verbose=FALSE){

  # ----------------------------------------------------------------------------
  # Check arguments. -----------------------------------------------------------
  # ----------------------------------------------------------------------------

  # Simple arguments. ----------------------------------------------------------
  stopifnot(is_1(center, "logical"))
  stopifnot(is_1(scale, "logical"))
  stopifnot(is_posNum(kurt_quantile))
  stopifnot(is_1(get_dirs, "logical"))
  stopifnot(is_1(full_PCA, "logical"))
  stopifnot(is_1(get_outliers, "logical"))
  stopifnot(is_1(cutoff, "numeric") && cutoff==round(cutoff))
  stopifnot(is_1(verbose, "logical"))
  ICA_method <- match.arg(ICA_method, c("C", "R"))

  # `X` ------------------------------------------------------------------------
  if (verbose) { cat("Checking for missing, infinite, and constant data.\n") }
  X <- as.matrix_ifti(X, verbose=verbose); class(X) <- "numeric"
  V0_ <- ncol(X)
  X_NA_mask <- apply(X, 2, function(x){any(x %in% c(NA, NaN, -Inf, Inf))})
  if (any(X_NA_mask)) {
    if (all(X_NA_mask)) { stop("All data columns have at least one NA, NaN, or infinte value. None of these are allowed.\n") }
    warning(
      "Removing ", sum(X_NA_mask),
      " columns with at least one NA, NaN, or infinite value (out of ", ncol(X), ").\n"
    )
    X <- X[,!X_NA_mask,drop=FALSE]
  }
  X_const_mask <- apply(X, 2, fMRItools::is_constant)
  if (any(X_const_mask)) {
    if (all(X_const_mask)) { stop("All data columns are constant.\n") }
    warning(
      "Removing ", sum(X_const_mask), 
      " constant data columns (out of ", ncol(X), ").\n"
    )
    X <- X[,!X_const_mask,drop=FALSE]
  }
  T_ <- nrow(X); V_ <- ncol(X)
  if (T_ > V_) {
    warning(
      "Data matrix has more rows than columns. Check that observations ",
      "are in rows and variables are in columns.\n"
    )
  }

  # Create output structure. ---------------------------------------------------
  out <- list(
    measure = list(),
    measure_info = list(),
    outlier_cutoff = list(),
    outlier_flag = list(),
    mask = rep(1, V0_),
    PCA = NULL,
    #fusedPCA = NULL,
    ICA = NULL
  )

  out$mask[X_NA_mask] <- -1
  out$mask[!X_NA_mask][X_const_mask] <- -2

  # `projection`----------------------------------------------------------------
  valid_projection_PESEL <- c(
    "PCA", "PCA_kurt", 
    #"fusedPCA", "fusedPCA_kurt", 
    "ICA", "ICA_kurt"
  )
  valid_projection_avgvar <- c(
    "PCA2", "PCA2_kurt", 
    #"fusedPCA2", "fusedPCA2_kurt", 
    "ICA2", "ICA2_kurt"
  )
  valid_projection <- c(valid_projection_PESEL, valid_projection_avgvar)
  if ("all" %in% projection) {
    projection <- valid_projection
  } else {
    projection <- unique(match.arg(projection, valid_projection, several.ok=TRUE))
  }
  base_projection <- unique(gsub("2", "", gsub("_kurt", "", projection)))

  # `out$measure_info` ---------------------------------------------------------
  m_info <- data.frame(name = valid_projection[valid_projection %in% projection])
  m_info$type <- "Leverage"
  m_info$projection <- gsub("2", "", gsub("_kurt", "", m_info$name))
  m_info$PESEL <- !grepl("2", m_info$name)
  m_info$kurt <- grepl("kurt", m_info$name)
  m_info$comps_mean_dt <- comps_mean_dt
  m_info$comps_var_dt <- comps_var_dt
  out$measure_info <- m_info

  # `nuisance`------------------------------------------------------------------
  if (identical(nuisance, "DCT4")) { 
    nuisance <- cbind(1, dct_bases(nrow(X), 4))
  } else {
    if (is.character(nuisance)) { stop("`nuisance` must be 'DCT4' or a numeric matrix.") }
  }
  do_nuisance <- !(is.null(nuisance) || isFALSE(nuisance) || identical(nuisance, 0))
  if (do_nuisance) { 
    nuisance <- validate_design_mat(nuisance, T_)
    design_const_mask <- apply(nuisance, 2, fMRItools::is_constant)
    if (!any(design_const_mask)) {
      if (!any(abs(apply(X, 2, mean)) < 1e-8)) {
        warning("No intercept detected in `design`, yet the data are not centered.")
      }
    }
  }

  # Component detrending arguments. --------------------------------------------
  if (isTRUE(comps_mean_dt)) {
    comps_mean_dt <- 4
  } else if (isFALSE(comps_mean_dt)) { 
    comps_mean_dt <- 0
  } else {
    comps_mean_dt <- as.numeric(comps_mean_dt)
    stopifnot(is_integer(comps_mean_dt, nneg=TRUE))
  }
  if (isTRUE(comps_var_dt)) {
    comps_var_dt <- 4
  } else if (isFALSE(comps_var_dt)) { 
    comps_var_dt <- 0
  } else {
    comps_var_dt <- as.numeric(comps_var_dt)
    stopifnot(is_integer(comps_var_dt, nneg=TRUE))
  }
  comps_dt <- (comps_mean_dt > 0) || (comps_var_dt > 0)
  # if(!identical(fusedPCA_kwargs, NULL)){
  #   names(fusedPCA_kwargs) <- match.arg(
  #     names(fusedPCA_kwargs), c("lambda", "niter_max", "TOL", "verbose"),
  #     several.ok=TRUE)
  #   if(length(names(fusedPCA_kwargs)) != length(unique(names(fusedPCA_kwargs)))){
  #     stop("Duplicate fusedPCA_kwargs were given.")
  #   }
  # }

  # ----------------------------------------------------------------------------
  # Nuisance regression followed by centering & scaling. -----------------------
  # ----------------------------------------------------------------------------

  if (do_nuisance) { 
    if (verbose) { cat("Performing nuisance regression.\n") }
    X <- nuisance_regression(X, nuisance)
  }

  if (center || scale) {
    if (verbose) { 
      action <- c("Centering", "Scaling", "Centering and scaling")[1*center + 2*scale]
      cat(action, "the data columns.\n")
    }

    # Center & scale here, rather than calling `fMRItools::scale_med`, to save memory.
    X <- t(X)
    if (center) { X <- X - c(rowMedians(X)) }
    if (scale) { X <- X / (1.4826 * rowMedians(abs(X))) }
    X <- t(X)
  }

  # ----------------------------------------------------------------------------
  # Make projection. -----------------------------------------------------------
  # ----------------------------------------------------------------------------

  # Compute PESEL
  maxK_PCA <- 1
  if (any(valid_projection_PESEL %in% projection)) {
    if (verbose) { cat("Computing PESEL.\n") }
    if (!is.null(seed)) {
      nPCs_PESEL <- with(set.seed(seed), pesel::pesel(
        t(X), npc.max=ceiling(T_/2), method="homogenous"
      )$nPCs)
    } else {
      nPCs_PESEL <- pesel::pesel(
        t(X), npc.max=ceiling(T_/2), method="homogenous"
      )$nPCs
    }
    if (nPCs_PESEL == 1) {
      warning("PESEL estimates that there is only one component. Using two.")
      nPCs_PESEL <- 2
    }
    maxK_PCA <- max(maxK_PCA, nPCs_PESEL)
  }

  # Compute PCA.
  if ("PCA" %in% base_projection){# || "fusedPCA" %in% base_projection) {
    if (verbose) { cat("Computing PCA.\n") }
    # Compute the SVD of X or XXt.
    if (get_dirs) {# || "fusedPCA" %in% base_projection) {
      out$PCA <- tryCatch(
        { svd(X)[c("u", "d", "v")] },
        error = function(cond) {
          message(cond)
          if (!requireNamespace("corpcor", quietly = TRUE)) {
            stop(
              "`svd` failed, and the backup routine `corpcor::fast.svd` ", 
              "is not available since the Package \"corpcor\" is needed. ",
              "Please install it.", call. = FALSE
            )
          }
          cat("Trying `corpcor::fast.svd`.\n")
          return(corpcor::fast.svd(X)[c("u", "d", "v")])
        }
      )
      names(out$PCA) <- toupper(names(out$PCA))
    } else {
      # Conserve memory by using `XXt`.
      out$PCA <- tryCatch(
        { svd(tcrossprod(X))[c("u", "d", "v")] },
        error = function(cond) {
          message(cond)
          if (!requireNamespace("corpcor", quietly = TRUE)) {
            stop(
              "`svd` failed, and the backup routine `corpcor::fast.svd` ", 
              "is not available since the Package \"corpcor\" is needed. ",
              "Please install it.", call. = FALSE
            )
          }
          cat("Trying `corpcor::fast.svd`.\n")
          return(corpcor::fast.svd(tcrossprod(X))[c("u", "d", "v")])
        }
      )
      names(out$PCA) <- toupper(names(out$PCA))
      out$PCA$D <- sqrt(out$PCA$D)
      out$PCA$V <- NULL
    }
    # Keep only the above-average variance/PESEL PCs (whichever is greater).
    if (any(valid_projection_PESEL %in% projection)) {
      out$PCA$nPCs_PESEL <- nPCs_PESEL
    }
    if (any(valid_projection_avgvar %in% projection)) {
      out$PCA$nPCs_avgvar <- max(1, sum(out$PCA$D^2 > mean(out$PCA$D^2)))
      maxK_PCA <- max(maxK_PCA, out$PCA$nPCs_avgvar)
    }
    # Identify which PCs have high kurtosis.
    if (any(c("PCA_kurt", "PCA2_kurt") %in% projection)) {
      out$PCA$highkurt <- high_kurtosis(out$PCA$U[, seq(maxK_PCA), drop=FALSE], kurt_quantile=kurt_quantile, min_1=TRUE)
    }
  } else {
    # If computing ICA but not PCA or fusedPCA, just get the nPCs.
    if (any(valid_projection_PESEL %in% projection)) {
      out$PCA$nPCs_PESEL <- nPCs_PESEL
    }
    if (any(valid_projection_avgvar %in% projection)) {
      eig <- svd(tcrossprod(X))$d
      out$PCA$nPCs_avgvar <- max(1, sum(eig > mean(eig)))
    }
  }

  # # Compute fusedPCA.
  # if ("fusedPCA" %in% base_projection) {
  #   maxK_fusedPCA <- max(as.numeric(list(
  #     fusedPCA = out$PCA$nPCs_PESEL,
  #     fusedPCA_kurt = out$PCA$nPCs_PESEL,
  #     fusedPCA2 = out$PCA$nPCs_avgvar,
  #     fusedPCA2_kurt = out$PCA$nPCs_avgvar
  #   )[projection[grepl("fusedPCA", projection)]]))
  #   if (verbose) { cat("Computing fusedPCA.\n") }
  #   out$fusedPCA <- do.call(
  #     fusedPCA, 
  #     c(
  #       list(
  #         X=X, X.svd=out$PCA[c("U", "D", "V")], 
  #         K=maxK_fusedPCA, solve_directions=get_dirs
  #       ), 
  #       fusedPCA_kwargs
  #     )
  #   )
  #   out$fusedPCA$PC_exec_times <- NULL; out$fusedPCA$nItes <- NULL
  #   # V matrix from PCA no longer needed.
  #   if(!get_dirs){ out$PCA$V <- NULL }
    
  #   tf_const_mask <- apply(out$fusedPCA$u, 2, is_constant)
  #   if(any(tf_const_mask)){
  #     warning(
  #       "Warning: ", sum(tf_const_mask), " out of ", length(tf_const_mask),
  #       " fused PC scores are zero-variance.\n"
  #     )
  #   }
  #   names(out$fusedPCA)[names(out$fusedPCA) %in% c("u", "d", "v")] <- toupper(
  #     names(out$fusedPCA)[names(out$fusedPCA) %in% c("u", "d", "v")]
  #   )
  # }
  # # Identify which fused PCs have high kurtosis.
  # if (any(c("fusedPCA_kurt", "fusedPCA2_kurt") %in% projection)) {
  #   out$fusedPCA$highkurt <- high_kurtosis(out$fusedPCA$U, kurt_quantile=kurt_quantile)
  # }

  # Remove extra PCA information.
  if (!is.null(out$PCA$U) && !full_PCA) {
    out$PCA$U <- out$PCA$U[, seq(maxK_PCA), drop=FALSE]
    out$PCA$D <- out$PCA$D[seq(maxK_PCA), drop=FALSE]
    if (!is.null(out$PCA$V)) { 
      out$PCA$V <- out$PCA$V[, seq(maxK_PCA), drop=FALSE]
    }
  }

  # Compute ICA
  if ("ICA" %in% base_projection) {
    maxK_ICA <- max(as.numeric(list(
      ICA = out$PCA$nPCs_PESEL,
      ICA_kurt = out$PCA$nPCs_PESEL,
      ICA2 = out$PCA$nPCs_avgvar,
      ICA2_kurt = out$PCA$nPCs_avgvar
    )[projection[grepl("ICA", projection)]]))

    if (verbose) { cat("Computing ICA.\n" ) }
    if (!requireNamespace("fastICA", quietly = TRUE)) {
      stop("Package \"fastICA\" needed to compute the ICA. Please install it.", call. = FALSE)
    }
    if (!is.null(seed)) {
      out$ICA <- with(set.seed(seed), fastICA::fastICA(t(X), maxK_ICA, method=ICA_method))[c("S", "A")]
    } else {
      out$ICA <- fastICA::fastICA(t(X), maxK_ICA, method=ICA_method)[c("S", "A")]
    }
    names(out$ICA)[names(out$ICA)=="A"] <- "M"
    out$ICA$M <- t(out$ICA$M)

    # Issue due to rank. Does this still happen w/ fastICA?
    if (ncol(out$ICA$M) < maxK_ICA) {
      cat("Rank issue with ICA: adding constant zero columns.\n")
      K_missing <- maxK_ICA - ncol(out$ICA$M)
      out$ICA$M <- cbind(out$ICA$M, matrix(0, nrow=nrow(out$ICA$M), ncol=K_missing))
      out$ICA$S <- cbind(out$ICA$S, matrix(0, nrow=nrow(out$ICA$S), ncol=K_missing))
    }

    if (any(c("ICA_kurt", "ICA2_kurt") %in% projection)) {
      out$ICA$highkurt <- high_kurtosis(out$ICA$M, kurt_quantile=kurt_quantile)
    }

    if(!get_dirs){ out$ICA$S <- NULL }
  }

  if (comps_dt) {
    if (!is.null(out$PCA$U)) { 
      out$PCA$U_dt <- apply(
        out$PCA$U, 2, rob_stabilize, center=comps_mean_dt, scale=comps_var_dt
      )
      out$PCA$highkurt_dt <- high_kurtosis(out$PCA$U_dt, kurt_quantile=kurt_quantile)
    }
    # if (!is.null(out$fusedPCA$U)) { 
    #   out$fusedPCA$U_dt <- apply(
    #     out$fusedPCA$U, 2, rob_stabilize, center=comps_mean_dt, scale=comps_var_dt
    #   )
    #   out$fusedPCA$highkurt_dt <- high_kurtosis(out$fusedPCA$U_dt, kurt_quantile=kurt_quantile)
    # }
    if (!is.null(out$ICA$M)) { 
      out$ICA$M_dt <- apply(
        out$ICA$M, 2, rob_stabilize, center=comps_mean_dt, scale=comps_var_dt
      )
      out$ICA$highkurt_dt <- high_kurtosis(out$ICA$M_dt, kurt_quantile=kurt_quantile)
    }
  }

  rm(X); gc()

  # ----------------------------------------------------------------------------
  # Compute leverage. ----------------------------------------------------------
  # ----------------------------------------------------------------------------

  if (verbose) { cat("Computing leverage.\n") }

  for (ii in seq(length(projection))) {
    proj_ii <- projection[ii]
    base_ii <- gsub("2", "", gsub("_kurt", "", proj_ii))
    scores_ii <- paste0(
      ifelse(grepl("ICA", proj_ii), "M", "U"), 
      ifelse(!isFALSE(comps_dt), "_dt", "")
    )
    highkurt_ii <- paste0("highkurt", ifelse(comps_dt, "_dt", ""))

    # Make projection.
    Comps_ii <- switch(proj_ii,
      PCA = seq(out$PCA$nPCs_PESEL),
      PCA_kurt = which(out$PCA[[highkurt_ii]][seq(out$PCA$nPCs_PESEL)]),
      PCA2 = seq(out$PCA$nPCs_avgvar),
      PCA2_kurt = which(out$PCA[[highkurt_ii]][seq(out$PCA$nPCs_avgvar)]),
      #fusedPCA = seq(out$PCA$nPCs_PESEL),
      #fusedPCA_kurt = which(out$fusedPCA[[highkurt_ii]][seq(out$PCA$nPCs_PESEL)]),
      #fusedPCA2 = seq(out$PCA$nPCs_avgvar),
      #fusedPCA2_kurt = which(out$fusedPCA[[highkurt_ii]][seq(out$PCA$nPCs_avgvar)]),
      ICA = seq(out$PCA$nPCs_PESEL),
      ICA_kurt = which(out$ICA[[highkurt_ii]][seq(out$PCA$nPCs_PESEL)]),
      ICA2 = seq(out$PCA$nPCs_avgvar),
      ICA2_kurt = which(out$ICA[[highkurt_ii]][seq(out$PCA$nPCs_avgvar)])
    )

    if (grepl("kurt", proj_ii) && length(Comps_ii) < 1) {
      # No high-kurtosis components: no outliers!
      result_ii <- list(
        meas = rep(0, T_), 
        cut = NA, 
        flag = rep(FALSE, T_)
      )
    } else {
      Comps_ii <- out[[base_ii]][[scores_ii]][, Comps_ii, drop=FALSE]
      # Compute leverage.
      result_ii <- leverage(Comps=Comps_ii, median_cutoff=cutoff)
    }
    out$measure[[proj_ii]] <- result_ii$meas
    if (get_outliers) {
      out$outlier_cutoff[[proj_ii]] <- result_ii$cut
      out$outlier_flag[[proj_ii]] <- result_ii$flag
    }
  }

  # ----------------------------------------------------------------------------
  # Format output. -------------------------------------------------------------
  # ----------------------------------------------------------------------------

  out$measure <- as.data.frame(out$measure)
  if (length(out$outlier_cutoff) > 0) {
    out$outlier_cutoff <- as.data.frame(out$outlier_cutoff)
  } else {
    out$outlier_cutoff <- NULL
  }
  if (length(out$outlier_flag) > 0) {
    out$outlier_flag <- as.data.frame(out$outlier_flag)
  } else {
    out$outlier_flag <- NULL
  }

  out <- out[!vapply(out, is.null, FALSE)]
 
  structure(out, class="scrub_projection_multi")
}