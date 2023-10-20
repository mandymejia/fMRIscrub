#' pscrub
#' 
#' @param X Wide numeric data matrix (\eqn{T} observations by \eqn{V} variables, 
#'  \eqn{T << V}). If \code{X} represents an fMRI run, \eqn{T} should be the 
#'  number of timepoints and \eqn{V} should be the number of vertices/voxels. 
#'  Projection scrubbing will measure the outlyingness of each row in \code{X}.
#' @param nuisance Nuisance signals to regress from each column of \code{X}. 
#'  Should be specified as a design matrix: a \eqn{T} by \eqn{N} numeric matrix
#'  where \eqn{N} represents the number of nuisance signals. Or can be "DCT4"
#'  (default), which will create a matrix with a constant column (the intercept 
#'  term) and four DCT bases. This default nuisance regression will have the 
#'  effect of demeaning and detrending the data by removing low-frequency 
#'  components. To not perform any nuisance regression set this argument to 
#'  \code{NULL}, \code{0}, or \code{FALSE}.
#' 
#'  Detrending is highly recommended for time-series data, especially if there 
#'  are many time points or evolving circumstances affecting the data. Additionally,
#'  if kurtosis is being used to select the projection directions, trends can 
#'  induce positive or negative kurtosis, contaminating the connection between 
#'  high kurtosis and outlier presence. Detrending should not be used with 
#'  non-time-series data because the observations are not temporally related.
#' 
#'  Additional nuisance regressors can be specified like so:
#'  \code{cbind(1, fMRItools::dct_bases(nrow(x), 4), more_nuisance)}.
#' @param center,scale Center the columns of the data by their medians, and scale the
#'  columns of the data by their median absolute deviations (MADs)? Default: \code{TRUE}. 
#'  Centering is necessary for computing the projections, so if \code{center} is
#'  \code{FALSE}, the data must already be centered.
#' 
#'  Note that centering and scaling occur after nuisance regression, so even if
#'  \code{center} is \code{FALSE}, the data will be centered on the means if
#'  the nuisance regression included an intercept term, as it does by default.
#' @param comps_mean_dt,comps_var_dt Stabilize the mean and variance of each
#'  projection component's timecourse prior to computing kurtosis and leverage?
#'  These arguments should be \code{TRUE}, \code{FALSE} (default), or the number
#'  of DCT bases to use for detrending (\code{TRUE} will use 4). 
#'  Note that these arguments affect the projection components and not the data
#'  itself. Also, if variance-stabilizing but not mean-stabilizing, 
#'  the components must already be expected to be mean-stabilized, for example 
#'  if the data was rigorously detrended; otherwise, the results will be invalid.
#' 
#'  Slow-moving mean and variance patterns in the components will interfere with
#'  the roles of kurtosis and leverage in identifying outliers. While 
#'  \code{nuisance} can be used to detrend the data, this nuisance regression is
#'  estimated \emph{non-robustly}, since a robust model takes too long to estimate  
#'  at each data location. On the other hand, \code{comps_mean_dt} and
#'  \code{comps_var_dt} can be used to apply a \emph{robust} nuisance regression at each
#'  component, since there are much fewer components than original data locations.
#'  Thus, even if the data has been detrended with \code{nuisance} it may be
#'  helpful to detrend the components with \code{comps_mean_dt}; furthermore,
#'  the data nuisance regression does not address the potential existence of variance
#'  patterns in the components. 
#' 
#'  Overall, for fMRI we recommend enabling \code{comps_mean_dt} and \code{comps_var_dt}
#'  unless the data has been cleaned not only with a low-pass filter like 
#'  DCT nuisance regression, but also with anatomical CompCor, ICA-FIX, or
#'  a similar data-driven strategy that takes into account common sources of
#'  artifactual mean and variance trends such as motion and physiological cycles.
#' @param kurt_quantile What quantile cutoff should be used to select the
#'  components? Default: \code{0.99}. Use \code{0} to select all high-variance
#'  components regardless of kurtosis value.
#' 
#'  We model each component as a length \eqn{T} vector of Normal iid random variables, 
#'  for which the distribution of kurtosis values can be approximated. The
#'  quantile is estimated based on this distribution. 
# @param fusedPCA_kwargs Arguments to \code{\link{fusedPCA}} in list form. Valid
#  entries are:
#  
#  \describe{
#    \item{lambda}{Trend-filtering parameter. Default: \code{5}.}
#    \item{niter_max}{Maximum number of iterations. Default: \code{1000}.}
#    \item{TOL}{Convergence tolerance parameter. Default: \code{1e-8}.}
#    \item{verbose}{Print updates? Default: \code{FALSE}.}
#  }
#' @param get_dirs Should the projection directions be returned? This is the 
#'  \eqn{V} matrix in PCA and \eqn{S} matrix in ICA. The default is \code{FALSE}
#'  to save memory. However, \code{get_dirs==TRUE} is required for \code{\link{artifact_images}}.
#' @param full_PCA Only applies to the PCA projection. Return the full SVD? 
#'  Default: \code{FALSE} (return only the high-variance components).
#' @param get_outliers Should outliers be flagged based on \code{cutoff}? Default: \code{TRUE}.
#' @param cutoff Median leverage cutoff value. Default: \code{4}.
#' @param verbose Should occasional updates be printed? Default: \code{FALSE}.
#' @param seed Set a seed right before the call to \code{pesel::pesel} or 
#'  \code{ica::icaimax}? If \code{NULL}, do not set a seed. If numeric (default:
#'  \code{0}), will use as the seed.
#' @param ICA_method The \code{method} argument to \code{fastICA}: \code{"C"} to
#'  use C code with BLAS (default), or \code{"R"} to use R code.
#'
#' @name pscrub_Params
#' @keywords internal
NULL

#' fMRI data for \code{scrub} and \code{CompCor}
#' 
#' @param X Wide numeric data matrix (\eqn{T observations} by \eqn{V variables}, \eqn{T << V}).
#'  For example, if \code{X} represents an fMRI run, \eqn{T} should be the number
#'  of timepoints and \eqn{V} should be the number of brainordinate vertices/voxels.
#' 
#'  Or, a 4D array or NIFTI or file path to a NIFTI (\eqn{I} by \eqn{J} by \eqn{K} by \eqn{T} 
#'  observations), in which case \code{ROI_data} must be provided. 
#'  (The vectorized data will be \eqn{T timepoints} by \eqn{V_{in-mask} voxels})
#' 
#'  Or, a \code{ciftiTools} \code{"xifti"} object or a file path to a CIFTI
#'  (The vectorized data will be \eqn{T timepoints} by \eqn{V_{left+right+sub} grayordinates}).
#' @param ROI_data Indicates the data ROI. Allowed arguments depend on \code{X}:
#' 
#'  If \code{X} is a matrix, this must be a length \eqn{V} logical vector, where
#'  the data ROI is indicated by \code{TRUE} values. If \code{"infer"} (default), all 
#'  columns of \code{X} will be included in the data ROI (\code{rep(TRUE, V)}).
#' 
#'  If \code{X} is an array or NIFTI, this must be either a vector of values
#'  to expect for out-of-mask voxels in \code{X}, or a (file path to a) 3D NIFTI.
#'  In the latter case, each of the volume dimensions should match the first
#'  three dimensions of \code{X}. Voxels in the data ROI should be indicated by
#'  \code{TRUE} and all other voxels by \code{FALSE}. If \code{"infer"} (default),
#'  will be set to \code{c(0, NA, NaN)} (include all voxels which are not constant
#'  \code{0}, \code{NA}, or \code{NaN}).
#' 
#'  If \code{X} is a \code{"xifti"} this must be the \code{brainstructures}
#'  argument to \code{\link[ciftiTools]{read_cifti}}. If \code{"infer"} (default),
#'  \code{brainstructures} will be set to \code{"all"} (use both left and right
#'  cortex vertices, and subcortical voxels).
#'
#'  If \code{NULL}, the data ROI will be empty. This is useful for obtaining just
#'  the noise ROI, if the data and noise are located in separate files.
#' @param ROI_noise Indicates the noise ROIs for aCompCor. Should be a list where
#'  each entry corresponds to a distinct noise ROI. The names of the list should
#'  be the ROI names, e.g. \code{"white_matter"} and \code{"csf"}. The expected
#'  formats of the list entries depends on \code{X}:
#' 
#'  For all types of \code{X}, \code{ROI_noise} entries can be a matrix of noise
#'  ROI data. The matrix should have \eqn{T} rows, with each column being a
#'  data location's timeseries.
#'  
#'  If \code{X} is a matrix, entries can also indicate a noise ROI within \code{X}.
#'  These entries must be a length \eqn{V} logical vector with \code{TRUE} values 
#'  indicating locations in \code{X} within that noise ROI. Since the ROIs must 
#'  not overlap, the masks must be mutually exclusive with each other, and with 
#'  \code{ROI_data}. 
#' 
#'  If \code{X} is an array or NIFTI, entries can also indicate a noise ROI within \code{X}.
#'  These entries must be a logical array or (file path to) a 3D NIFTI with the
#'  same spatial dimensions as \code{X}, and with \code{TRUE} values indicating
#'  voxels inside the noise ROI. Since the ROIs must not overlap, the masks must
#'  be mutually exclusive with each other, and with \code{ROI_data}. 
#' 
#'  (If \code{X} is a \code{"xifti"}, entries must be data matrices, since no 
#'  grayordinate locations in \code{X} are appropriate noise ROIs).
#' @name data_CompCor_Params
#' @keywords internal
NULL
#' noise parameters for CompCor
#' @param noise_nPC The number of principal components to compute for each noise
#'  ROI. Alternatively, values between 0 and 1, in which case they will 
#'  represent the minimum proportion of variance explained by the PCs used for
#'  each noise ROI. The smallest number of PCs will be used to achieve this 
#'  proportion of variance explained. 
#' 
#'  Should be a list or numeric vector with the same length as \code{ROI_noise}. 
#'  It will be matched to each ROI based on the name of each entry, or if the 
#'  names are missing, the order of entries. If it is an unnamed vector, its
#'  elements will be recycled. Default: \code{5} (compute the top 5 PCs for 
#'  each noise ROI).
#' @param noise_erosion The number of voxel layers to erode the noise ROIs by. 
#'  Should be a list or numeric vector with the same length as \code{ROI_noise}. 
#'  It will be matched to each ROI based on the name of each entry, or if the 
#'  names are missing, the order of entries. If it is an unnamed vector, its 
#'  elements will be recycled. Default: \code{NULL}, which will use a value of
#'  0 (do not erode the noise ROIs). Note that noise erosion can only be
#'  performed if the noise ROIs are volumetric.
#' @name noise_Params
#' @keywords internal
NULL