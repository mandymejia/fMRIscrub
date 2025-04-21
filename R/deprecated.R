#' @rdname flags2spikes
#' @export
flags_to_nuis_spikes <- function(flags, n_time){
  message("`flags_to_nuis_spikes` has been renamed to `flags2spikes` and will be removed in an upcoming version.")
  flags2spikes(flags, n_time)
}

#' @rdname IDvols
#' @export
scrub <- function(X, method=c("projection", "DVARS"), ...) {
  message("`scrub` has been renamed to `IDvols` and will be removed in an upcoming version.")
  IDvols(X, method, ...)
}

#' @rdname IDvols_xifti
#' @export
scrub_xifti <- function(X, method=c("projection", "DVARS"), brainstructures=c("left", "right"), ...) {
  message("`scrub_xifti` has been renamed to `IDvols_xifti` and will be removed in an upcoming version.")
  IDvols_xifti(X, method=method, brainstructures=brainstructures, ...)
}