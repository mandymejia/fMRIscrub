#' Robust kurtosis
#'
#' Robust estimate of kurtosis
#' 
#  We decided not to use this.
#'
#' @param x The data to estimate the kurtosis for
#' @return The robust kurtosis estimate
#' @export
kurtosis_rob <- function(x){
  nN <- length(x)
  x <- x - median(x)
  MAD_x <- median(abs(x))
  SD_x <- MAD_x * 1.4826
  M4_x <- sum(x^4)/nN

  # # Type I
  M2_x <- SD_x^2
  # M4_x/(M2_x^2) - 3

  # Type III
  kurt_x <- M4_x/(SD_x^4) - 3

  NULL
}