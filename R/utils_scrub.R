#' Summarize a \code{"IDvols_projection"} object
#'
#' Summary method for class \code{"IDvols_projection"}
#'
#' @param object Object of class \code{"IDvols_projection"}. 
#' @param ... further arguments passed to or from other methods.
#' @return A plot of the scrubbing results
#' @export
#' @method summary IDvols_projection
summary.IDvols_projection <- function(object, ...) {
  plot(object, ...)
}

#' @rdname summary.IDvols_projection
#' @export
#' 
#' @param x Object of class \code{"IDvols_projection"}. 
#' @method print summary.IDvols_projection
print.summary.IDvols_projection <- function(x, ...) {
  print(summary.IDvols_projection(x))
}

#' @rdname summary.IDvols_projection
#' @export
#' 
#' @method print IDvols_projection
print.IDvols_projection <- function(x, ...) {
  print.summary.IDvols_projection(summary(x, ...))
}

#' Summarize a \code{"IDvols_DVARS"} object
#'
#' Summary method for class \code{"IDvols_DVARS"}
#'
#' @param object Object of class \code{"IDvols_DVARS"}. 
#' @param ... further arguments passed to or from other methods.
#' @export
#' @return A plot of the scrubbing results
#' @method summary IDvols_DVARS
summary.IDvols_DVARS <- function(object, ...) {
  plot(object, ...)
}

#' @rdname summary.IDvols_DVARS
#' @export
#' 
#' @param x Object of class \code{"IDvols_DVARS"}. 
#' @method print summary.IDvols_DVARS
print.summary.IDvols_DVARS <- function(x, ...) {
  print(summary.IDvols_DVARS(x))
}

#' @rdname summary.IDvols_DVARS
#' @export
#' 
#' @method print IDvols_DVARS
print.IDvols_DVARS <- function(x, ...) {
  print.summary.IDvols_DVARS(summary(x, ...))
}

#' Summarize a \code{"IDvols_FD"} object
#'
#' Summary method for class \code{"IDvols_FD"}
#'
#' @param object Object of class \code{"IDvols_FD"}. 
#' @param ... further arguments passed to or from other methods.
#' @export
#' @return A plot of the scrubbing results
#' @method summary IDvols_FD
summary.IDvols_FD <- function(object, ...) {
  plot(object, ...)
}

#' @rdname summary.IDvols_FD
#' @export
#' 
#' @param x Object of class \code{"IDvols_FD"}. 
#' @method print summary.IDvols_FD
print.summary.IDvols_FD <- function(x, ...) {
  print(summary.IDvols_FD(x))
}

#' @rdname summary.IDvols_FD
#' @export
#' 
#' @method print IDvols_FD
print.IDvols_FD <- function(x, ...) {
  print.summary.IDvols_FD(summary(x, ...))
}