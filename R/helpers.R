##'
##' Checking for scalar
##'
##' @param x the input
##' @return Returns \code{TRUE} if \code{x} is a length one vector
##' (i.e., a scalar)
##'
##' @keywords internal
is.scalar <- function(x) {
  return(identical(length(x), 1L))
}


##' Predicate checking for a boolean option
##'
##' @param x the object being checked
##' @return Returns \code{TRUE} if \code{x} is a length one logical vector (i.e., a
##' scalar)
##'
##' @keywords internal
is.bool <- function(x) {
  return(is.scalar(x) && is.logical(x))
}

##' Predicate checking for a probability
##'
##' @param x the object being checked
##' @param bounds whether to include the bounds 0 and 1 (default)
##' @return Returns \code{TRUE} if \code{x} is a probability
##'
##' @keywords internal
is.probability <- function(x,
                           bounds = TRUE) {
  return(is.scalar(x) &&
    if (bounds) {
      0 <= x && 1 >= x
    } else {
      0 < x && 1 > x
    })
}

##' Predicate checking for a probability range
##'
##' @param x the object being checked
##' @param bounds whether to include the bounds 0 and 1 (default)
##' @return Returns \code{TRUE} if \code{x} is a probability range
##'
##' @keywords internal
is.probRange <- function(x,
                         bounds = TRUE) {
  return(identical(length(x), 2L) &&
    x[1] < x[2] &&
    if (bounds) {
      0 <= x[1] && 1 >= x[2]
    } else {
      0 < x[1] && 1 > x[2]
    })
}


##' The logit function
##'
##' @param x vector of probabilities
##' @return logit(x) calculates logit(x)=log(x/(1-x))
##'
##' @importFrom stats qlogis
##'
##' @example examples/logit.R
##' @export
##' @keywords programming
logit <- function(x) {
  stats::qlogis(x)
}
