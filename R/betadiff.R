#' The Difference between Two Beta Distributions
#'
#' Density, distribution function and quantile function for
#' the distribution of the difference of two beta distributions with parameters parX and parY
#'
#' @param z vector of differences
#' @param q vector of quantiles
#' @param p vector of probabilities
#' @param parX two parameters of X's beta distribution (Control)
#' @param parY two parameters of Y's beta distribution (Treatment)
#'
#' @return \code{dbetadiff} gives the density, \code{pbetadiff} the distribution function,
#' \code{qbetadiff} the quantile function.
#'
#' @example examples/betadiff.R
#' @name betadiff
#' @importFrom stats dbeta integrate
#' @rdname betadiff
#' @export
dbetadiff <- function(z, parY, parX) {
  ret <- z

  ## determine which z are positive and which negative
  zPos <- z >= 0
  zNeg <- z < 0

  ## use epsilon to avoid infinite function values
  eps <- .Machine$double.eps

  integrandPos <- function(x, zval) {
    exp(
      stats::dbeta(x = x, parX[1], parX[2], log = TRUE) +
        stats::dbeta(x = x + zval, parY[1], parY[2], log = TRUE)
    )
  }

  integrandNeg <- function(y, zval) {
    exp(
      stats::dbeta(x = y, parY[1], parY[2], log = TRUE) +
        stats::dbeta(x = y - zval, parX[1], parX[2], log = TRUE)
    )
  }

  for (i in seq_along(z)[zPos]) {
    ret[i] <- stats::integrate(
      f = integrandPos,
      lower = eps, upper = 1 - z[i],
      zval = z[i],
      subdivisions = 1000L,
      rel.tol = .Machine$double.eps^0.1
    )$value
  }

  for (i in seq_along(z)[zNeg]) {
    ret[i] <- stats::integrate(
      f = integrandNeg,
      lower = eps, upper = 1 + z[i],
      zval = z[i],
      subdivisions = 1000L,
      rel.tol = .Machine$double.eps^0.1
    )$value
  }

  return(ret)
}

#' @importFrom stats integrate
#' @rdname betadiff
#' @export
pbetadiff <- function(q, parY, parX) {
  stats::integrate(
    f = dbetadiff,
    parY = parY,
    parX = parX,
    lower = -1,
    upper = q,
    subdivisions = 1000L,
    rel.tol = .Machine$double.eps^0.1
  )$value
}


#' @importFrom stats uniroot
#' @rdname betadiff
#' @export
qbetadiff <- function(p, parY, parX) {
  target <- function(q) {
    pbetadiff(
      q = q,
      parY = parY,
      parX = parX
    ) - p
  }
  eps <- 1e-10
  stats::uniroot(
    f = target,
    interval = c(-1 + eps, 1 - eps)
  )$root
}