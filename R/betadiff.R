#' The Difference between Two Beta Distributions
#'
#' Density, distribution function and quantile function for
#' the distribution of the difference of two Beta distributions with parameters parX and parY
#'
#' @typed z : numeric
#'  vector of differences between Control and Treatment arms
#' @typed parX : numeric
#'  two parameters of X's Beta distribution (Control)
#' @typed parY : numeric
#'  two parameters of Y's Beta distribution (Treatment)
#' @return `dbetadiff` gives the density, `pbetadiff` the distribution function,
#' `qbetadiff` the quantile function.
#'
#' @name betadiff
#'
#' @importFrom stats dbeta integrate
#'
#' @rdname betadiff
#'
#' @example examples/betadiff.R
#' @export
dbetadiff <- function(z, parY, parX) {
  assert_numeric(z, finite = TRUE)
  ret <- z

  # determine which z are positive and which negative
  zPos <- z >= 0
  zNeg <- z < 0
  assert_logical(zPos)
  assert_logical(zNeg)

  # use epsilon to avoid infinite function values
  eps <- .Machine$double.eps

  assert_numeric(parY, finite = TRUE)
  assert_numeric(parX, finite = TRUE)

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
  assert_number(eps)
  for (i in seq_along(z)[zPos]) {
    ret[i] <- stats::integrate(
      f = integrandPos,
      lower = eps,
      upper = 1 - z[i],
      zval = z[i],
      subdivisions = 1000L,
      rel.tol = .Machine$double.eps^0.1
    )$value
  }

  for (i in seq_along(z)[zNeg]) {
    ret[i] <- stats::integrate(
      f = integrandNeg,
      lower = eps,
      upper = 1 + z[i],
      zval = z[i],
      subdivisions = 1000L,
      rel.tol = .Machine$double.eps^0.1
    )$value
  }

  return(ret)
}

#' @importFrom stats integrate
#' @rdname betadiff
#' @inheritParams dbetadiff
#' @typed q : number
#'  vector of quantiles
#' @return `pbetadiff` the distribution function,
#' `qbetadiff` the quantile function.

#'
#' @example examples/betadiff.R
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
#' @typed p : number
#'  vector of probabilities
#' @rdname betadiff #TODO should we use @describeIn ? description
#' @return `qbetadiff` the quantile function.
#' @export
#'
#' @example examples/betadiff.R
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
