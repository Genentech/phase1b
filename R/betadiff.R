#' The Difference between Two Beta Distributions
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The Probability Density Function of the difference of two Beta Distributions.
#'
#' Density, distribution function and quantile function for
#' the distribution of the difference of two Beta distributions with parameters `parX` and `parY`.
#' We denote `X` and `Y` as two random variables representing the response rate of Control and Treatment
#' group respectively. The assignment of Control and Treatment is practically interchangeable.
#' We denote `Z` as the difference between two groups such that `Z = Y-X`.
#'
#' @typed z : numeric
#'  vector of differences between Control and Treatment arms such that `Z = Y-X`
#' @typed parX : numeric
#'  two parameters of `X`'s Beta distribution (Control)
#' @typed parY : numeric
#'  two parameters of `Y`'s Beta distribution (Treatment)
#' @typed eps : number
#'  lowest floating point number as lower bound of integration
#' @typed rel.tol : number
#'  used in [stats::integrate()]
#' @return The density values
#'
#' @note `X` and `Y` can be either Control or Treatment and `Z = X-Y`, subject to assumptions.
#'
#' @importFrom stats dbeta integrate
#' @rdname dbetadiff
#' @example examples/dbetadiff.R
#' @export
dbetadiff <- function(z, parY, parX, eps = .Machine$double.eps, rel.tol = .Machine$double.eps^0.1) {
  assert_numeric(z, min.len = 1, finite = TRUE, any.missing = TRUE, null.ok = FALSE)
  ret <- z
  is_zPos <- z >= 0
  is_zNeg <- z < 0

  assert_numeric(parY, len = 2, lower = .Machine$double.xmin, any.missing = FALSE, finite = TRUE)
  assert_numeric(parX, len = 2, lower = .Machine$double.xmin, any.missing = FALSE, finite = TRUE)
  assert_number(eps, finite = TRUE)

  integrandPos <- function(x, zval) {
    exp(
      stats::dbeta(x = x, parX[1], parX[2], log = TRUE) +
        # x + zval = y, if z = y - x
        stats::dbeta(x = x + zval, parY[1], parY[2], log = TRUE)
    )
  }
  integrandNeg <- function(y, zval) {
    exp(
      stats::dbeta(x = y, parY[1], parY[2], log = TRUE) +
        # y - zval = x, if  z = y - x
        stats::dbeta(x = y - zval, parX[1], parX[2], log = TRUE)
    )
  }
  for (i in seq_along(z)[zPos]) {
    ret[i] <- stats::integrate(
      f = integrandPos,
      # We transform the bounds to follow the support of integrandPos here.
      lower = eps,
      # The upper bounds here are between 0 and 1.
      upper = 1 - z[i],
      zval = z[i],
      subdivisions = 1000L,
      rel.tol = rel.tol
    )$value
  }

  for (i in seq_along(z)[zNeg]) {
    ret[i] <- stats::integrate(
      f = integrandNeg,
      # We transform the bounds to follow the support of integrandNeg.
      lower = eps,
      # The upper bounds here are between 0 and 1.
      upper = 1 + z[i],
      zval = z[i],
      subdivisions = 1000L,
      rel.tol = rel.tol
    )$value
  }
  ret
}

#' Beta difference Cumulative Probability Function
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Calculates the Cumulative Probability Function of the Beta difference for a given probability.
#'
#' @inheritParams dbetadiff
#' @typed q : number
#'  vector of quantiles
#'
#' @return The probability distribution value
#'
#' @importFrom stats integrate
#' @rdname pbetadiff
#' @example examples/pbetadiff.R
#' @export
pbetadiff <- function(q, parY, parX, rel.tol = .Machine$double.eps^0.1) {
  stats::integrate(
    f = dbetadiff,
    parY = parY,
    parX = parX,
    lower = -1,
    upper = q,
    subdivisions = 1000L,
    rel.tol = rel.tol
  )$value
}

#' Beta difference Quantile Function
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Calculates the quantile of the Beta difference for a given probability.
#'
#' @inheritParams dbetadiff
#' @typed p : number
#'  vector of probabilities
#' @return The quantile values.

#' @importFrom stats uniroot
#' @rdname qbetadiff
#' @example examples/qbetadiff.R
#' @export
qbetadiff <- function(p, parY, parX, eps = .Machine$double.eps) {
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
