#' Beta-Binomial Density Function
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Calculates the density function of the beta-binomial distribution.
#'
#' The beta-binomial density function has the following form:
#' `p(x) = (m! / (x!*(m-x)!)) * Beta(x+a,m-x+b) / Beta(a,b)`
#'
#' @typed x : numeric
#'  number of successes. Can be a vector of `length(x) > 1`.
#' @typed m : number
#'  number of trials.
#' @typed a : numeric
#'  first parameter of the beta distribution.
#' @typed b : numeric
#'  second parameter of the beta distribution.
#' @typed log : flag
#'  whether to return the log density value (not default).
#' @return The density values of the beta-binomial distribution at `x`.
#'
#' @example examples/dbetabinom.R
#' @export
dbetabinom <- function(x, m, a, b, log = FALSE) {
  assert_numeric(x, lower = 0, upper = m, finite = TRUE)
  assert_number(m, lower = 0, finite = TRUE)
  assert_numeric(a, lower = 0, finite = TRUE)
  assert_numeric(b, lower = 0, finite = TRUE)
  assert_flag(log)
  log_ret <- lchoose(m, x) + lbeta(x + a, m - x + b) - lbeta(a, b)
  if (log) {
    log_ret
  } else {
    exp(log_ret)
  }
}

#' Beta-Mixture-Binomial Density Function
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Calculates the density function for a mixture of beta-binomial distributions.
#'
#' @inheritParams dbetabinom
#' @typed par : matrix
#'  the beta parameters matrix, with K rows and 2 columns,
#'  corresponding to the beta parameters of the K components.
#' @typed weights : numeric
#'  the mixture weights of the beta mixture prior of length K.
#'  Each element corresponds to the row of beta parameters in `par`.
#' @typed log : flag
#'  whether to return the log density value (not default).
#' @return The (log) density values of the mixture of beta-binomial distributions at `x`.
#'
#' @example examples/dbetabinomMix.R
#' @export
dbetabinomMix <- function(x, m, par, weights, log = FALSE) {
  assert_matrix(par, min.rows = 1, min.cols = 2)
  assert_flag(log)
  ret <- sum(weights * dbetabinom(x, m, par[, 1], par[, 2]))
  if (log) {
    log(ret)
  } else {
    ret
  }
}
dbetabinomMix <- Vectorize(dbetabinomMix, vectorize.args = "x")


#' Compute Beta-Mixture-Binomial Posterior Distribution
#'
#' A helper function that computes the posterior parameters of a beta-mixture-binomial distribution.
#'
#' @inheritParams dbetabinom
#' @inheritParams dbetabinomMix
#'
#' @return A list with the updated beta parameters and weights.
#'
#' @importFrom stats dbeta dbinom
#'
#' @keywords internal
h_getBetamixPost <- function(x, n, par, weights) {
  assert_numeric(x, lower = 0, upper = n, finite = TRUE)
  assert_numeric(n, lower = 0, finite = TRUE)
  assert_matrix(par)
  assert_numeric(weights, min = 0, len = nrow(par), finite = TRUE)
  # We renormalize weights.
  weights <- weights / sum(weights)
  # We now compute updated parameters.
  postPar <- par
  postPar[, 1] <- postPar[, 1] + x
  postPar[, 2] <- postPar[, 2] + n - x
  postParProb <- postPar[, 1] / (postPar[, 1] + postPar[, 2])
  # We compute updated mixture probabilities.
  tmp <- exp(
    stats::dbinom(x, size = n, prob = postParProb, log = TRUE) +
      stats::dbeta(postParProb, par[, 1], par[, 2], log = TRUE) -
      stats::dbeta(postParProb, postPar[, 1], postPar[, 2], log = TRUE)
  )
  # We compute the updated weights of the posterior
  postWeights <- weights * tmp / sum(weights * tmp)
  assert_numeric(postWeights)
  list(
    par = postPar,
    weights = postWeights
  )
}


#' Beta-Mixture density function
#'
#' Calculating `log` or non-log Beta-Mixture density values of support `x`.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The Beta Mixture density can be calculated with the combination of K set of beta parameters and K
#' length of weights.
#'
#' @inheritParams dbetabinom
#' @inheritParams dbetabinomMix
#' @typed log : flag
#'  Default is `FALSE`. If `TRUE`,log values of the Beta-Mixture density function are returned
#'
#' @return the (log) density values
#'
#' @export
dbetaMix <- function(x, par, weights, log = FALSE) {
  ret <- sum(weights * dbeta(x, par[, 1], par[, 2]))
  if (log) {
    log(ret)
  } else {
    ret
  }
}
dbetaMix <- Vectorize(dbetaMix, vectorize.args = "x")


#' Beta-Mixture CDF
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Calculates the CDF of the Beta-Mixture distribution.
#'
#' @typed q : number
#'  the abscissa.
#' @typed par : matrix
#'  the beta parameters matrix, with K rows and 2 columns,
#'  corresponding to the beta parameters of the K components.
#' @typed weights : numeric
#'  the mixture weights of the beta mixture prior which add up to 1.
#' @typed lower.tail : flag
#'  if `TRUE` (default), probabilities are `P[X <= x]`,
#'  and otherwise `P[X > x]`.
#' @return The (one minus) cdf value
#'
#' @note `q` can be a vector.
#'
#' @example examples/pbetaMix.R
#' @export
pbetaMix <- function(q, par, weights, lower.tail = TRUE) {
  assert_number(q, lower = 0, upper = 1, finite = TRUE)
  assert_numeric(weights, lower = 0, upper = 1, finite = TRUE)
  assert_matrix(par)
  assert_flag(lower.tail)
  sum(weights * pbeta(q, par[, 1], par[, 2], lower.tail = lower.tail))
}
pbetaMix <- Vectorize(pbetaMix, vectorize.args = "q")


#' Beta-Mixture Quantile Function
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Calculates the quantile of the Beta-Mixture distribution for a given probability.
#'
#' @typed p : numeric
#'  the required probability.
#' @typed par : number
#'  the beta parameters matrix, with K rows and 2 columns,
#'  corresponding to the beta parameters of the K components.
#' @typed weights : matrix
#'  the mixture weights of the beta mixture prior.
#' @typed lower.tail : flag
#'  whether CDF at x taken at lower or upper tail
#' @return The abscissa.
#'
#' @example examples/qbetaMix.R
#' @export
qbetaMix <- function(p, par, weights, lower.tail = TRUE) {
  f <- function(pi) {
    pbetaMix(q = pi, par = par, weights = weights, lower.tail = lower.tail) - p
  }
  unirootResult <- uniroot(f, lower = 0, upper = 1)
  if (unirootResult$iter < 0) {
    NA
  } else {
    assert_number(unirootResult$root)
    unirootResult$root
  }
}
qbetaMix <- Vectorize(qbetaMix, vectorize.args = "p")
