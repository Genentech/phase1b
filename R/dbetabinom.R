#' Beta-binomial density function
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Calculates the density function of the beta-binomial distribution.
#'
#' The beta-binomial density function has the following form:
#' `p(x) = (m! / (x!*(m-x)!)) * Beta(x+a,m-x+b) / Beta(a,b)`
#'
#' @typed x : numeric
#'  number of successes.
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
#' @note `x`, `a` and `b` can be vectors.
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


#' Beta-mixture-binomial density function
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Calculates the density function for a mixture of beta-binomial distributions.
#'
#' @typed x : numeric
#'  number of successes.
#' @typed m : number
#'  number of trials.
#' @typed par : matrix
#'  the beta parameters matrix, with K rows and 2 columns,
#'  corresponding to the beta parameters of the K components.
#' @typed weights : numeric
#'  the mixture weights of the beta mixture prior.
#' @typed log : flag
#'  whether to return the log density value (not default).
#' @return The (log) density values of the mixture of beta-binomial distributions at `x`.
#' @note `x` can be a vector.
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


#' Computes the posterior parameters of a beta mixture
#'
#' @param x number of successes
#' @param n number of patients
#' @param par the beta parameters matrix, with K rows and 2 columns,
#' corresponding to the beta parameters of the K components
#' @param weights the mixture weights
#' @return a list with the updated beta parameters and weights
#'
#' @importFrom stats dbeta dbinom
#'
#' @example examples/getBetamixPost.R
#' @export
getBetamixPost <- function(x, n, par, weights) {
  ## check the format
  stopifnot(
    is.matrix(par),
    is.numeric(par),
    identical(ncol(par), 2L),
    all(par > 0),
    identical(nrow(par), length(weights)),
    all(weights > 0)
  )

  ## renormalize weights
  weights <- weights / sum(weights)

  ## now compute updated parameters
  postPar <- par
  postPar[, 1] <- postPar[, 1] + x
  postPar[, 2] <- postPar[, 2] + n - x
  postParProb <- postPar[, 1] / (postPar[, 1] + postPar[, 2])

  ## compute updated mixture probabilities
  tmp <- exp(
    stats::dbinom(x, size = n, prob = postParProb, log = TRUE) +
      stats::dbeta(postParProb, par[, 1], par[, 2], log = TRUE) -
      stats::dbeta(postParProb, postPar[, 1], postPar[, 2], log = TRUE)
  )

  postWeights <- weights * tmp / sum(weights * tmp)

  return(list(
    par = postPar,
    weights = postWeights
  ))
}


#' Beta-mixture density function
#'
#' Note that `x` can be a vector.
#'
#' @param x the abscissa
#' @param par the beta parameters matrix, with K rows and 2 columns,
#' corresponding to the beta parameters of the K components
#' @param weights the mixture weights of the beta mixture prior
#' @param log return the log value? (not default)
#' @return the (log) density values
#'
#' @export
dbetaMix <- function(x, par, weights, log = FALSE) {
  ret <- sum(weights * dbeta(x, par[, 1], par[, 2]))
  if (log) {
    return(log(ret))
  } else {
    return(ret)
  }
}
dbetaMix <- Vectorize(dbetaMix, vectorize.args = "x")


#' Beta-Mixture CDF
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Calculates the cdf of the beta-mixture
#'
#' @typed x : number
#'  the abscissa.
#' @typed par : matrix
#'  the beta parameters matrix, with K rows and 2 columns,
#'  corresponding to the beta parameters of the K components.
#' @typed weights : matrix
#'  the mixture weights of the beta mixture prior.
#' @typed lower.tail : logical # TODO Why not flag
#'  if TRUE (default), probabilities are `P[X <= x]`,
#'  and otherwise `P[X > x]`.
#' @return the (one minus) cdf value # TODO DO WE NEED THIS return and where is the "1-".
#'
#' @note `x` can be a vector.
#'
#' @example examples/pbetaMix.R
#' @export
pbetaMix <- function(x, par, weights, lower.tail = TRUE) {
  assert_numeric(x, lower = 0, finite = TRUE)
  assert_numeric(weights, lower = 0, upper = 1, finite = TRUE)
  assert_matrix(par)
  assert_flag(lower.tail)
  ret <- sum(weights * pbeta(x, par[, 1], par[, 2], lower.tail = lower.tail))
  ret
}
pbetaMix <- Vectorize(pbetaMix, vectorize.args = "x")


#' Beta-mixture quantile function
#'
#'  @description `r lifecycle::badge("experimental")`
#'
#' Calculates the quantile where x support is at the intersection of cdf and quantile function
#'
#' @typed q : numeric
#'  the required quantile.
#' @typed par : number
#'  the beta parameters matrix, with K rows and 2 columns,
#'  corresponding to the beta parameters of the K components.
#' @typed weights : matrix
#'  the mixture weights of the beta mixture prior.
#' @typed lower.tail : flag
#'  whether cdf at x taken at lower or upper tail
#' @return the abscissa.
#'
#' @example examples/qbetaMix.R
#' @export
qbetaMix <- function(q, par, weights, lower.tail) {
  f <- function(pi) {
    assert_numeric(pi, lower = 0, finite = TRUE)
    pbetaMix(x = pi, par = par, weights = weights) - q
  }
  unirootResult <- uniroot(f, lower = 0, upper = 1)
  assert_number(unirootResult$f.root)
  if (unirootResult$iter < 0) {
    NA
  } else {
    unirootResult$root
  }
}
qbetaMix <- Vectorize(qbetaMix, vectorize.args = "q")
