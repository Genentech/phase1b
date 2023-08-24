#' Beta-binomial density function
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Calculates the density function of the beta-binomial distribution
#'
#' Note that `x` can be a vector.
#'
#' The beta-binomial density function has the following form:
#' `p(x) = (m! / (x!*(m-x)!)) * Beta(x+a,m-x+b) / Beta(a,b)`
#'
#' @typed x : numeric
#'  number of successes.
#' @typed m : number
#'  number of trials.
#' @typed a : number
#'  first parameter of the beta distribution.
#' @typed b : number
#'  second parameter of the beta distribution.
#' @typed log : flag
#'  return the log value (not default).
#' @return The density values of the beta-binomial distribution at `x`.
#'
#' @example examples/dbetabinom.R
#' @export
dbetabinom <- function(x, m, a, b, log = FALSE) {
  assert_numeric(x, lower = 0, upper = m, finite = TRUE)
  assert_number(m, lower = 0, finite = TRUE)
  assert_number(a, lower = 0, finite = TRUE)
  assert_number(b, lower = 0, finite = TRUE)
  logRet <- lchoose(m, x) + lbeta(x + a, m - x + b) - lbeta(a, b)
  if (log == FALSE) {
    exp(logRet)
  } else {
    logRet
  }
}


#' Beta-mixture-binomial density function
#'
#' Calculates the density function for a mixture of beta-binomial distributions.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Note that `x` can be a vector.
#'
#' @typed x : number or vector
#'  number of successes.
#' @typed m : number
#'  number of trials.
#' @typed par : matrix
#'  the beta parameters matrix, with K rows and 2 columns,
#'  corresponding to the beta parameters of the K components.
#' @typed weights : numeric
#'   the mixture weights of the beta mixture prior.
#' @typed log : flag
#'  return the log value (not default).
#' @return The (log) density values of the mixture of beta-binomial distributions at `x`.
#'
#' @export
dbetabinomMix <- function(x, m, par, weights, log = FALSE) {
  assert_matrix(par, min.rows = 1, min.cols = 2)
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


#' Beta-mixture cdf
#'
#' Note that `x` can be a vector.
#'
#' @param x the abscissa.
#' @param par the beta parameters matrix, with K rows and 2 columns,
#' corresponding to the beta parameters of the K components.
#' @param weights the mixture weights of the beta mixture prior.
#' @param lower.tail logical; if TRUE (default), probabilities are `P[X <= x]`,
#' and otherwise `P[X > x]`.
#' @return the (one minus) cdf value.
#'
#' @export
pbetaMix <- function(x, par, weights, lower.tail = TRUE) {
  ret <- sum(weights * pbeta(x, par[, 1], par[, 2], lower.tail = lower.tail))
  return(ret)
}
pbetaMix <- Vectorize(pbetaMix, vectorize.args = "x")


#' Beta-mixture quantile function
#'
#' Note that `x` can be a vector.
#'
#' @param q  the required quantile.
#' @param par  the beta parameters matrix, with K rows and 2 columns,
#' corresponding to the beta parameters of the K components.
#' @param weights  the mixture weights of the beta mixture prior.
#' @return the abscissa.
#'
#' @export
qbetaMix <- function(q, par, weights) {
  f <- function(pi) {
    pbetaMix(x = pi, par = par, weights = weights) - q
  }
  unirootResult <- uniroot(f, lower = 0, upper = 1)
  if (unirootResult$iter < 0) {
    return(NA)
  } else {
    return(unirootResult$root)
  }
}
qbetaMix <- Vectorize(qbetaMix, vectorize.args = "q")
