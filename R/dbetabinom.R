##' Beta-binomial density function
##'
##' Calculates the density function of the beta-binomial distribution
##'
##' Note that \code{x} can be a vector.
##'
##' The beta-binomial density function has the following form:
##' \deqn{p(x) = (m! / (x!*(m-x)!)) * Beta(x+a,m-x+b) / Beta(a,b)}
##'
##' @param x number of successes
##' @param m number of trials
##' @param a first parameter of the beta distribution
##' @param b second parameter of the beta distribution
##' @return the density values of the beta-binomial distribution at \code{x}
##'
##' @example examples/dbetabinom.R
##' @export
dbetabinom <- function(x, m, a, b) {
  logRet <- lchoose(m, x) + lbeta(x + a, m - x + b) - lbeta(a, b)
  exp(logRet)
}


##' Beta-mixture-binomial density function
##'
##' Calculates the density function for a mixture of beta-binomial distributions.
##'
##' Note that \code{x} can be a vector.
##'
##' @param x number of successes
##' @param m number of trials
##' @param par the beta parameters matrix, with K rows and 2 columns,
##' corresponding to the beta parameters of the K components
##' @param weights the mixture weights of the beta mixture prior
##' @param log return the log value? (not default)
##' @return The (log) density values of the mixture of beta-binomial distributions at \code{x}.
##'
##' @export
dbetabinomMix <- function(x, m, par, weights, log = FALSE) {
  ret <- sum(weights * dbetabinom(x, m, par[, 1], par[, 2]))
  if (log) {
    return(log(ret))
  } else {
    return(ret)
  }
}
dbetabinomMix <- Vectorize(dbetabinomMix, vectorize.args = "x")


##' Computes the posterior parameters of a beta mixture
##'
##' @param x number of successes
##' @param n number of patients
##' @param par the beta parameters matrix, with K rows and 2 columns,
##' corresponding to the beta parameters of the K components
##' @param weights the mixture weights
##' @return a list with the updated beta parameters and weights
##'
##' @importFrom stats dbeta dbinom
##'
##' @example examples/getBetamixPost.R
##' @export
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


##' Beta-mixture density function
##'
##' Note that \code{x} can be a vector.
##'
##' @param x the abscissa
##' @param par the beta parameters matrix, with K rows and 2 columns,
##' corresponding to the beta parameters of the K components
##' @param weights the mixture weights of the beta mixture prior
##' @param log return the log value? (not default)
##' @return the (log) density values
##'
##' @export
dbetaMix <- function(x, par, weights, log = FALSE) {
  ret <- sum(weights * dbeta(x, par[, 1], par[, 2]))
  if (log) {
    return(log(ret))
  } else {
    return(ret)
  }
}
dbetaMix <- Vectorize(dbetaMix, vectorize.args = "x")


##' Beta-mixture cdf
##'
##' Note that \code{x} can be a vector.
##'
##' @param x the abscissa
##' @param par the beta parameters matrix, with K rows and 2 columns,
##' corresponding to the beta parameters of the K components
##' @param weights the mixture weights of the beta mixture prior
##' @param lower.tail logical; if TRUE (default), probabilities are `P[X <= x]`,
##' and otherwise `P[X > x]`
##' @return the (one minus) cdf value
##'
##' @export
pbetaMix <- function(x, par, weights, lower.tail = TRUE) {
  ret <- sum(weights * pbeta(x, par[, 1], par[, 2], lower.tail = lower.tail))
  return(ret)
}
pbetaMix <- Vectorize(pbetaMix, vectorize.args = "x")


##' Beta-mixture quantile function
##'
##' Note that \code{q} can be a vector.
##'
##' @param q the required quantile
##' @param par the beta parameters matrix, with K rows and 2 columns,
##' corresponding to the beta parameters of the K components
##' @param weights the mixture weights of the beta mixture prior
##' @return the abscissa
##'
##' @export
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
