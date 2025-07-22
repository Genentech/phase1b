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
  assert_number(x, lower = 0, upper = n, finite = TRUE)
  assert_number(n, lower = 0, finite = TRUE)
  assert_matrix(par, min.rows = 1, max.cols = 2, mode = "numeric")
  assert_numeric(weights, min.len = 0, len = nrow(par), finite = TRUE)
  # We renormalize weights.
  weights <- weights / sum(weights)
  # We now compute updated parameters.
  postPar <- par
  postPar[, 1] <- postPar[, 1] + x
  postPar[, 2] <- postPar[, 2] + n - x
  # We compute updated mixture probabilities.
  tmp <- exp(
    lbeta(a = postPar[, 1], b = postPar[, 2]) - lbeta(a = par[, 1], b = par[, 2])
  )
  # We compute the updated weights of the posterior
  # postWeights <- weights * tmp / sum(weights * tmp)
  postWeights <- exp(log(weights) + log(tmp)) - sum(weights * tmp)
  assert_numeric(postWeights)
  list(
    par = postPar,
    weights = postWeights
  )
}

#' Beta-Mixture density function
#'
#' This function calculates beta-mixture density values.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The beta-mixture distribution is defined by K beta parameters and the corresponding weights.
#'
#' @inheritParams dbetabinom
#' @inheritParams dbetabinomMix
#' @typed log : flag
#'  whether log values of the beta-mixture density function are returned.
#'
#' @return the (log) density values
#'
#' @example examples/dbetaMix.R
#' @export
dbetaMix <- function(x, par, weights, log = FALSE) {
  assert_numeric(weights, lower = 0, upper = 1, finite = TRUE, any.missing = FALSE)
  assert_true(all.equal(sum(weights), 1))
  assert_true(identical(length(weights), nrow(par)))
  degree <- length(weights)

  component_densities <- matrix(
    dbeta(rep(x, each = degree), par[, 1], par[, 2]),
    nrow = degree,
    ncol = length(x)
  )
  ret <- as.numeric(weights %*% component_densities)
  if (log) {
    log(ret)
  } else {
    ret
  }
}


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
#' @importFrom stats pbeta
#' @export
pbetaMix <- function(q, par, weights, lower.tail = TRUE) {
  assert_numeric(q, lower = 0, upper = 1, finite = TRUE)
  assert_numeric(weights, lower = 0, upper = 1, finite = TRUE)
  assert_matrix(par)
  assert_flag(lower.tail)
  .pbetaMix(q = q, par = par, weights = weights, lower.tail = lower.tail)
}

.pbetaMix <- function(q, par, weights, lower.tail) {
  degree <- length(weights)
  component_p <- matrix(
    pbeta(rep(q, each = degree), par[, 1], par[, 2], lower.tail = lower.tail),
    nrow = degree,
    ncol = length(q)
  )
  as.numeric(weights %*% component_p)
}


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
  assert_numeric(p, lower = 0, upper = 1)
  assert_numeric(weights, lower = 0, upper = 1, finite = TRUE)
  assert_matrix(par)
  assert_flag(lower.tail)

  grid <- seq(0, 1, len = 31)
  f_grid <- .pbetaMix(grid, par, weights, lower.tail = lower.tail)

  sapply(p, function(p) {
    # special cases
    if (p == 0) {
      return(0)
    }
    if (p == 1) {
      return(1)
    }

    diff <- f_grid - p
    pos <- diff > 0
    grid_interval <- c(grid[!pos][which.max(diff[!pos])], grid[pos][which.min(diff[pos])])

    uniroot(
      f = function(q) .pbetaMix(q, par, weights, lower.tail = lower.tail) - p,
      interval = grid_interval,
      f.lower = -p,
      f.upper = 1 - p,
      tol = sqrt(.Machine$double.eps)
    )$root
  })
}
