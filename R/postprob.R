#' @include dbetabinom.R
NULL

#' Posterior Probability of Efficacy Given Beta Prior
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Compute the posterior probability to be above threshold assuming a beta prior
#' on the response rate such that  `Pr(P_E > p | data)`. Prior is `P_E ~ beta(a, b)`,
#' with default set to be a uniform or beta(1,1).
#'
#' We observed `x` successes in n trials and so the posterior is
#' `P_E | data  ~ beta(a + x, b + n - x)`.
#'
#' @typed x : number
#'  number of successes.
#' @typed n : number
#'  number of patients.
#' @typed p : number
#'  threshold set to compute posterior probability.
#' @typed a : matrix
#'  first parameter `alpha` of the beta prior (successes).
#' @typed b : matrix
#'  second parameter `beta` of the beta prior (failures).
#' @return The posterior probability that the response rate P_E is above a threshold p.
#'
#' @note that `x`, can be a vector.
#'
#' @example examples/postprobOld.R
#' @export
postprobOld <- function(x, n, p, a = 1, b = 1) {
  assert_numeric(x, lower = 0, upper = n, finite = TRUE)
  assert_number(a, finite = TRUE)
  assert_number(b, finite = TRUE)
  assert_number(n, lower = 0, finite = TRUE)
  assert_number(p, lower = 0, upper = 1, finite = TRUE)
  stats::pbeta(p, a + x, b + n - x, lower.tail = FALSE)
}


#' Posterior Probability in Beta-Mixture distribution
#'
#'  @description `r lifecycle::badge("experimental")`
#'
#' Compute the posterior probability to be above threshold,
#' with a beta mixture prior on the response rate.
#'
#' Computes the posterior probability `Pr(P_E > p | data)`. Prior is
#' `P_E ~ sum(weights * beta(parE[, 1], parE[, 2]))`, i.e., a mixture of beta priors.
#' Default is one component only with uniform or `beta(1,1)`.
#'
#' We observed `x` successes in n trials.
#'
#' Posterior is again a mixture of beta priors, with updated mixture weights
#' and beta parameters.
#'
#' @typed x : numeric
#'  number of successes.
#' @typed n : number
#'  number of patients.
#' @typed p : number
#'  threshold that P_E is measured.
#' @typed parE : matrix
#'  the beta parameters matrix, with K rows and 2 columns,
#'  corresponding to the beta parameters of the K components.
#'  Default is a uniform prior.
#' @typed weights : vector
#'  The mixture weights of the beta mixture prior. Default are
#'  uniform weights across mixture components.
#' @typed betamixPost : matrix
#'  optional result of `[getBetamixPost()]` in order
#'  to speed up the computations. If supplied, this is directly used, bypassing
#'  the other arguments (except `p` and `log.p` of course).
#' @typed log.p : number
#'  Return the log of the probability? (default: `FALSE`).
#' @return The posterior probability that the response rate `P_E` is above `p`.
#'
#' @note that `x` can be a vector.
#'
#' @example examples/postprob.R
#' @export
postprob <- function(x, n, p, parE = c(1, 1), weights, betamixPost, log.p = FALSE) {
  if (missing(betamixPost)) {
    if (is.vector(parE)) {
      # Here there is only one component.
      assert_true(identical(length(parE), 2L))
      # To get matrix with one row.
      parE <- t(parE)
    }
    assert_matrix(parE)
    # if prior weights of the beta mixture are not supplied
    if (missing(weights)) {
      weights <- rep(1, nrow(parE))
    }
    ## now compute updated parameters
    betamixPost <- getBetamixPost(
      x = x,
      n = n,
      par = parE,
      weights = weights
    )
  }
  assert_list(betamixPost)
  assert_names(names(betamixPost), identical.to = c("par", "weights"))
  ## now compute the survival function at p, i.e. 1 - cdf at p:
  ret <- with(
    betamixPost,
    pbetaMix(q = p, par = par, weights = weights, lower.tail = FALSE)
  )

  if (log.p) {
    log(ret)
  } else {
    ret
  }
}
postprob <- Vectorize(postprob, vectorize.args = "x")
