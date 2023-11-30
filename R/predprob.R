#' @include dbetabinom.R
#' @include postprob.R
NULL

#' Predictive probability of trial success
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Compute the predictive probability that the trial will be
#' successful with fixed response rate threshold
#'
#' Compute the predictive probability of trial success given current data.
#' Success means that at the end of the trial the posterior probability is
#' `Pr(P_E > p) >= thetaT`,
#' where p is the fixed response rate threshold.
#' Then the predictive probability for success is:
#' `pp = sum over i: Pr(Y=i|x,n)*I{Pr(P_E > p|x,Y=i)>=thetaT}`
#' where `Y` is the number of future responses in the treatment group and `x` is
#' the current number of responses in the treatment group out of n.
#' The prior of `Y` is `P_E ~ beta(a, b)` has prior `beta(1,1)` or other if beta mixture prior
#' specified.
#'
#' A table with the following contents will be included in the return output :
#' `i`: `Y = i`, number of future successes in `Nmax-n` subjects.
#' `density`: `Pr(Y = i|x)` using beta-(mixture)-binomial distribution.
#' `posterior`: `Pr(P_E > p | x, Y=i)` using beta posterior.
#' `result_thetaT`: indicator `I(b>thetaT)`.
#'
#' @typed x : number
#'  number of successes.
#' @typed n : number
#'  number of patients.
#' @typed Nmax : number
#'  maximum number of patients at the end of the trial.
#' @typed p : number
#'  threshold on the response rate.
#' @typed thetaT : number
#'  threshold on the probability to be above p.
#' @typed parE : numeric
#'  the beta parameters matrix, with K rows and 2 columns,
#'  corresponding to the beta parameters of the K components. Default is a
#'  uniform prior.
#' @typed weights : numeric
#'  the mixture weights of the beta mixture prior. Default are
#'  equal weights across mixture components. Weights can exceed 1 which will be
#'  normalised in `[h_getBetamixPost()]`.
#' @return A `list` is returned with names `result` for predictive probability and
#'  `table` of numeric values with counts, densities, posterior and result of indicator function.
#'
#' @references Lee, J. J., & Liu, D. D. (2008). A predictive probability
#'  design for phase II cancer clinical trials. Clinical Trials, 5(2),
#'  93-106. doi:10.1177/1740774508089279
#'
#' @example examples/predprob.R
#' @export
predprob <- function(x, n, Nmax, p, thetaT, parE = c(1, 1),
                     weights) {
  # m = Nmax - n future observations
  m <- Nmax - n
  if (is.vector(parE)) {
    assert_true(identical(length(parE), 2L))
    # and transpose to matrix with one row
    parE <- t(parE)
  }
  if (missing(weights)) {
    weights <- rep(1, nrow(parE))
  }
  betamixPost <- h_getBetamixPost(x = x, n = n, par = parE, weights = weights)

  py <- with(
    betamixPost,
    dbetabinomMix(x = 0:m, m = m, par = par, weights = weights)
  )
  # posterior probabilities to be above threshold p
  posterior <- postprob(x = x + c(0:m), n = Nmax, p = p, parE = parE, weights = weights)
  assert_numeric(density, lower = 0, upper = 1, finite = TRUE, any.missing = FALSE)
  assert_numeric(posterior, lower = 0, upper = 1, finite = TRUE, any.missing = FALSE)
  assert_number(thetaT, lower = 0, upper = 1, finite = TRUE)
  list(
    result = sum(density * (posterior > thetaT)),
    table = data.frame(
      counts = c(0:m),
      cumul_counts = n + (0:m),
      density = round(density, 4),
      posterior = round(posterior, 4),
      result_theta = ifelse((posterior > thetaT), 1, 0)
    )
  )
}
