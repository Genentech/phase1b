#' @include dbetabinom.R
#' @include postprob.R
NULL

#' Compute the predictive probability that the trial will be
#' successful, with a fixed response rate threshold
#'
#' Compute the predictive probability of trial success given current data.
#' Success means that at the end of the trial the posterior probability is
#' `Pr(P_E > p) >= thetaT`,
#' where p is the fixed response rate threshold.
#' Then the predictive probability for success is:
#' `pp = sum over i: Pr(Y=i|x,n)*I{Pr(P_E > p|x,Y=i)>=thetaT}`
#' where `Y` is the number of future responses in the treatment group and `x` is
#' the current number of responses in the treatment group out of n.
#' The prior is `P_E ~ beta(a, b)`, default uniform which is a `beta(1,1)`.
#' However, a beta mixture prior can also be specified.
#'
#' A table with the following contents will be included in the return output :
#' `i`: `Y = i`, number of future successes in Nmax-n subjects.
#' `density`: `Pr(Y = i|x)` using beta-(mixture)-binomial distribution.
#' `posterior`: `Pr(P_E > p | x, Y=i)` using beta posterior.
#' bgttheta: indicator `I(b>thetaT)`.
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
#'  normalised in `getBetamixPost`
#' @return The predictive probability, a numeric value. In addition, a
#'  `list` called `table` is returned as attribute of the returned number.
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
  betamixPost <- getBetamixPost(x = x, n = n, par = parE, weights = weights)
  assert_list(betamixPost, any.missing = FALSE, types = "numeric")
  density <- with(
    betamixPost,
    dbetabinomMix(x = 0:m, m = m, par = par, weights = weights)
  )
  # posterior probabilities to be above threshold p
  posterior <- postprob(x = x + c(0:m), n = Nmax, p = p, parE = parE, weights = weights)
  structure(sum(density * (posterior > thetaT)),
    table =
      round(
        cbind(
          i = c(0:m),
          density,
          posterior,
          bgttheta = (posterior > thetaT)
        ),
        4
      )
  )
}
