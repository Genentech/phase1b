#' @include dbetabinom.R
#' @include postprob.R
NULL

#' Compute the predictive probability that the trial will be
#' successful, with a fixed response rate threshold
#'
#' Compute the predictive probability of trial success given current data.
#' Success means that at the end of the trial the posterior probability
#' Pr(P_E > p) >= thetaT,
#' where p is the fixed response rate threshold.
#' Then the predictive probability for success is:
#' pp = sum over i: Pr(Y=i|x,n)*I{Pr(P_E > p|x,Y=i)>=thetaT}
#' where Y is the number of future responses in the treatment group and x is
#' the current number of responses in the treatment group (out of n).
#' The prior is P_E ~ beta(a, b), default uniform which is a beta(1,1).
#' However, also a beta mixture prior can be specified.
#'
#' A table with the following contents will be included in the \code{table} attribute of
#' the return value:
#' i: Y=i (number of future successes in Nmax-n subjects)
#' py: Pr(Y=i|x) using beta-(mixture)-binomial distribution
#' b: Pr(P_E > p | x, Y=i) using beta (mixture) posterior
#' bgttheta: indicator I(b>thetaT)
#'
#' @param x number of successes
#' @param n number of patients
#' @param Nmax maximum number of patients at the end of the trial
#' @param p threshold on the response rate
#' @param thetaT threshold on the probability to be above p
#' @param parE the beta parameters matrix, with K rows and 2 columns,
#' corresponding to the beta parameters of the K components. default is a
#' uniform prior.
#' @param weights the mixture weights of the beta mixture prior. Default are
#' uniform weights across mixture components.
#' @return The predictive probability, a numeric value. In addition, a
#' list called \code{table} is returned as attribute of the returned number.
#'
#' @references Lee, J. J., & Liu, D. D. (2008). A predictive probability
#' design for phase II cancer clinical trials. Clinical Trials, 5(2),
#' 93-106. doi:10.1177/1740774508089279
#'
#' @example examples/predprob.R
#' @export
predprob <- function(x, n, Nmax, p, thetaT, parE = c(1, 1),
                     weights) {
  ## m = Nmax - n future observations
  m <- Nmax - n

  ## if par is a vector => situation where there is only one component
  if (is.vector(parE)) {
    ## check that it has exactly two entries
    stopifnot(identical(length(parE), 2L))

    ## and transpose to matrix with one row
    parE <- t(parE)
  }

  ## if prior weights of the beta mixture are not supplied
  if (missing(weights)) {
    weights <- rep(1, nrow(parE))
    ## (don't need to be normalized, this is done in getBetamixPost)
  }

  ## now compute updated parameters
  betamixPost <- getBetamixPost(x = x, n = n, par = parE, weights = weights)

  py <- with(
    betamixPost,
    dbetabinomMix(x = 0:m, m = m, par = par, weights = weights)
  )

  ## now with the beta binomial mixture:
  ## posterior probabilities to be above threshold p
  b <- postprob(x = x + c(0:m), n = Nmax, p = p, parE = parE, weights = weights)

  ## prepare the return value
  ret <- structure(sum(py * (b > thetaT)),
    table =
      round(
        cbind(
          i = c(0:m),
          py,
          b,
          bgttheta = (b > thetaT)
        ),
        4
      )
  )

  return(ret)
}
