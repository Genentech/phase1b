#' @include dbetabinom.R
#' @include postprob.R
NULL

#' The Posterior Beta Mixture Integrand when `delta` is relative.
#'
#' The helper function to generate Integrand function when  `relative Delta = TRUE`.
#'
#' A numerical integration to compute this probability is given on p.338
#  in the article by Thall and Simon (1994, Biometrics):
#'
#' @typed delta : numeric
#'  the margin of which treatment group `E` is superior than the success rate of
#'  the standard of care `S`. If the `p_S` or success rate of `S` is `0`,
#'  then the difference between two groups is merely `delta`. See also @note
#' @typed p_s : number
#'  probability of success or response rate of standard of care or `SOC` group.
#' @typed activeBetamixPost : list
#'  a list of posterior parameters of a beta-mixture-binomial distribution with generic names
#'  `par` and `weights`. See `[getBetaMix()]`.
#' @typed controlBetamixPost : list
#'  a list of posterior parameters of a beta-mixture-binomial distribution with generic names
#'  `par` and `weights`. See `[getBetaMix()]`.
#'
#' @return An R function that is an argument for `[stats::integrate()]`.
#'
#' @keywords internal
h_integrand_relDelta <- function(p_s, delta, activeBetamixPost, controlBetamixPost) {
  cdf <- postprob(
    x = 0, # dummy x for Vectorize()
    p = (1 - p_s) * delta + p_s,
    betamixPost = activeBetamixPost
  )
  pdf <- with(
    controlBetamixPost,
    dbetaMix(x = p_s, par = par, weights = weights)
  )
  cdf * pdf
}

#' The Posterior Beta Mixture Integrand when Delta is absolute.
#'
#' The helper function to generate Integrand function when `relative Delta = FALSE`,
#' a default setting.
#' See `[postprobDist()]`
#'
#' A numerical integration to compute this probability is given on p.338
#  in the article by Thall and Simon (1994, Biometrics):
#'
#' @inheritParams h_integrand_relDelta
#'
#' @return An R function that is an argument for `[stats::integrate()]`.
#'
#' @keywords internal
#'
h_integrand <- function(p_s, delta, activeBetamixPost, controlBetamixPost) {
  cdf <- postprob(
    x = 0, # dummy x for Vectorize()
    p = p_s + delta,
    betamixPost = activeBetamixPost
  )
  pdf <- with(
    controlBetamixPost,
    dbetaMix(x = p_s, par = par, weights = weights)
  )
  cdf * pdf
}

#' Generating bounds for the Integration of Beta Mixture Posterior
#'
#' Using the quantile of the Beta Mixture Distribution from parameters given by standard of care `SOC` or
#' experimental group `E` to determine bounds as inputs to `[stats::integrate()]`
#'
#' @typed controlbetamixPost : list
#'  arguments of `par`and `weights` of Beta Mixture Posterior in format list. See `[getBetaMix()]`.
#' @return Integrand function
#'
#' @keywords internal
#'
h_get_bounds <- function(controlBetamixPost) {
  epsilon <- .Machine$double.xmin
  with(
    controlBetamixPost,
    qbetaMix(
      p = c(epsilon, 1 - epsilon),
      par = par,
      weights = weights
    )
  )
}

#' Compute the Posterior Probability with Beta Prior on `SOC`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Using the approach by Thall and Simon (Biometrics, 1994), this evaluates the
#' posterior probability of achieving superior response rate in the treatment group compared to  standard of care (SOC).
#' See notes below for two formulations of the difference in response rates.
#'
#' In reality, data may or may not be complete for both the new treatment `E` as well as for the SOC group,
#' `S`. Accordingly prior distribution should be specified.
#'
#' 1. No precedent data :
#' The default setting is a uniform prior of `Beta(1,1)`. This can be used to reflect no precedent data
#' in both the `E` and `S` arms.
#'
#' 2a. Precedent data for only either `E` :
#' A user input prior is given by user to reflect precedent data of the `E` arm.
#' For each set of prior parameters, user can input weighting. See (4)
#'
#' 2b. Precedent data for only either `S` :
#' A user input prior is given by user to reflect precedent data of the `S` arm.
#' For each set of prior parameters, user can input weighting. See (4)
#'
#' Choice of Weights
#'
#' 3. In the simple case of no mixture of priors, the one Beta parameter are weighted as `100 %`.
#'
#' 4. In the Beta Binomial Mixture case, users can allocate a non-negative weighting and can exceed `100 %`,
#'  which the algorithm will normalised such that all weights sum to 1.
#'
#' @typed x : vector
#'  vector of success counts in the treatment group. Vector of minimum length of 1.
#' @typed n : number
#'  number of patients in the treatment group.
#' @typed xS : vector
#'  vector of success counts in the SOC group (default: 0). Vector of minimum length of 1.
#' @typed nS : number
#'  number of patients in the SOC group (default: 0)
#' @typed delta : number
#'  margin by which the response rate in the treatment group should
#'  be better than in the SOC group (default: 0). Must be >= `0`.  see @note.
#' @typed relativeDelta : flag
#'  If `TRUE`, then a `relativeDelta` is used. Represents that a minimum
#'  response rate in magnitude of `delta` of the SOC non-responding patients. see @note.
#' @typed parE : matrix
#'  the beta parameters matrix, with K rows and 2 columns,
#'  corresponding to the beta parameters of the K components. default is a
#'  uniform prior.
#' @typed weights : matrix
#'  the mixture weights of the beta mixture prior. Default are
#'  equal weights across mixture components.
#' @typed parS : matrix
#'  beta parameters for the SOC group (default: uniform)
#' @typed weightsS : matrix
#'  weights for the SOC group (default: uniform)
#' @typed epsilon : number
#'  the smallest non-negative floating number to represent the lower bound for
#'  the interval of integration.
#' @return The posterior probability
#'
#' @note
#'
#' ## Delta :
#'
#' The desired improvement is denoted as `delta`. There are two options in using `delta`.
#' The absolute case when `relativeDelta = FALSE` and relative as when `relativeDelta = TRUE`.
#'
#' 1. The absolute case is when we define an absolute delta, greater than `P_S`,
#' the response rate of the `SOC` group such that
#' the posterior is `Pr(P_E > P_S + delta | data)`.
#'
#' 2. In the relative case, we suppose that the treatment group's
#' response rate is assumed to be greater than `P_S + (1-P_S)*delta` such that
#' the posterior is `Pr(P_E > P_S + (1 - P_S) * delta | data)`.
#'
#' @details
#'
#' The beta mixture prior for the E arm requires argument `parE` and `weights`.
#' The beta mixture prior for the E arm requires argument `parS` and `weightsS`.
#' See `[postprob()]` for details.
#'
#' @example examples/postprobDist.R
#' @export

postprobDist <- function(x,
                         n,
                         xS = 0,
                         nS = 0,
                         delta,
                         relativeDelta = FALSE,
                         parE = c(1, 1),
                         weights,
                         parS = c(1, 1),
                         weightsS) {
  if (is.vector(parE)) {
    assert_true(identical(length(parE), 2L))
    parE <- t(parE)
  }
  if (is.vector(parS)) {
    assert_true(identical(length(parS), 2L))
    parS <- t(parS)
  }
  if (missing(weights)) {
    weights <- rep(1, nrow(parE))
  }
  if (missing(weightsS)) {
    weightsS <- rep(1, nrow(parS))
  }
  activeBetamixPost <- getBetamixPost(x = x, n = n, par = parE, weights = weights)
  controlBetamixPost <- getBetamixPost(x = xS, n = nS, par = parS, weights = weightsS)
  assert_names(names(activeBetamixPost), identical.to = c("par", "weights"))
  assert_names(names(controlBetamixPost), identical.to = c("par", "weights"))
  if (relativeDelta) {
    epsilon <- .Machine$double.xmin
    integrand <- h_integrand_relDelta
  } else {
    epsilon <- .Machine$double.xmin
    integrand <- h_integrand
  }
  bounds <- h_get_bounds(controlBetamixPost = controlBetamixPost)
  intRes <- integrate(
    f = integrand,
    lower =
      max(
        bounds[1],
        ifelse(relativeDelta, 0, 0 - delta)
      ),
    upper =
      min(
        ifelse(relativeDelta, 1, 1 - delta),
        bounds[2]
      ),
    delta = delta,
    activeBetamixPost = activeBetamixPost,
    controlBetamixPost = controlBetamixPost
  )
  if (intRes$message == "OK") {
    intRes$value
  } else {
    stop(intRes$message)
  }
}
postprobDist <- Vectorize(postprobDist, vectorize.args = "x")
