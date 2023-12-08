#' @include dbetabinom.R
#' @include postprob.R
NULL

#' The Posterior Beta Mixture Integrand when `delta` is relative
#'
#' The helper function to generate Integrand function when `relative Delta = TRUE`.
#'
#' @typed delta : number
#'  margin by which the response rate in the treatment group should
#'  be better than in the SOC group. Note that this can also be negative, e.g.
#'  when non-inferiority is being assessed.
#' @typed p_s : number
#'  probability of success or response rate of standard of care or `SOC` group.
#' @typed activeBetamixPost : list
#'  a list of posterior parameters of a beta-mixture-binomial distribution with generic names
#'  `par` and `weights`. See `[getBetaMix()]`.
#' @typed controlBetamixPost : list
#'  a list of posterior parameters of a beta-mixture-binomial distribution with generic names
#'  `par` and `weights`. See `[getBetaMix()]`.
#'
#' @return Function that is an argument for `[stats::integrate()]`.
#'
#' @keywords internal
#'
h_integrand_relDelta <- function(p_s, delta, activeBetamixPost, controlBetamixPost) {
  cdf <- postprob(
    x = 0, # Needed for Vectorize()
    p = (1 - p_s) * delta + p_s,
    betamixPost = activeBetamixPost
  )
  pdf <- with(
    controlBetamixPost,
    dbetaMix(x = p_s, par = par, weights = weights)
  )
  cdf * pdf
}

#' The Posterior Beta Mixture Integrand when Delta is absolute
#' The helper function to generate Integrand function when `relative Delta = FALSE`,
#' a default setting.
#'
#' @inheritParams h_integrand_relDelta
#'
#' @return Function that is an argument for `[stats::integrate()]`.
#'
#' @keywords internal
#'
h_integrand <- function(p_s, delta, activeBetamixPost, controlBetamixPost) {
  cdf <- postprob(
    x = 0, # Needed for Vectorize()
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
#' experimental group `E` to determine bounds as inputs to `[stats::integrate()]`.
#'
#' @inheritParams h_integrand_relDelta
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
#' posterior probability of achieving superior response rate in the treatment group compared to standard of care (SOC).
#' See note below for two formulations of the difference in response rates.
#'
#' @inheritParams h_integrand_relDelta
#' @typed x : numeric
#'  number of success counts in the treatment group.
#' @typed n : number
#'  number of patients in the treatment group.
#' @typed xS : number
#'  number of success counts in the SOC group.
#' @typed nS : number
#'  number of patients in the SOC group.
#' @typed relativeDelta : flag
#'  If `TRUE`, then a `relativeDelta` is used. Represents that a minimum
#'  response rate in magnitude of `delta` of the SOC non-responding patients. See note.
#' @typed parE : "`numeric` or `matrix`"
#'  parameters for beta distribution. If it is a matrix, it needs to have 2 columns,
#'  and each row corresponds to each component of a beta-mixture distribution
#'  for the `E` group. See details.
#' @typed weights : numeric
#'  the non-negative mixture weights of the beta mixture prior for group `E`. See details.
#' @typed parS : "`numeric` or `matrix`"
#'  parameters for beta distribution. If it is a matrix, it needs to have 2 columns,
#'  and each row corresponds to each component of a beta-mixture distribution
#'  for the `S` group. See details.
#' @typed weightsS : numeric
#'  the non-negative mixture weights of the beta mixture prior for group `S`. See details.
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
#' response rate is assumed to be greater than `P_S + (1-P_S) * delta` such that
#' the posterior is `Pr(P_E > P_S + (1 - P_S) * delta | data)`.
#'
#' @details
#'
#' The beta mixture prior for the `E` arm requires argument `parE` and `weights`.
#' The beta mixture prior for the `S` arm requires argument `parS` and `weightsS`.
#' See `[postprob()]` for details.
#'
#' If a beta-mixture is used, by default, the weights are uniform across the components.
#' Weights can exceed 1, to which the algorithm will normalize the weights such that all weights sum to 1.
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
  assert_number(n, lower = x, finite = TRUE)
  assert_numeric(x, lower = 0, upper = n, finite = TRUE)
  assert_number(nS, lower = 0, finite = TRUE)
  assert_number(xS, lower = 0, upper = nS, finite = TRUE)
  assert_number(delta, finite = TRUE)
  assert_flag(relativeDelta)
  assert_numeric(weights, lower = 0, finite = TRUE)
  assert_numeric(weightsS, lower = 0, finite = TRUE)
  assert_numeric(parE, lower = 0, finite = TRUE)
  assert_numeric(parS, lower = 0, finite = TRUE)
  activeBetamixPost <- h_getBetamixPost(x = x, n = n, par = parE, weights = weights)
  controlBetamixPost <- h_getBetamixPost(x = xS, n = nS, par = parS, weights = weightsS)
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
