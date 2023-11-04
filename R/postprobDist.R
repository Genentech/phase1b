#' @include dbetabinom.R
#' @include postprob.R
NULL

#' The Posterior Beta Mixture Integrand based on `delta`
#'
#' The helper function to generate Integrand function when  `relative Delta = TRUE`.
#'
#' @typed p : number
#'  probability of success or response rate of standard of care or `SOC` group.
#'
#' @return An R function that is an argument for `[stats::integrate()]`.
#'
#' @keywords internal
h_integrand_relDelta <- function(p_s, delta, x, betamixPost, controlBetamixPost) {
  cdf <- postprob(
    x = x,
    p = (1 - p_s) * delta + p_s,
    betamixPost = activeBetamixPost
  )
  pdf <- with(
    controlBetamixPost = controlBetamixPost,
    dbetaMix(x = p_s, par = par, weights = weights)
  )
  cdf * pdf
}

#' The Posterior Beta Mixture Integrand when Delta is absolute.
#'
#' The helper function to generate Integrand function when `relative Delta = FALSE`
#' , a default setting.
#' A numerical integration to compute this probability is given on p.338
#  in the article by Thall and Simon (1994, Biometrics):
#'
#' @inheritParams h_integrand_relDelta
#'
#' @return An R function that is an argument for `[stats::integrate()]`.
#'
#' @keywords internal
h_integrand <- function(p_s, delta, x, betamixPost, controlBetamixPost) {
  cdf <- postprob(
    x = x,
    p = p_s + delta,
    betamixPost = activeBetamixPost
  )
  pdf <- with(
    controlBetamixPost = controlBetamixPost,
    dbetaMix(x = p_s, par = par, weights = weights)
  )
  cdf * pdf
}

#' Generating bounds for the Integration of Beta Mixture Posterior
#'
#' Using the quantile of the Beta Mixture Distribution from parameters given by standard of care `SOC` or
#' experimental group `E` to determine bounds as inputs to `[stats::integrate()]`
#'
#' @typed betamixPost : list
#'  arguments of `par`and `weights` of Beta Mixture Posterior in format list. See `[getBetaMix()]`.
#' @typed par : matrix
#'  the beta parameters matrix, with `K` rows and 2 columns,
#'  corresponding to the beta parameters of the `K` components.
#' @typed weights : vector
#'  The mixture weights of the beta mixture prior. Default are
#'  uniform weights across mixture components.
#'
#' @return Integrand function
#'
#' @keywords internal
#'
h_get_bounds <- function(betamixPost) {
  epsilon <- .Machine$double.xmin
  with(
    betamixPost,
    qbetaMix(
      p = c(epsilon, 1 - epsilon),
      par = par,
      weights = weights
    )
  )
}

#' Compute the posterior probability with beta prior on SOC
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Using the approach by Thall and Simon (Biometrics, 1994), evaluate the
#' posterior probability of having `Pr(P_E > P_S + delta | data)` (but see below
#' for relative delta margin). Both for the new treatment `E` as well as for the
#' Standard of Care (SOC): `S` data might be available. However the default assumption is that no data is
#' available for the `SOC`, corresponding to the single arm trial situation
#' where we only rely on incoming data from the Experimental arm. In the case
#'
#' Using the approach by Thall and Simon (Biometrics, 1994), we evaluate the
#' posterior probability of having a desired improvement of treatment effect to
#' standard of care, SOC. When there is no standard of care,
#'
#' The choice of prior will consider the following :
#'
#' For single arm trial, an informative prior on the SOC proportion is
#' useful.
#' Otherwise, a uniform prior is the useful default for the treatment proportion. see @note.
#'
#' The calculation of `delta` :
#'
#' The desired improvement is denoted as `delta`. There are two options in calculating `delta`.
#' The absolute case when `relativeDelta = FALSE` and relative as when `relativeDelta = TRUE`.
#' The posterior in question can be expressed as `Pr(P_E > P_S + delta | data)`.
#'
#' 1. The absolute case is when we define an absolute delta, greater than `P_S`,
#' the response rate of the `SOC` group such that
#' the posterior is `Pr(P_E > P_S + delta | data)`.
#'
#' 2. In the relative case, we suppose that the treatment group's
#' response rate is assumed to be greater than `P_S + (1-P_S)*delta` such that
#' the posterior is `Pr(P_E > P_S + (1 - P_S) * delta | data)`.
#'
#' @note on beta Priors
#'
#' Beta mixture prior can be specified for the treatment `parE`
#' and `weights` parameters) and control proportion `parS` and
#' `weightsS` parameters), see `postprob` for details. Note
#' that being able to specify a beta mixture prior also on the control
#' treatment is e.g. important for the futility decision making (see the
#' `oc2` code).
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
#'  uniform weights across mixture components.
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
#' Beta mixture prior can be specified for the treatment in `parE`
#' and `weights` parameters) and SOC in `parS` and
#' `weightsS` parameters), see `[postprob()]` for details. Note
#' that being able to specify a beta mixture prior also on the control
#' treatment is important for the futility decision making (see the
#' `[oc2()]` code).
#'
#' @example examples/postprobDist.R
#' @export

postprobDist <- function(x,
                         n,
                         xS = 0,
                         nS = 0,
                         delta = 0,
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
  bounds <- h_get_bounds(betamixPost = controlBetamixPost)
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
    x = x,
    delta = delta,
    betamixPost = activeBetamixPost,
    controlBetamixPost = controlBetamixPost
  )
  if (intRes$message == "OK") {
    intRes$value
  } else {
    stop(intRes$message)
  }
}
postprobDist <- Vectorize(postprobDist, vectorize.args = "x")
