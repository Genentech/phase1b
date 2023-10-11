#' @include dbetabinom.R
#' @include postprob.R
NULL

#' Compute the posterior probability with beta prior on SOC
#'
#' @description `r lifecycle::badge("experimental")
#'
#' Using the approach by Thall and Simon (Biometrics, 1994), evaluate the
#' posterior probability of having Pr(P_E > P_S + delta | data) (but see below
#' for relative delta margin). Both for the new treatment E as well as for the
#' SOC S data might be available. However the default is that no data is
#' available for the SOC, corresponding to the single arm trial situation. Note
#' that a uniform prior is the useful default for the treatment proportion,
#' while in the single arm trial an informative prior on the SOC proportion is
#' useful.
#'
#' Beta mixture prior can be specified for the treatment (`parE`
#' and `weights` parameters) and control proportion (`parS` and
#' `weightsS` parameters), see [postprob] for details. Note
#' that being able to specify a beta mixture prior also on the control
#' treatment is e.g. important for the futility decision making (see the
#' [oc2] code).
#'
#' @typed x :
#'  number of successes (in the treatment group). Note that \code{x}
#' can be a vector.
#' @typed n :
#'  number of patients (in the treatment group).
#' @typed xS :
#'  number of successes in the SOC group (default: 0).
#' @typed nS :
#'  number of patients in the SOC group (default: 0).
#' @typed delta :
#'  margin by which the response rate in the treatment group should
#' be better than in the SOC group (default: 0).
#' @typed relativeDelta :
#'  should the delta be relative? (not default). If this is
#' `TRUE`, then a relative delta is used. This means we want to have
#' response at least in delta proportion of the SOC non-responding patients.
#' Non-responding patients rate is 1 - P_S, and we want to have P_S + (1 - P_S)
#' * delta response rate (at least) in the treatment. That is, we evaluate the
#' posterior probability Pr(P_E > P_S + (1 - P_S) * delta | data).
#' @typed parE :
#'  the beta parameters matrix, with K rows and 2 columns,
#' corresponding to the beta parameters of the K components. default is a
#' uniform prior.
#' @typed weights :
#' the mixture weights of the beta mixture prior. Default are
#' uniform weights across mixture components.
#' @typed parS :
#'  beta parameters for the SOC group (default: uniform).
#' @typed weightsS :
#'  weights for the SOC group (default: uniform).
#' @return the posterior probability
#'
#' @example examples/postprobDist.R
#' @export
postprobDist <- function(x, n,
                         xS = 0, nS = 0,
                         delta = 0,
                         relativeDelta = FALSE,
                         parE = c(1, 1),
                         weights,
                         parS = c(1, 1),
                         weightsS) {
  ## if parE is a vector => situation where there is only one component
  if (is.vector(parE)) {
    ## check that it has exactly two entries
    stopifnot(identical(length(parE), 2L))
    ## and transpose to matrix with one row
    parE <- t(parE)
  }
  ## if prior weights of the beta mixture are not supplied
  if (is.na(weights)) {
    weights <- rep(1, nrow(parE))
  }
  ## if parS is a vector => situation where there is only one component
  if (is.vector(parS)) {
    ## check that it has exactly two entries
    stopifnot(identical(length(parS), 2L))
    ## and transpose to matrix with one row
    parS <- t(parS)
  }
  ## if prior weights of the beta mixture are not supplied
  if (missing(weightsS)) {
    weightsS <- rep(1, nrow(parS))
  }
  ## compute updated beta parameters
  activeBetamixPost <- getBetamixPost(x = x, n = n, par = parE, weights = weights)
  controlBetamixPost <- getBetamixPost(x = xS, n = nS, par = parS, weights = weightsS)
  ## use numerical integration to compute this probability, as given on p.338
  ## in the article by Thall and Simon (1994):
  integrand <-
    if (relativeDelta) {
      function(p) {
        cdf <- postprob(
          x = x,
          p = (1 - delta) * p + delta,
          betamixPost = activeBetamixPost
        )
        pdf <- with(
          controlBetamixPost,
          dbetaMix(x = p, par = par, weights = weights)
        )
        cdf * pdf
      }
    } else {
      function(p) {
        cdf <- postprob(
          x = x,
          p = p + delta,
          betamixPost = activeBetamixPost
        )

        pdf <- with(
          controlBetamixPost,
          dbetaMix(x = p, par = par, weights = weights)
        )

        cdf * pdf
      }
    }
  ## do the integration. be careful to cover the region where there can
  ## really be any non-zero values. I.e. only integrate over the region where
  ## the beta density of the control is non-zero.
  epsilon <- 1e-13
  bounds <- with(
    controlBetamixPost,
    qbetaMix(
      p = c(epsilon, 1 - epsilon),
      par = par,
      weights = weights
    )
  )
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
      )
  )
  if (intRes$message == "OK") {
    return(intRes$value)
  } else {
    stop(intRes$message)
  }
}
postprobDist <- Vectorize(postprobDist, vectorize.args = "x")
