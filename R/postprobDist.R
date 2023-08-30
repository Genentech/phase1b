#' @include dbetabinom.R
#' @include postprob.R
NULL

#' Compute the posterior probability with beta prior on SOC
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
#' Beta mixture prior can be specified for the treatment (\code{parE}
#' and \code{weights} parameters) and control proportion (\code{parS} and
#' \code{weightsS} parameters), see \code{\link{postprob}} for details. Note
#' that being able to specify a beta mixture prior also on the control
#' treatment is e.g. important for the futility decision making (see the
#' \code{\link{oc2}} code).
#'
#' @param x number of successes (in the treatment group). Note that \code{x}
#' can be a vector.
#' @param n number of patients (in the treatment group)
#' @param xS number of successes in the SOC group (default: 0)
#' @param nS number of patients in the SOC group (default: 0)
#' @param delta margin by which the response rate in the treatment group should
#' be better than in the SOC group (default: 0)
#' @param relativeDelta should the delta be relative? (not default). If this is
#' \code{TRUE}, then a relative delta is used. This means we want to have
#' response at least in delta proportion of the SOC non-responding patients.
#' Non-responding patients rate is 1 - P_S, and we want to have P_S + (1 - P_S)
#' * delta response rate (at least) in the treatment. That is, we evaluate the
#' posterior probability Pr(P_E > P_S + (1 - P_S) * delta | data).
#' @param parE the beta parameters matrix, with K rows and 2 columns,
#' corresponding to the beta parameters of the K components. default is a
#' uniform prior.
#' @param weights the mixture weights of the beta mixture prior. Default are
#' uniform weights across mixture components.
#' @param parS beta parameters for the SOC group (default: uniform)
#' @param weightsS weights for the SOC group (default: uniform)
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
  if (missing(weights)) {
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

        return(cdf * pdf)
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

        return(cdf * pdf)
      }
    }

  ## do the integration. be careful to cover the region where there can
  ## really be any non-zero values. I.e. only integrate over the region where
  ## the beta density of the control is non-zero.
  epsilon <- 1e-13
  bounds <- with(
    controlBetamixPost,
    qbetaMix(
      q = c(epsilon, 1 - epsilon),
      par = par,
      weights = weights
    )
  )
  intRes <- integrate(
    f = integrand,
    lower = bounds[1],
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
