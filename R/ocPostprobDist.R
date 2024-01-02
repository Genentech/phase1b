#' @include postprobDist.R
NULL

#' Evaluate Posteriors based on Efficacy and Futility Thresholds in Two-Armed trials
#'
#' A helper function adapted from [h_get_decision()] by
#' replacing the internal use of [postprob()] with [postprobDist()]
#' to generate the `decision` and random looks `all_sizes`.
#'
#' @inheritParams h_get_decision
#' @inheritParams postprobDist
#' @typed deltaE : number
#'  margin by which the response rate in the treatment group should be better
#'  than in the standard of care of `group`. Delta for efficacy is used to
#'  calculate `P(P_E > P_S + deltaE)` which should
#'  exceed threshold `tU` to to stop for efficacy.
#'  Note that this can also be negative, e.g. when non-inferiority is being assessed.
#' @typed deltaF : number
#'  margin by which the response rate in the treatment group should be better
#'  than in the standard of care of `group`. Delta for futility is used to
#'  calculate  `P(P_E > P_S + deltaS)` which should
#'  exceed threshold `tL` to stop for futility.
#'  Note that this can also be negative, e.g. when non-inferiority is being assessed.
#'
#' @keywords internal
#'
h_get_decisionDist <- function(nnr,
                               nnrE,
                               nnrF,
                               truep,
                               parE = c(1, 1),
                               parS = c(1, 1),
                               tL,
                               tU,
                               deltaE,
                               deltaF,
                               relativeDelta) {
  assert_numeric(nnr, finite = TRUE, any.missing = FALSE)
  assert_numeric(nnrE, max.len = length(nnr), any.missing = FALSE)
  assert_numeric(nnrF, max.len = length(nnr), any.missing = FALSE)
  assert_number(truep, lower = 0, upper = 1)
  assert_numeric(parE, lower = 0, finite = TRUE, any.missing = FALSE)
  assert_numeric(parS, lower = 0, finite = TRUE, any.missing = FALSE)
  assert_number(tL, lower = 0, upper = 1)
  assert_number(tU, lower = 0, upper = 1)
  assert_number(deltaE, lower = 0, upper = 1)
  assert_number(deltaF, lower = 0, upper = 1)
  assert_flag(relativeDelta)

  index_look <- 1
  size_look <- nnr[index_look]
  all_sizes <- decision <- NA
  response <- stats::rbinom(max(nnr), size = 1, truep)

  while (is.na(decision) && index_look <= length(nnr)) {
    if (size_look %in% nnrF) {
      qL <- postprobDist(
        x = 0,
        n = 0,
        xS = sum(response[1:size_look]),
        nS = size_look,
        delta = deltaF,
        relativeDelta = relativeDelta,
        parE = parS,
        parS = parE
      )
      decision <- ifelse(qL >= tL, FALSE, NA)
    }
    if (size_look %in% nnrE) {
      qU <- postprobDist(
        x = sum(response[1:size_look]),
        n = size_look,
        xS = 0,
        nS = 0,
        delta = deltaE,
        relativeDelta = relativeDelta,
        parE = parE,
        parS = parS
      )
      decision <- ifelse(qU < tU, decision, TRUE)
    }
    all_sizes <- size_look
    index_look <- index_look + 1
    size_look <- nnr[index_look]
  }
  list(
    decision = decision,
    all_sizes = all_sizes
  )
}

#' Calculate operating characteristics for posterior probability method
#' with beta prior on SOC
#'
#' The trial is stopped for efficacy if the posterior probability to be at
#' least deltaE better than the control is larger than tU, and stopped for
#' futility if the posterior probability to be at least deltaF worse than the
#' control is larger than tL. Otherwise the trial is continued, and at the
#' maximum sample size it may happen that no decision is made ("gray zone").
#'
#' Returned operating characteristics in a matrix include:
#' ExpectedN: expected number of patients in the trials
#' PrStopEarly: probability to stop the trial early (before reaching the
#' maximum sample size)
#' PrEarlyEff: probability to decide for efficacy early
#' PrEarlyFut: probability to decide for futility early
#' PrEfficacy: probability to decide for efficacy
#' PrFutility: probability to decide for futility
#' PrGrayZone: probability of no decision at the end ("gray zone")
#'
#' @param nn vector of look locations for efficacy
#' (if futility looks should be different, please specify also \code{nnF})
#' @param p true rate (scenario)
#' @param deltaE delta for efficacy: P(pE > pS + deltaE) should be large
#' to stop for efficacy
#' @param deltaF delta for futility: P(pE < pS - deltaF) should be large to
#' stop for futility
#' @param relativeDelta see \code{\link{postprobDist}}
#' @param tL probability threshold for being below control - deltaF
#' @param tU probability threshold for being above control + deltaE
#' @param parE beta parameters for the prior on the treatment proportion
#' @param parS beta parameters for the prior on the control proportion
#' @param ns number of simulations
#' @param nr generate random look locations? (not default)
#' @param d distance for random looks around the look locations in \code{nn}
#' @param nnF vector of look locations for futility
#' (default: same as efficacy)
#' @return A list with the following elements:
#' oc: matrix with operating characteristics (see Details section)
#' Decision: vector of the decisions made in the simulated trials
#' (\code{TRUE} for success, \code{FALSE} for failure, \code{NA} for no
#' decision)
#' SampleSize: vector of the sample sizes in the simulated trials
#' nn: vector of look locations
#' nnE: vector of efficacy look locations
#' nnF: vector of futility look locations
#' todo: would we like to return nnr instead, the actual look locations?
#' params: input parameters for this function
#'
#' @example examples/ocPostprobDist.R
#' @export
ocPostprobDist <- function(nn, p, deltaE, deltaF, relativeDelta = FALSE,
                           tL, tU,
                           parE = c(a = 1, b = 1),
                           parS = c(a = 1, b = 1),
                           ns = 10000, nr = FALSE, d = NULL, nnF = nn) {
  nnE <- sort(nn)
  nnF <- sort(nnF)
  s <- rep(NA, ns)
  n <- s
  nn <- sort(unique(c(nnF, nnE)))
  nL <- length(nn)
  Nstart <- nn[1]
  Nmax <- nn[nL]
  if (nr && is.null(d)) {
    ## set parameter d for randomly generating look locations
    d <- floor(min(nn - c(0, nn[-nL])) / 2)
  }
  nnr <- nn
  nnrE <- nnE
  nnrF <- nnF
  ## simulate a clinical trial ns times
  for (k in 1:ns) {
    if (nr && (d > 0)) {
      ## randomly generate look locations
      dd <- sample(-d:d,
        size = nL - 1, replace = TRUE,
        prob = 2^(c(-d:0, rev(-d:(-1))) / 2)
      )
      nnr <- nn + c(dd, 0)
      nnrE <- nnr[nn %in% nnE]
      nnrF <- nnr[nn %in% nnF]
    }
    x <- stats::rbinom(Nmax, 1, p)
    j <- 1
    i <- nnr[j]
    while (is.na(s[k]) && (j <= length(nnr))) {
      if (i %in% nnrF) {
        qL <- postprobDist(
          x = 0, n = 0,
          xS = sum(x[1:i]), nS = i,
          delta = deltaF,
          relativeDelta = relativeDelta,
          parE = parS, parS = parE
        )
        s[k] <- ifelse(qL >= tL, FALSE, NA)
      }
      if (i %in% nnrE) {
        qU <- postprobDist(
          x = sum(x[1:i]), n = i,
          xS = 0, nS = 0,
          delta = deltaE,
          relativeDelta = relativeDelta,
          parE = parE,
          parS = parS
        )
        s[k] <- ifelse(qU < tU, s[k], TRUE)
      }
      n[k] <- i
      j <- j + 1
      i <- nnr[j]
    }
  }
  oc <- cbind(
    ExpectedN = mean(n),
    PrStopEarly = mean(n < Nmax),
    PrEarlyEff = sum(s * (n < Nmax), na.rm = TRUE) / ns,
    PrEarlyFut = sum((1 - s) * (n < Nmax), na.rm = TRUE) / ns,
    PrEfficacy = sum(s, na.rm = TRUE) / ns,
    PrFutility = sum(1 - s, na.rm = TRUE) / ns,
    PrGrayZone = sum(is.na(s) / ns)
  )
  list(
    oc = oc,
    Decision = s,
    SampleSize = n,
    nn = nn,
    nnE = nnE,
    nnF = nnF,
    params = as.list(match.call(expand.dots = FALSE))
  )
}
