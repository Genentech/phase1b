#' Generating random decision and sample size looks for `decision1 == TRUE` or default option
#'
#' A helper function for [ocPredprobDist()] to generate numeric of decisions `decisions` and
#' random looks `all_sizes` for `decision1 == TRUE`.
#'
#' @inheritParams h_get_decision_one_predprob
#' @inheritParams h_predprobdist
#' @typed deltaE : number
#' margin by which the response rate in the treatment group should
#' be better than in the standard of care or control or `S` group in Efficacy looks only.
#' @typed deltaF : number
#' margin by which the response rate in the treatment group should
#' be better than in the standard of care or control or `S` group in Futility looks only.
#' Note that this can also be negative as well.
#'
#' @return A list with the following elements:
#'  - `decision` : decision `flag` with `TRUE` for Go, `FALSE` for Stop, `NA` for Gray zone.
#'  - `all_sizes` : resulting number of look size, anything below maximum
#'   look size is an indicated interim, Futility or Efficacy or both.
#'
#' @keywords internal
h_get_decision1_predprobDist <- function(
    nnE,
    nnF,
    nnr,
    truep,
    xS,
    nS,
    parE = c(1, 1),
    parS = c(1, 1),
    tT,
    phiU,
    phiL,
    deltaE,
    deltaF,
    weights,
    weightsS,
    relativeDelta = FALSE) {
  assert_numeric(nnE, lower = 1, sorted = TRUE, any.missing = FALSE)
  assert_numeric(nnF, lower = 1, sorted = TRUE, any.missing = FALSE)
  assert_numeric(nnr, lower = 1, sorted = TRUE)
  assert_number(truep, lower = 0, upper = 1)
  assert_number(xS, finite = TRUE)
  assert_number(nS, finite = TRUE)
  assert_numeric(parE, min.len = 2, any.missing = FALSE)
  assert_numeric(parS, min.len = 2, any.missing = FALSE)
  assert_number(tT, lower = 0, upper = 1)
  assert_number(phiU, lower = 0, upper = 1)
  assert_number(phiL, lower = 0, upper = 1)
  assert_number(deltaE, lower = 0, upper = 1)
  assert_number(deltaF, lower = 0, upper = 1)
  assert_number(weights, lower = 1, finite = TRUE)
  assert_number(weightsS, lower = 1, finite = TRUE)
  assert_flag(relativeDelta)

  index_look <- 1
  Nmax <- max(nnr)
  NmaxControl <- max(nnF)
  decision <- NA
  response <- stats::rbinom(Nmax, size = 1, prob = truep)

  ### Decision 1:
  # The criteria for Decision 1 for Interim looks are :
  # interim GO =  P(successful trial at final) > phiU
  # interim STOP = P(successful trial at final) < phiL
  while (is.na(decision) && index_look < length(nnr)) {
    size_look <- nnr[index_look]
    if (size_look %in% nnE) {
      interim_qU <- predprobDist(
        x = sum(response[1:size_look]),
        n = size_look,
        xS = xS,
        nS = nS,
        Nmax = Nmax,
        NmaxControl = NmaxControl,
        delta = deltaE,
        relativeDelta = relativeDelta,
        parE = parE,
        weights = weights,
        parS = parS,
        weightsS = weightsS,
        thetaT = tT
      )$result
      decision <- ifelse(interim_qU > phiU, TRUE, decision)
    }
    if (size_look %in% nnF) {
      interim_qU <- predprobDist(
        x = sum(response[1:size_look]),
        n = size_look,
        xS = xS,
        nS = nS,
        Nmax = Nmax,
        NmaxControl = NmaxControl,
        delta = deltaF,
        relativeDelta = relativeDelta,
        parE = parE,
        weights = weights,
        parS = parS,
        weightsS = weightsS,
        thetaT = tT
      )$result
      decision <- ifelse(interim_qU < phiL, FALSE, decision)
    }
    index_look <- index_look + 1
  }
  # The criteria for Decision 1 for Final looks are:
  #- Final GO = P( RR > p0 + delta | data) => tT
  #- Final STOP = P(RR > p0 - delta | data ) < tT
  if (is.na(decision)) {
    size_look <- nnr[index_look]
    if (size_look %in% nnE) {
      final_eff_qU <- postprobDist(
        x = sum(response[1:size_look]),
        n = size_look,
        xS = xS,
        nS = nS,
        delta = deltaE,
        relativeDelta = relativeDelta,
        parE = parE,
        weights = weights, # for activeBetamixPost
        parS = parS,
        weightsS = weightsS # for controlBetamixPost
      )
    }
    decision <- ifelse(final_eff_qU >= tT, TRUE, NA)
  }
  if (size_look %in% nnF) {
    final_fu_qU <- postprobDist(
      x = sum(response[1:size_look]),
      n = size_look,
      xS = xS,
      nS = nS,
      delta = deltaF,
      relativeDelta = relativeDelta,
      parE = parE,
      weights = weights, # for activeBetamixPost
      parS = parS,
      weightsS = weightsS # for controlBetamixPost
    )
    decision <- ifelse(final_fu_qU < tT, FALSE, decision)
  }
  list(
    decision = decision,
    all_sizes = size_look
  )
}

#' Calculate operating characteristics for predictive probability method
#' with beta prior on SOC response rate (gray zone allowed in the final analysis)
#'
#' Decision rule 1: The trial is stopped for efficacy if the predictive probability of a
#' successful trial is larger than phiU, and stopped for futility if it is below
#' phiL. A trial is successful if after the maximum number of patients the
#' posterior probability of the treatment being at least delta better than the
#' control is above tT. Otherwise the decision is "failure". In this case, there is no
#' gray zone.
#'
#' Decision rule 2:A variation can be requested when skipping the argument phiL and utilizing the arguments
#' DeltaFu, tFu & PhiFu. The trial can be stopped for futility if the predictive
#' probability of an unsuccessful trial is larger than phiFu. In this case, the
#' decision is "failure" when the posterior probability of the treatment being
#' at most deltaFu better than the control is above tFu at the maximum number of
#' patients.
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
#' @param nn vector of look locations
#' @param p true rate (scenario)
#' @param delta We want to be better than the control + delta (default: 0)
#' @param deltaFu declare futility when worse than the control + deltaW.
#' Specify only when decision rule 2 is used.
#' @param relativeDelta see \code{\link{postprobDist}}
#' @param tT threshold for the probability to be above control + delta
#' at the end of the trial
#' @param tFu threshold for the probability to be under control + deltaFu
#' at the end of the trial. Specify only when decision rule 2 is used.
#' @param phiL lower threshold on the predictive probability
#' @param phiU upper threshold on the predictive probability
#' @param phiFu threshold on the predictive probability for futility. Specify only
#' when decision rule 2 is used. phiL argument should be skipped in this case.
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
#' (\code{TRUE} for success, \code{FALSE} for failure)
#' SampleSize: vector of the sample sizes in the simulated trials
#' nn: vector of look locations
#' nnE: vector of efficacy look locations
#' nnF: vector of futility look locations
#' params: input parameters of this function
#'
#' @example examples/ocPredprobDist.R
#' @export
ocPredprobDist <- function(nn, p, delta = 0, deltaFu = delta, relativeDelta = FALSE, tT, tFu = 1 - tT,
                           phiL = 1 - phiFu, phiU, phiFu = 1 - phiL,
                           parE = c(a = 1, b = 1), parS = c(a = 1, b = 1),
                           ns = 10000, nr = FALSE, d = NULL, nnF = nn) {
  ## s: decision reject H0 (TRUE) or fail to reject (FALSE)
  ##    during trial if continuing (NA)
  if (phiL + phiFu != 1) {
    warning("Both phiL and phiFu arguments are specified, phiL will be overwrite by 1-phiFu")
  }

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

  for (k in 1:ns) {
    ## simulate a clinical trial ns times
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
        qL <- 1 - predprobDist(
          x = sum(x[1:i]), n = i, Nmax = Nmax, delta = deltaFu,
          relativeDelta = relativeDelta,
          thetaT = 1 - tFu,
          parE = parE, parS = parS
        )$result

        s[k] <- ifelse(qL >= phiFu, FALSE, NA)
      }

      if (i %in% nnrE) {
        qU <- predprobDist(
          x = sum(x[1:i]), n = i, Nmax = Nmax, delta = delta,
          relativeDelta = relativeDelta,
          thetaT = tT,
          parE = parE, parS = parS
        )$result
        s[k] <- ifelse(qU < phiU, s[k], TRUE)
      }


      n[k] <- i
      j <- j + 1
      i <- nnr[j]
    }
  }

  oc <- cbind(
    ExpectedN = mean(n), PrStopEarly = mean(n < Nmax),
    PrEarlyEff = sum(s * (n < Nmax), na.rm = TRUE) / ns,
    PrEarlyFut = sum((1 - s) * (n < Nmax), na.rm = TRUE) / ns,
    PrEfficacy = sum(s, na.rm = TRUE) / ns,
    PrFutility = sum(1 - s, na.rm = TRUE) / ns,
    PrGrayZone = sum(is.na(s) / ns)
  )

  return(list(
    oc = oc, Decision = s, SampleSize = n,
    nn = nn, nnE = nnE, nnF = nnF,
    params = as.list(match.call(expand.dots = FALSE))
  ))
}
