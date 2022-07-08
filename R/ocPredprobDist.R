##' @include predprobDist.R
{}

##' Calculate operating characteristics for predictive probability method
##' with beta prior on SOC response rate (gray zone allowed in the final analysis)
##'
##' Decision rule 1: The trial is stopped for efficacy if the predictive probability of a
##' successful trial is larger than phiU, and stopped for futility if it is below
##' phiL. A trial is successful if after the maximum number of patients the
##' posterior probability of the treatment being at least delta better than the
##' control is above tT. Otherwise the decision is "failure". In this case, there is no
##' gray zone.
##'
##' Decision rule 2:A variation can be requested when skipping the argument phiL and utilizing the arguments
##' DeltaFu, tFu & PhiFu. The trial can be stopped for futility if the predictive
##' probability of an unsuccessful trial is larger than phiFu. In this case, the
##' decision is "failure" when the posterior probability of the treatment being
##' at most deltaFu better than the control is above tFu at the maximum number of
##' patients.
##'
##' Returned operating characteristics in a matrix include:
##' ExpectedN: expected number of patients in the trials
##' PrStopEarly: probability to stop the trial early (before reaching the
##' maximum sample size)
##' PrEarlyEff: probability to decide for efficacy early
##' PrEarlyFut: probability to decide for futility early
##' PrEfficacy: probability to decide for efficacy
##' PrFutility: probability to decide for futility
##' PrGrayZone: probability of no decision at the end ("gray zone")
##'
##' @param nn vector of look locations
##' @param p true rate (scenario)
##' @param delta We want to be better than the control + delta (default: 0)
##' @param deltaFu declare futility when worse than the control + deltaW.
##' Specify only when decisian rule 2 is used.
##' @param relativeDelta see \code{\link{postprobDist}}
##' @param tT threshold for the probability to be above control + delta
##' at the end of the trial
##' @param tFu threshold for the probability to be under control + deltaFu
##' at the end of the trial. Specify only when decisian rule 2 is used.
##' @param phiL lower threshold on the predictive probability
##' @param phiU upper threshold on the predictive probability
##' @param phiFu threshold on the predictive probability for futility. Specify only
##' when decisian rule 2 is used. phiL argument should be skipped in this case.
##' @param parE beta parameters for the prior on the treatment proportion
##' @param parS beta parameters for the prior on the control proportion
##' @param ns number of simulations
##' @param nr generate random look locations? (not default)
##' @param d distance for random looks around the look locations in \code{nn}
##' @param nnF vector of look locations for futility
##' (default: same as efficacy)
##' @return A list with the following elements:
##' oc: matrix with operating characteristics (see Details section)
##' Decision: vector of the decisions made in the simulated trials
##' (\code{TRUE} for success, \code{FALSE} for failure)
##' SampleSize: vector of the sample sizes in the simulated trials
##' nn: vector of look locations
##' nnE: vector of efficacy look locations
##' nnF: vector of futility look locations
##' params: input parameters of this function
##'
##' @example examples/ocPredprobDist.R
##' @export
ocPredprobDist <- function(nn, p, delta = 0, deltaFu = delta, relativeDelta = FALSE, tT, tFu = 1 - tT,
                           phiL = 1 - phiFu, phiU, phiFu = 1 - phiL,
                           parE = c(a = 1, b = 1), parS = c(a = 1, b = 1),
                           ns = 10000, nr = F, d = NULL, nnF = nn) {
  ## s: decision reject H0 (T) or fail to reject (F)
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

  for (k in 1:ns)
  {
    ## simulate a clinical trial ns times
    if (nr && (d > 0)) {
      ## randomly generate look locations
      dd <- sample(-d:d,
        size = nL - 1, replace = T,
        prob = 2^(c(-d:0, rev(-d:(-1))) / 2)
      )
      nnr <- nn + c(dd, 0)


      nnrE <- nnr[nn %in% nnE]
      nnrF <- nnr[nn %in% nnF]
    }
    x <- stats::rbinom(Nmax, 1, p)
    j <- 1
    i <- nnr[j]
    while (is.na(s[k]) & (j <= length(nnr))) {
      if (i %in% nnrF) {
        ## this should also work for the relative delta
        ## PP(P(P_E < P_S + (1 - P_S) * deltaFu | data)>tFu)
        ##    =PP(P(P_E > P_S + (1 - P_S) * deltaFu | data)<1-tFu)
        ##    =1-PP(P(P_E > P_S + (1 - P_S) * deltaFu | data)>1-tFu)
        qL <- 1 - predprobDist(
          x = sum(x[1:i]), n = i, Nmax = Nmax, delta = deltaFu,
          relativeDelta = relativeDelta,
          thetaT = 1 - tFu,
          parE = parE, parS = parS
        )

        s[k] <- ifelse(qL >= phiFu, FALSE, NA)
      }

      if (i %in% nnrE) {
        qU <- predprobDist(
          x = sum(x[1:i]), n = i, Nmax = Nmax, delta = delta,
          relativeDelta = relativeDelta,
          thetaT = tT,
          parE = parE, parS = parS
        )
        s[k] <- ifelse(qU < phiU, s[k], TRUE)
      }


      n[k] <- i
      j <- j + 1
      i <- nnr[j]
    }
  }

  oc <- cbind(
    ExpectedN = mean(n), PrStopEarly = mean(n < Nmax),
    PrEarlyEff = sum(s * (n < Nmax), na.rm = T) / ns,
    PrEarlyFut = sum((1 - s) * (n < Nmax), na.rm = T) / ns,
    PrEfficacy = sum(s, na.rm = T) / ns,
    PrFutility = sum(1 - s, na.rm = T) / ns,
    PrGrayZone = sum(is.na(s) / ns)
  )

  return(list(
    oc = oc, Decision = s, SampleSize = n,
    nn = nn, nnE = nnE, nnF = nnF,
    params = as.list(match.call(expand.dots = FALSE))
  ))
}
