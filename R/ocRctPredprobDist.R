##' @include predprobDist.R
##' @include helpers.R
{}

##' Calculate operating characteristics for RCT against SOC,
##' using the predictive probability method with beta priors
##'
##' The randomization works as follows. According to the randomization ratio and
##' the maximum sample size, patients are allocated to the treatment and SOC
##' arms. The number of patients in the active treatment arm is rounded to the
##' next higher integer. That is, the sequence of patients is determined from
##' the start, such that the number of patients in both arms is constant across
##' trial simulations.
##'
##' Decision rule 1:The trial is stopped for efficacy if the predictive probability of a
##' successful trial is larger than phiU, and stopped for futility if it is below
##' phiL. A trial is successful if after the maximum number of patients the
##' posterior probability of the treatment being at least delta better than the
##' control is above tT. Otherwise the decision is "failure". Hence there is no
##' gray zone.
##'
##' Decision rule 2:A variation can be requested when skipping the argument phiL and utilizing the arguments
##' p1, tFu & PhiFu. The trial can be stopped for futility if the predictive
##' probability of an unsuccessful trial is larger than phiFu. In this case, the
##' decision is "failure" when the posterior probability of having the treatment
##' response rate at most p1 is above tFu at the maximum number of patients.
##'
##' Returned operating characteristics in a matrix include:
##' ExpectedN: expected number of patients in the trials
##' ExpectedNactive: expected number of patients with treatment
##' ExpectedNcontrol: expected number of patients with SOC
##' PrStopEarly: probability to stop the trial early (before reaching the
##' maximum sample size)
##' PrEfficacy: probability to decide for efficacy
##' PrFutility: probability to decide for futility
##' PrGrayZone: probability of no decision at the end ("gray zone")
##'
##' @param nn vector of look locations for efficacy . Note that the maximum sample size is
##' derived as the maximum value (latest look) in this vector.
##' (if futility looks should be different, please specify also \code{nnF})
##' @param pE true rate in the treatment population
##' @param pS true rate in the standard of care population
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
##' @param parE beta parameters for the prior on the control proportion
##' @param parS beta parameters for the prior on the treatment proportion
##' @param randRatio the randomization ratio (active vs. control) to be used.
##' default: 1.
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
##' SampleSizeActive: vector of the patients with treatment in the simulated trials
##' SampleSizeControl: vector of the patients with control in the simulated trials
##' nn: vector of look locations
##' nnE: vector of efficacy look locations
##' nnF: vector of futility look locations
##' todo: would we like to return nnr instead, the actual look locations?
##' params: input parameters of this function
##'
##' @example examples/ocRctPredprobDist.R
##' @export
ocRctPredprobDist <- function(nn, pE, pS, delta = 0, deltaFu = delta, relativeDelta = FALSE,
                              tT, tFu = 1 - tT,
                              phiL = 1 - phiFu, phiU, phiFu = 1 - phiL,
                              parE = c(a = 1, b = 1), parS = c(a = 1, b = 1),
                              randRatio = 1,
                              ns = 10000, nr = F, d = NULL, nnF = nn) {
  ## checks
  stopifnot(
    is.probability(pE),
    is.probability(pS),
    is.probability(delta),
    is.bool(relativeDelta),
    is.probability(tT),
    is.probability(phiL),
    is.probability(phiU),
    randRatio > 0,
    is.scalar(ns),
    is.bool(nr)
  )

  ## s: decision reject H0 (T) or fail to reject (F)
  ##    during trial if continuing (NA)

  if (phiL + phiFu != 1) {
    warning("Both phiL and phiFu arguments are specified, phiL will be overwrite by 1-phiFu")
  }

  nnE <- sort(nn)
  nnF <- sort(nnF)
  s <- rep(NA, ns)
  n <- nActive <- nControl <- s
  ns <- as.integer(ns)
  nn <- sort(unique(c(nnF, nnE)))
  nL <- length(nn)
  Nstart <- nn[1]
  Nmax <- nn[nL]

  ## proportion of active patients:
  activeProp <- randRatio / (randRatio + 1)

  ## determine number of active and control patients
  NmaxActive <- ceiling(activeProp * Nmax)
  NmaxControl <- Nmax - NmaxActive

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

    ## simulate sequence of patients
    isActive <- sample(
      x = rep(c(TRUE, FALSE), c(NmaxActive, NmaxControl)),
      size = Nmax,
      replace = FALSE
    )

    ## simulate sequence of responses
    x <- stats::rbinom(Nmax, 1,
      prob = ifelse(isActive, pE, pS)
    )

    j <- 1
    i <- nnr[j]

    while (is.na(s[k]) & (j <= length(nnr))) {
      ## current data in both arms:
      xActive <- x[which(isActive[1:i])]
      xControl <- x[which(!isActive[1:i])]

      ## compute predictive probability
      if (i %in% nnrF) {
        qL <- 1 - predprobDist(
          x = sum(xActive), n = length(xActive),
          xS = sum(xControl), nS = length(xControl),
          Nmax = NmaxActive, NmaxControl = NmaxControl,
          delta = deltaFu,
          relativeDelta = relativeDelta,
          thetaT = 1 - tFu,
          parE = parE, parS = parS
        )

        s[k] <- ifelse(qL >= phiFu, FALSE, NA)
      }
      if (i %in% nnrE) {
        q <- predprobDist(
          x = sum(xActive), n = length(xActive),
          xS = sum(xControl), nS = length(xControl),
          Nmax = NmaxActive, NmaxControl = NmaxControl,
          delta = delta,
          relativeDelta = relativeDelta,
          thetaT = tT,
          parE = parE, parS = parS
        )

        ## make the decision
        s[k] <- ifelse(q >= phiU & !(i < Nmax & phiU == 1), ## (1)
          TRUE,
          s[k]
        )
      }

      ## what happens if phiU == 1?
      ## then q >= phiU will always be FALSE, except for the last iteration
      ## when i == Nmax -> then it will be 1 or 0.
      ## If it is 1, then the first condition (1) will be TRUE,
      ## so it will be "success".

      ## sample sizes: total and in both arms
      n[k] <- i
      nActive[k] <- length(xActive)
      nControl[k] <- length(xControl)

      j <- j + 1
      i <- nnr[j]
    }
  }
  oc <- cbind(
    ExpectedN = mean(n),
    ExpectedNactive = mean(nActive),
    ExpectedNcontrol = mean(nControl),
    PrStopEarly = mean(n < Nmax),
    PrEarlyEff = sum(s * (n < Nmax), na.rm = T) / ns,
    PrEarlyFut = sum((1 - s) * (n < Nmax), na.rm = T) / ns,
    PrEfficacy = sum(s, na.rm = T) / ns,
    PrFutility = sum(1 - s, na.rm = T) / ns,
    PrGrayZone = sum(is.na(s) / ns)
  )

  return(list(
    oc = oc, Decision = s, SampleSize = n,
    SampleSizeActive = nActive,
    SampleSizeControl = nControl,
    nn = nn, nnE = nnE, nnF = nnF,
    params = as.list(match.call(expand.dots = FALSE))
  ))
}
