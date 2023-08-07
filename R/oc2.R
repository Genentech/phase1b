##' @include postprob.R
##' @include postprobDist.R
##' @include predprob.R
##' @include predprobDist.R
NULL

##' Run simulations to obtain operating characteristics for methods that include
##' historical data on control
##'
##' Interim looks are done three times total including the last one, with
##' approximately equal distance between the looks.
##'
##' @param method 8 different methods can be selected:
##' PointMass.Bayes: single arm trial with pointmass derived from the historical
##' data on the control response rate, see \code{\link{postprob}}
##' Prior.Bayes: single arm trial with prior derived from the historical data on
##' the control response rate, see \code{\link{postprobDist}}
##' RCT.Bayes: RCT (1:1 randomization) with prior derived from the historical data on
##' the control response rate, see \code{\link{postprobDist}}
##' RCT.BayesRobust: Same as RCT.Bayes, but with a robust prior, enabling
##' dynamic borrowing of the historical information
##' RCT.vanillaBayes: RCT, not using the historical control data
##' PointMass.PP: single arm trial with pointmass derived from the historical
##' data on the control response rate, and using the predictive probability
##' method, see \code{\link{predprob}}
##' Prior.PP: single arm trial with prior derived from the historical data on
##' the control response rate, and using the predictive probability
##' method, see \code{\link{predprobDist}}
##' RCT.PP: RCT (1:1 randomization) with prior derived from the historical data on
##' the control response rate, and using the predictive probability
##' method, see \code{\link{predprobDist}}
##' RCT.PProbust: Same as RCT.PP,  but with a robust prior, enabling
##' dynamic borrowing of the historical information
##' RCT.vanillaPP: RCT and using the predictive probability
##' method, not using the historical control data
##' @param nSim number of trials to simulate
##' @param histSize historical data size
##' @param trialSize total trial sample size
##' @param drift drift parameter: the actual control response rate is the
##' historical control response rate plus the drift. Hence a positive drift
##' means that the controls now have higher response rates than in the
##' historical trial.
##' @param controlRate the actual control response rate
##' @param nmeRate the treatment response rate
##' @param delta delta for stopping for efficacy to be used. Implicitly a zero
##' delta for stopping for futility is used.
##' @param tL for Bayes methods: probability threshold for being below control
##' response rate (default: 0.8)
##' @param tU for Bayes methods: probability threshold for being above control
##' response rate + delta (default: 0.8)
##' @param tT threshold for the probability to be above the response rate p0 at
##' the end of the trial (default: 0.85)
##' @param phiL lower threshold on the predictive probability (default: 0.05)
##' @param phiU upper threshold on the predictive probability (default: 0.95)
##' @param parE the beta parameters matrix, with K rows and 2 columns,
##' corresponding to the beta parameters of the K components. Default is a
##' uniform prior.
##' @param weights the mixture weights of the beta mixture prior. Default are
##' uniform weights across mixture components.
##' @return Returned operating characteristics in a matrix include:
##' ExpectedN: expected number of patients in the trials
##' ExpectedNactive: expected number of patients with treatment
##' PrStopEarly: probability to stop the trial early (before reaching the
##' maximum sample size)
##' PrEarlyEff: probability to decide for efficacy early
##' PrEarlyFut: probability to decide for futility early
##' PrEfficacy: probability to decide for efficacy
##' PrFutility: probability to decide for futility
##' PrGrayZone: probability of no decision at the end ("gray zone")
##'
##' @importFrom stats rbinom
##'
##' @example examples/oc2.R
##' @export
oc2 <- function(method =
                  c(
                    "PointMass.Bayes", "Prior.Bayes",
                    "RCT.Bayes", "RCT.BayesRobust", "RCTvanilla.Bayes",
                    "PointMass.PP", "Prior.PP",
                    "RCT.PP", "RCT.PProbust", "RCTvanilla.PP"
                  ),
                nSim,
                histSize,
                trialSize,
                drift = 0,
                controlRate,
                nmeRate,
                delta,
                ## for Bayes methods:
                tL = 0.8,
                tU = 0.8,
                ## for PP methods:
                tT = 0.85,
                phiL = 0.05,
                phiU = 0.95,
                ## prior on NME response rate:
                parE = c(1, 1),
                weights) {
  method <- match.arg(method)
  bayesNames <- c(
    "PointMass.Bayes", "Prior.Bayes", "RCT.Bayes", "RCT.BayesRobust",
    "RCTvanilla.Bayes"
  )
  ppNames <- c(
    "PointMass.PP", "Prior.PP", "RCT.PP", "RCT.PProbust",
    "RCTvanilla.PP"
  )

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
    ## (don't need to be normalized, this is done in getBetamixPost)
  }

  ## allocation to active and control arms:
  activeProp <-
    if (method %in% c(
      "RCT.Bayes",
      "RCT.BayesRobust",
      "RCTvanilla.Bayes",
      "RCT.PP",
      "RCT.PProbust",
      "RCTvanilla.PP"
    )) {
      ## randomize 1:1
      1 / 2
    } else {
      ## only active here
      1
    }

  ## deltas for prior methods:
  deltaE <- delta
  deltaF <- 0

  ## drift is accounted for:
  histRate <- controlRate - drift

  ## s: decision reject H0 (T) or fail to reject (F)
  ##    during trial if continuing (NA)
  s <- rep(NA, nSim)
  n <- nActive <- nControl <- integer(length(s))

  ## looks: three times total including the last one,
  ## approx equal distance.
  nn <- ceiling(seq(from = 1, to = trialSize, length = 3 + 1))[-1]
  nn <- sort(nn)
  nr <- FALSE
  nL <- length(nn)
  Nstart <- nn[1]
  Nmax <- nn[nL]

  NmaxActive <- ceiling(activeProp * Nmax)
  NmaxControl <- Nmax - NmaxActive

  if (nr && is.null(d)) {
    ## set parameter d for randomly generating look locations
    d <- floor(min(nn - c(0, nn[-nL])) / 2)
  }
  nnr <- nn

  for (k in 1:nSim) { ## simulate a clinical trial nSim times
    if (nr && (d > 0)) {
      ## randomly generate look locations
      dd <- sample(-d:d,
        size = nL - 1, replace = TRUE,
        prob = 2^(c(-d:0, rev(-d:(-1))) / 2)
      )
      nnr <- nn + c(dd, 0)
    }

    ## simulate sequence of patients
    isActive <- sample(
      x =
        rep(
          c(TRUE, FALSE),
          c(NmaxActive, NmaxControl)
        ),
      size = Nmax,
      replace = FALSE
    )

    ## generate the control data (binomial distribution)
    xHist <- stats::rbinom(n = 1L, size = histSize, prob = histRate)

    ## this generates p0:
    p0 <- xHist / histSize

    ## and with the delta this gives p1:
    p1 <- min(p0 + delta, 1)

    ## for the prior methods, these are the
    ## beta parameters:
    parS <-
      if (method %in% c("RCTvanilla.Bayes", "RCTvanilla.PP")) {
        ## change parS to uniform in RCT vanilla case
        c(1, 1)
      } else if (method %in% c("RCT.BayesRobust", "RCT.PProbust")) {
        ## specify a robust prior using the historical data
        ## beta mixture (50% weight each) of the informative
        ## and uniform distribution:
        matrix(
          data =
            c(
              1 + xHist, 1 + histSize - xHist,
              1, 1
            ),
          nrow = 2,
          byrow = TRUE
        )
        ## note that, because the default is a uniform distribution
        ## across the mixture components, we do not need to manually
        ## specify the weights.
      } else {
        ## otherwise we use the historical data without robustification
        c(1 + xHist, 1 + histSize - xHist)
      }

    ## generate the trial data (Bernoulli distribution)
    ## simulate sequence of responses
    x <- stats::rbinom(Nmax, 1,
      prob = ifelse(isActive, nmeRate, controlRate)
    )

    ## start trial
    j <- 1
    i <- nnr[j]

    while (is.na(s[k]) && (j <= length(nnr))) {
      if (method == "PointMass.Bayes") {
        ## PointMass.Bayes
        qL <- 1 - postprob(x = sum(x[1:i]), n = i, p = p0, parE = parE, weights = weights)
        qU <- postprob(x = sum(x[1:i]), n = i, p = p1, parE = parE, weights = weights)
      } else if (method %in% c(
        "Prior.Bayes",
        "RCT.Bayes",
        "RCT.BayesRobust",
        "RCTvanilla.Bayes"
      )) {
        ## current data in both arms:
        xActive <- x[which(isActive[1:i])]
        xControl <- x[which(!isActive[1:i])]

        ## compute the two probabilities
        qL <- postprobDist(
          x = sum(xControl), n = length(xControl),
          xS = sum(xActive), nS = length(xActive),
          delta = deltaF,
          parE = parS,
          parS = parE, weightsS = weights
        )
        qU <- postprobDist(
          x = sum(xActive), n = length(xActive),
          xS = sum(xControl), nS = length(xControl),
          delta = deltaE,
          parE = parE, weights = weights,
          parS = parS
        )

        ## sample sizes: in both arms
        nActive[k] <- length(xActive)
        nControl[k] <- length(xControl)
      } else if (method == "PointMass.PP") {
        ## PointMass.PP
        q <- predprob(
          x = sum(x[1:i]),
          n = i, Nmax = Nmax, p = p1,
          thetaT = tT, parE = parE, weights = weights
        )
      } else if (method %in% c(
        "Prior.PP",
        "RCT.PP",
        "RCT.PProbust",
        "RCTvanilla.PP"
      )) {
        ## current data in both arms:
        xActive <- x[which(isActive[1:i])]
        xControl <- x[which(!isActive[1:i])]

        ## compute predictive probability
        q <- predprobDist(
          x = sum(xActive), n = length(xActive),
          xS = sum(xControl), nS = length(xControl),
          Nmax = NmaxActive, NmaxControl = NmaxControl,
          delta = delta,
          thetaT = tT,
          parE = parE,
          weights = weights,
          parS = parS
        )

        ## sample sizes: in both arms
        nActive[k] <- length(xActive)
        nControl[k] <- length(xControl)
      }

      ## decision depends on whether Bayes or PP is used:
      s[k] <- if (method %in% bayesNames) {
        ifelse(qU >= tU, TRUE, ifelse(qL >= tL, FALSE, NA))
      } else {
        ifelse(q >= phiU & !(i < Nmax & phiU == 1),
          TRUE,
          ifelse(q <= phiL, FALSE, NA)
        )
      }

      ## advance iterators
      n[k] <- i
      j <- j + 1
      i <- nnr[j]
    }
  }

  oc <- cbind(
    ExpectedN = mean(n),
    ExpectedNactive = mean(nActive),
    ExpectedNcontrol = mean(nControl),
    PrStopEarly = mean(n < Nmax),
    PrEarlyEff = sum(s * (n < Nmax), na.rm = TRUE) / nSim,
    PrEarlyFut = sum((1 - s) * (n < Nmax), na.rm = TRUE) / nSim,
    PrEfficacy = sum(s, na.rm = TRUE) / nSim,
    PrFutility = sum(1 - s, na.rm = TRUE) / nSim,
    PrGrayZone = sum(is.na(s) / nSim)
  )

  ## return OC
  return(oc)
}
