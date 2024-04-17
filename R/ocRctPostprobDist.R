#' Generating random decision and sample size looks
#'
#' A helper function for [ocRctPostprobDist()] to generate numeric of decisions `decisions` and
#' random looks `all_sizes`.
#'
#' @inheritParams h_get_decision_one_predprob
#' @inheritParams h_get_decision_two_predprob
#'
#' @typed randRatio : numeric
#'  The randomisation ratio between treatment and control. Must be greater than 0 and maximum of 1.
#' @typed Nmax : number
#'  The max sample size or the sample size of final look.
#' @return A list with the following elements :
#'  - `decision` : decision `flag` with `TRUE` for Go, `FALSE` for Stop, `NA` for Gray zone.
#'  - `all_sizes` : resulting number of look size, anything below maximum
#'   look size is an indicated interim, Futility or Efficacy or both.
#'  - `nActive` : mean of look size for Active arm.
#'  - `nControl` : mean of look size for Control arm.
#' @keywords internal
h_get_decisionDist_rct <- function(nnr,
                                   nnrE,
                                   nnrF,
                                   pE,
                                   pS,
                                   parE = c(1, 1),
                                   parS = c(1, 1),
                                   tL,
                                   tU,
                                   deltaE,
                                   deltaF,
                                   relativeDelta,
                                   randRatio = 1,
                                   Nmax) {
  assert_numeric(nnr, finite = TRUE, any.missing = FALSE)
  assert_numeric(nnrE, max.len = length(nnr), any.missing = FALSE)
  assert_numeric(nnrF, max.len = length(nnr), any.missing = FALSE)
  assert_number(pE, lower = 0, upper = 1)
  assert_number(pS, lower = 0, upper = 1)
  assert_numeric(parE, lower = 0, finite = TRUE, any.missing = FALSE)
  assert_numeric(parS, lower = 0, finite = TRUE, any.missing = FALSE)
  assert_number(tL, lower = 0, upper = 1)
  assert_number(tU, lower = 0, upper = 1)
  assert_number(deltaE, finite = TRUE)
  assert_number(deltaF, finite = TRUE)
  assert_flag(relativeDelta)
  assert_number(randRatio, na.ok = FALSE, upper = 1, finite = TRUE)
  assert_number(Nmax, lower = 1)

  index_look <- 1
  size_look <- nnr[index_look]
  all_sizes <- decision <- nActive <- nControl <- NA
  activeProp <- randRatio / (randRatio + 1)
  NmaxActive <- ceiling(activeProp * Nmax)
  NmaxControl <- Nmax - NmaxActive

  isActive <- sample(
    x = rep(c(TRUE, FALSE), c(NmaxActive, NmaxControl)),
    size = Nmax,
    replace = FALSE
  )
  response <- rbinom(Nmax, size = 1, prob = ifelse(isActive, pE, pS))

  while (is.na(decision) && index_look <= length(nnr)) {
    ## current data in both arms:
    xActive <- response[which(isActive[1:size_look])]
    xControl <- response[which(!isActive[1:size_look])]

    if (size_look %in% nnrF) {
      qL <- postprobDist(
        x = sum(xControl),
        n = length(xControl),
        xS = sum(xActive),
        nS = length(xActive),
        delta = deltaF,
        relativeDelta = relativeDelta,
        parE = parS,
        parS = parE
      )
      decision <- ifelse(qL >= tL, FALSE, NA)
    }
    if (size_look %in% nnrE) {
      qU <- postprobDist(
        x = sum(xActive),
        n = length(xActive),
        xS = sum(xControl),
        nS = length(xControl),
        delta = deltaE,
        relativeDelta = relativeDelta,
        parE = parE,
        parS = parS
      )
      decision <- ifelse(qU < tU, decision, TRUE)
    }
    nActive <- length(xActive)
    nControl <- length(xControl)
    all_sizes <- size_look
    index_look <- index_look + 1
    size_look <- nnr[index_look]
  }
  list(
    decision = decision,
    all_sizes = all_sizes,
    nActive = nActive,
    nControl = nControl
  )
}

#' Creating list for operating characteristics
#'
#' Generates operating characteristics for [ocRctPostprobDist()].
#'
#' @inheritParams h_get_decisionDist_rct
#'
#' @return A list of results containing :
#' - `ExpectedN`: expected number of patients in the trials in both treatment and SOC group
#' - `PrStopEarly`: probability to stop the trial early (before reaching the
#' maximum sample size)
#' - `PrEarlyEff`: probability of Early Go decision
#' - `PrEarlyFut`: probability of for Early Stop decision
#' - `PrEfficacy`: probability of Go decision
#' - `PrFutility`: probability of Stop decision
#' - `PrGrayZone`: probability between Go and Stop ,"Evaluate" or Gray decision zone
#' - `ExpectedNactive` : the mean of the number of patients in treatment arm
#' - `ExpectedNcontrol`: the mean of the number of patients in control arm
#'
#' @keywords internal
#'
h_get_oc_rct <- function(all_sizes, Nmax, nActive, nControl, decision) {
  assert_numeric(nActive, any.missing = FALSE, len = length(all_sizes))
  assert_numeric(nControl, any.missing = FALSE, len = length(all_sizes))
  assert_true(all(nActive + nControl == all_sizes))

  tmp <- h_get_oc(
    all_sizes = all_sizes,
    Nmax = Nmax,
    decision = decision
  )
  tmp$ExpectedNactive <- mean(nActive)
  tmp$ExpectedNcontrol <- mean(nControl)
  tmp
}

#' Calculate operating characteristics for RCT against SOC,
#' using the posterior probability method with beta priors
#'
#' The randomization works as follows. According to the randomization ratio and
#' the maximum sample size, patients are allocated to the treatment and SOC
#' arms. The number of patients in the active treatment arm is rounded to the
#' next higher integer. That is, the sequence of patients is determined from
#' the start, such that the number of patients in both arms is constant across
#' trial simulations.
#'
#' The randomized controlled trial (RCT) is stopped for efficacy if the
#' posterior probability to be at least deltaE better than the control is
#' larger than tU, and stopped for futility if the posterior probability to be
#' at least deltaF worse than the control is larger than tL. Otherwise the
#' trial is continued, and at the maximum sample size it may happen that no
#' decision is made ("gray zone").
#'
#' A variation can be requested when \code{deltaF} is set to \code{NULL}. Then
#' the futility decision is made when the probability to be deltaE better than
#' the control is lower than 1 - tL.
#'
#' Returned operating characteristics in a matrix include:
#' ExpectedN: expected total number of patients in the trials
#' ExpectedNactive: expected number of patients with treatment
#' ExpectedNcontrol: expected number of patients with SOC
#' PrStopEarly: probability to stop the trial early (before reaching the
#' maximum sample size)
#' PrEfficacy: probability to decide for efficacy
#' PrFutility: probability to decide for futility
#' PrGrayZone: probability of no decision at the end ("gray zone")
#'
#' @param nn vector of look locations for efficacy. Note that the maximum sample size is
#' derived as the maximum value (latest look) in this vector.
#' (if futility looks should be different, please specify also \code{nnF})
#' @param pE true rate in the treatment population
#' @param pS true rate in the standard of care population
#' @param deltaE delta for efficacy: P(pE > pS + deltaE) should be large
#' to stop for efficacy. Must be non-negative value between 0 and 1.
#' @param deltaF delta for futility: P(pE < pS - deltaF) should be large to
#' stop for futility. Must be numeric value between -1 and 1.
#' Can also be \code{NULL}. (see details)
#' @param relativeDelta see \code{\link{postprobDist}}
#' @param tL probability threshold for being below control - deltaF (default: 0.8)
#' @param tU probability threshold for being above control + deltaE (default: 0.8)
#' @param parE beta parameters for the prior on the control proportion
#' @param parS beta parameters for the prior on the treatment proportion
#' @param randRatio the randomization ratio (active vs. control) to be used.
#' default: 1.
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
#' SampleSizeActive: vector of the patients with treatment in the simulated trials
#' SampleSizeControl: vector of the patients with control in the simulated trials
#' nn: vector of look locations
#' nnE: vector of efficacy look locations
#' nnF: vector of futility look locations
#' todo: would we like to return nnr instead, the actual look locations?
#' params: input parameters for this function
#'
#' @example examples/ocRctPostprobDist.R
#' @export
ocRctPostprobDist <- function(nn, pE, pS, deltaE, deltaF, relativeDelta = FALSE,
                              tL = 0.8, tU = 0.8,
                              parE = c(a = 1, b = 1),
                              parS = c(a = 1, b = 1),
                              randRatio = 1,
                              ns = 10000, nr = FALSE, d = NULL, nnF = nn) {
  ## checks
  stopifnot(
    is.probability(pE),
    is.probability(pS),
    is.probability(deltaE),
    is.bool(relativeDelta),
    is.probability(tL),
    is.probability(tU),
    randRatio > 0,
    is.scalar(ns),
    is.bool(nr)
  )

  if (!is.null(deltaF)) {
    stopifnot(is.probability(abs(deltaF)))
  }


  ## s: decision reject H0 (TRUE) or fail to reject (FALSE)
  ##    during trial if continuing (NA)
  nnE <- sort(nn)
  nnF <- sort(nnF)
  s <- rep(NA, ns)
  n <- nActive <- nControl <- integer(length(s))
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

    ## start trial
    j <- 1
    i <- nnr[j]

    while (is.na(s[k]) && (j <= length(nnr))) {
      ## current data in both arms:
      xActive <- x[which(isActive[1:i])]
      xControl <- x[which(!isActive[1:i])]

      ## compute the two probabilities
      if (!is.null(deltaF) && i %in% nnrF) {
        qL <- postprobDist(
          x = sum(xControl), n = length(xControl),
          xS = sum(xActive), nS = length(xActive),
          delta = deltaF,
          relativeDelta = relativeDelta,
          parE = parS, parS = parE
        )
      }

      qU <- postprobDist(
        x = sum(xActive), n = length(xActive),
        xS = sum(xControl), nS = length(xControl),
        delta = deltaE,
        relativeDelta = relativeDelta,
        parE = parE, parS = parS
      )

      if (!is.null(deltaF)) {
        if (i %in% nnrF) {
          s[k] <- ifelse(qL >= tL, FALSE, NA)
        }

        if (i %in% nnrE) {
          s[k] <- ifelse(qU < tU, s[k], TRUE)
        }
      } else {
        if (i %in% nnrF) {
          s[k] <- ifelse(qU <= 1 - tL, FALSE, NA)
        }

        if (i %in% nnrE) {
          s[k] <- ifelse(qU < tU, s[k], TRUE)
        }
      }

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
    PrEarlyEff = sum(s * (n < Nmax), na.rm = TRUE) / ns,
    PrEarlyFut = sum((1 - s) * (n < Nmax), na.rm = TRUE) / ns,
    PrEfficacy = sum(s, na.rm = TRUE) / ns,
    PrFutility = sum(1 - s, na.rm = TRUE) / ns,
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
