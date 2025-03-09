#' Generating random decision and sample size looks
#'
#' A helper function for [ocRctPredprobDist()] to generate a single trial decision and sample size.
#'
#' @inheritParams h_get_decisionDist
#' @inheritParams ocPredprobDist
#'
#' @return A list with the following elements :
#'  - `decision` : decision `flag` with `TRUE` for Go, `FALSE` for Stop, `NA` for Gray zone.
#'  - `all_sizes` : resulting sample size.
#'  - `nActive` : number of patients in the active arm.
#'  - `nControl` : number of patients in the control arm.
#' @keywords internal
h_decision_one_RctpredprobDist <- function(
    nnr,
    nnE,
    nnF,
    pE,
    pS,
    parE,
    parS,
    tT,
    phiU,
    phiL,
    deltaE,
    deltaF,
    weights,
    weightsS,
    relativeDelta = FALSE,
    randRatio = 1,
    orig_nnr) {
  assert_numeric(nnE, lower = 1, sorted = TRUE, any.missing = FALSE)
  assert_numeric(nnF, lower = 1, sorted = TRUE, any.missing = FALSE)
  assert_numeric(nnr, lower = 1, sorted = TRUE)
  assert_number(pE, lower = 0, upper = 1)
  assert_number(pS, lower = 0, upper = 1)
  assert_number(randRatio, lower = 1, finite = TRUE)

  Nmax <- max(nnr)
  decision <- NA
  activeProp <- randRatio / (randRatio + 1)
  NmaxActive <- ceiling(activeProp * Nmax)
  NmaxControl <- Nmax - NmaxActive

  isActive <- sample(
    x = rep(c(TRUE, FALSE), c(NmaxActive, NmaxControl)),
    size = Nmax,
    replace = FALSE
  )
  response <- rbinom(Nmax, size = 1, prob = ifelse(isActive, pE, pS))
  index_look <- 1
  size_look <- nnr[index_look]
  ### Decision 1:
  # The criteria for Decision 1 for Interim looks are :
  # Interim GO =  P(successful trial at final) > phiU
  # Interim STOP = P(successful trial at final) < phiL
  while (is.na(decision) && index_look < length(nnr)) {
    xActive <- response[which(isActive[1:size_look])]
    xControl <- response[which(!isActive[1:size_look])]
    if (size_look %in% nnF) {
      interim_qU <- predprobDist(
        x = sum(xActive),
        n = length(xActive),
        xS = sum(xControl),
        nS = length(xControl),
        Nmax = NmaxActive,
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
      all_looks <- orig_nnr[index_look]
    }
    if (size_look %in% nnE) {
      interim_qU <- predprobDist(
        x = sum(xActive),
        n = length(xActive),
        xS = sum(xControl),
        nS = length(xControl),
        Nmax = NmaxActive,
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
      all_looks <- orig_nnr[index_look]
    }
    index_look <- index_look + 1
    size_look <- nnr[index_look]
  }
  # The criteria for Decision 1 for Final looks are:
  # Final GO = P(RR > p0 + deltaE | data) => tT
  # Final STOP = P(RR > p0 + deltaF | data ) < tT
  if (is.na(decision)) {
    size_look <- nnr[index_look]
    xActive <- response[which(isActive[1:size_look])]
    xControl <- response[which(!isActive[1:size_look])]
    if (size_look %in% nnE) {
      final_eff_qU <- postprobDist(
        x = sum(xActive),
        n = length(xActive),
        xS = sum(xControl),
        nS = length(xControl),
        delta = deltaE,
        relativeDelta = relativeDelta,
        parE = parE,
        weights = weights, # for activeBetamixPost
        parS = parS,
        weightsS = weightsS # for controlBetamixPost
      )
      decision <- ifelse(final_eff_qU >= tT, TRUE, decision)
      all_looks <- orig_nnr[index_look]
    }
    if (size_look %in% nnF) {
      final_fu_qU <- postprobDist(
        x = sum(xActive),
        n = length(xActive),
        xS = sum(xControl),
        nS = length(xControl),
        delta = deltaF,
        relativeDelta = relativeDelta,
        parE = parE,
        weights = weights, # for activeBetamixPost
        parS = parS,
        weightsS = weightsS # for controlBetamixPost
      )
      decision <- ifelse(final_fu_qU < tT, FALSE, decision)
      all_looks <- orig_nnr[index_look]
    }
  }
  list(
    nActive = length(xActive),
    nControl = length(xControl),
    decision = decision,
    all_looks = all_looks,
    all_sizes = length(xActive) + length(xControl)
  )
}

#' Generating random decision and sample size looks
#'
#' A helper function for [ocRctPredprobDist()] to generate a single trial decision and sample size.
#'
#' @inheritParams h_decision_one_RctpredprobDist
#' @inheritParams ocPredprobDist
#'
#' @return A list with the following elements :
#'  - `decision` : decision `flag` with `TRUE` for Go, `FALSE` for Stop, `NA` for Gray zone.
#'  - `all_sizes` : resulting sample size.
#'  - `nActive` : number of patients in the active arm.
#'  - `nControl` : number of patients in the control arm.
#' @keywords internal
h_decision_two_RctpredprobDist <- function(
    nnE,
    nnF,
    nnr,
    pE,
    pS,
    parE,
    parS,
    tF,
    tT,
    phiU,
    phiFu,
    deltaE,
    deltaF,
    weights,
    weightsS,
    relativeDelta,
    randRatio,
    orig_nnr) {
  assert_numeric(nnE, lower = 1, sorted = TRUE, any.missing = FALSE)
  assert_numeric(nnF, lower = 1, sorted = TRUE, any.missing = FALSE)
  assert_numeric(nnr, lower = 1, sorted = TRUE)
  assert_number(pE, lower = 0, upper = 1)
  assert_number(pS, lower = 0, upper = 1)
  assert_number(randRatio, lower = 1, finite = TRUE)

  Nmax <- max(nnr)
  NmaxControl <- max(nnF)
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
  index_look <- 1
  size_look <- nnr[index_look]
  # The criteria for Decision 2 for Interim looks are :
  # Interim GO : P ( successful at final) > phiU
  # Interim STOP : P ( unsuccessful at final ) > phiFu
  while (is.na(decision) && index_look < length(nnr)) {
    xActive <- response[which(isActive[1:size_look])]
    xControl <- response[which(!isActive[1:size_look])]
    if (size_look %in% nnE) {
      interim_qU <- predprobDist(
        x = sum(xActive),
        n = length(xActive),
        xS = sum(xControl),
        nS = length(xControl),
        Nmax = NmaxActive,
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
      all_looks <- orig_nnr[index_look]
    }
    if (size_look %in% nnF) {
      interim_qU <- 1 - predprobDist(
        x = sum(xActive),
        n = length(xActive),
        xS = sum(xControl),
        nS = length(xControl),
        Nmax = NmaxActive,
        NmaxControl = NmaxControl,
        delta = deltaF,
        relativeDelta = relativeDelta,
        parE = parE,
        weights = weights,
        parS = parS,
        weightsS = weightsS,
        thetaT = tT
      )$result
      decision <- ifelse(interim_qU > phiFu, FALSE, decision)
      all_looks <- orig_nnr[index_look]
    }
    index_look <- index_look + 1
    size_look <- nnr[index_look]
  }
  # The criteria for Decision 2 for Futility looks are :
  # Final GO = P( RR > p0 + delta ) > tT
  # Final STOP = P( RR < p1 - delta ) > tF
  if (is.na(decision)) {
    size_look <- nnr[index_look]
    xActive <- response[which(isActive[1:size_look])]
    xControl <- response[which(!isActive[1:size_look])]
    if (size_look %in% nnE) {
      final_eff_qU <- postprobDist(
        x = sum(xActive),
        n = length(xActive),
        xS = sum(xControl),
        nS = length(xControl),
        delta = deltaE,
        relativeDelta = relativeDelta,
        parE = parE,
        parS = parS
      )
      decision <- ifelse(final_eff_qU > tT, TRUE, decision)
      all_looks <- orig_nnr[index_look]
    }
    if (size_look %in% nnF) {
      final_fu_qU <- 1 - postprobDist(
        x = sum(xActive),
        n = length(xActive),
        xS = sum(xControl),
        nS = length(xControl),
        delta = deltaF,
        relativeDelta = relativeDelta,
        parE = parE,
        parS = parS
      )
      decision <- ifelse(final_fu_qU > tF, FALSE, decision)
      all_looks <- orig_nnr[index_look]
    }
  }
  list(
    nActive = length(xActive),
    nControl = length(xControl),
    decision = decision,
    all_looks = all_looks,
    all_sizes = length(xActive) + length(xControl)
  )
}

#' Calculate operating characteristics for RCT against SOC,
#' using the predictive probability method with beta priors
#'
#' We emulate a randomized-controlled trial setting where at any given sample size,
#' there exists the number of patients enrolled in either standard of care (SOC) or control arm, and
#' a treatment or experimental arm. The allocation of patients will depend on the
#' randomization ratio set by the user and is rounded to the next higher integer.
#' Therefore the sequence of patients is determined from the start, such that the number of
#' patients in both arms is constant across trial simulations, however the number of patients
#' within the control and treatment arm is determined by the randomisation ratio.
#' Interim looks are for sample sizes below that of the final sample size.
#'
#' Final looks are only performed at the maximum sample size.
#'
#' At each interim or final look, a futility or efficacy or both can be performed.
#'
#' The rules for Stop, Go and Gray Zone (where applicable), and use of beta priors are the same
#' as in [ocPredprobDist()] where the only difference here is to emulate a
#' randomized-controlled trial setting.
#'
#' The returned value is a list with the following elements:
#' - `oc`: matrix with operating characteristics with the following details:
#'  - `ExpectedN`: expected number of patients in the trials in both treatment and SOC group
#'  - `ExpectedNactive` : the mean of the number of patients in treatment arm
#'  - `ExpectedNcontrol`: the mean of the number of patients in control arm
#'  - `PrStopEarly`: probability to stop the trial early (before reaching the maximum sample size)
#'  - `PrEarlyEff`: probability of Early Go decision
#'  - `PrEarlyFut`: probability of Early Stop decision
#'  - `PrEfficacy`: probability of Go decision
#'  - `PrFutility`: probability of Stop decision
#'  - `PrGrayZone`: probability of Evaluate or "Gray Zone" decision (between Go and Stop)
#' - `Decision` : results in `logical` with `TRUE` as Go, `FALSE` as Stop and `NA` as Evaluate decision.
#' - `SampleSize` : numeric of sample sizes from `nnE` or `nnF` or both.
#' - `SampleSizeActive` : numeric of sample sizes in the treatment or experimental arm.
#' - `SampleSizeControl` : numeric of sample sizes in either standard of care (SOC) or control arm.
#' - `union_nn` : unique `nnE` and `nnF` looks.
#' - `wiggled_nnE` : user input for `nnE` with random distance applied.
#' - `wiggled_nnF` : user input for `nnF` with random distance applied.
#' - `wiggled_dist` : magnitude of random distance applied in order of input looks.
#' - `params` : all user input arguments.
#'
#' @inheritParams h_decision_one_predprobDist
#' @inheritParams h_decision_two_predprobDist
#' @inheritParams ocPredprobDist
#' @inheritParams ocRctPostprobDist
#' @example examples/ocRctPredprobDist.R
#' @export
ocRctPredprobDist <- function(nnE,
                              pE,
                              pS,
                              deltaE,
                              deltaF,
                              phiL = 1 - phiFu,
                              phiFu = 1 - phiL,
                              phiU,
                              relativeDelta = FALSE,
                              tT,
                              tF,
                              parE = c(a = 1, b = 1),
                              parS = c(a = 1, b = 1),
                              weights,
                              weightsS,
                              randRatio = 1,
                              sim,
                              wiggle = FALSE,
                              nnF = nnE,
                              decision1 = TRUE) {
  assert_numeric(nnE, min.len = 1, lower = 1, upper = max(nnE), any.missing = FALSE)
  assert_number(sim, lower = 1, finite = TRUE)
  assert_flag(wiggle)
  assert_numeric(nnF, min.len = 0, any.missing = FALSE)
  assert_flag(decision1)

  if (sim < 50000) {
    warning("Advise to use sim >= 50000 to achieve convergence")
  }
  decision <- all_sizes <- all_looks <- nActive <- nControl <- vector(length = sim)
  nnE <- sort(nnE)
  nnF <- sort(nnF)
  nnr <- sort(unique(c(nnF, nnE)))

  Nstart <- nnr[1]
  Nmax <- max(nnr)

  for (k in seq_len(sim)) {
    if (length(nnr) != 1 && wiggle) {
      dist <- h_get_distance(nn = nnr)
      nnr <- h_get_looks(dist = dist, nnE = nnE, nnF = nnF)
      nnrE <- nnr$nnrE
      nnrF <- nnr$nnrF
      orig_nnE <- nnE
      orig_nnF <- nnF
    } else {
      nnrE <- nnE
      nnrF <- nnF
      dist <- NA
      orig_nnE <- nnrE
      orig_nnF <- nnrF
    }
    nnr <- unique(c(nnrE, nnrF))
    orig_nnr <- unique(sort(c(orig_nnE, orig_nnF)))
    tmp <- if (decision1) {
      h_decision_one_RctpredprobDist(
        nnr = nnr,
        nnE = nnE,
        nnF = nnF,
        pE = pE,
        pS = pS,
        parE = parE,
        parS = parS,
        tT = tT,
        phiU = phiU,
        phiL = phiL,
        deltaE = deltaE,
        deltaF = deltaF,
        weights = weights,
        weightsS = weightsS,
        relativeDelta = relativeDelta,
        randRatio = randRatio,
        orig_nnr = orig_nnr
      )
    } else {
      h_decision_two_RctpredprobDist(
        nnr = nnr,
        nnE = nnE,
        nnF = nnF,
        pE = pE,
        pS = pS,
        parE = parE,
        parS = parS,
        tT = tT,
        tF = tF,
        phiU = phiU,
        phiFu = phiFu,
        deltaE = deltaE,
        deltaF = deltaF,
        weights = weights,
        weightsS = weightsS,
        relativeDelta = relativeDelta,
        randRatio = randRatio,
        orig_nnr = orig_nnr
      )
    }
    decision[k] <- tmp$decision
    all_sizes[k] <- tmp$all_sizes
    nActive[k] <- tmp$nActive
    nControl[k] <- tmp$nControl
    all_looks[k] <- tmp$all_looks
  }
  oc <- h_get_oc_rct(
    all_sizes = all_sizes,
    Nmax = Nmax,
    nActive = nActive,
    nControl = nControl,
    decision = decision
  )
  list(
    oc = oc,
    Decision = decision,
    Looks = all_looks,
    SampleSize = all_sizes,
    SampleSizeActive = nActive,
    SampleSizeControl = nControl,
    union_nn = nnr,
    wiggled_nnE = nnrE,
    wiggled_nnF = nnrF,
    wiggle_dist = dist,
    params = as.list(match.call(expand.dots = FALSE))
  )
}
