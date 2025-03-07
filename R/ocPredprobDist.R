#' Generating random decision and sample size looks for `decision1 == TRUE` or default option
#'
#' A helper function for [ocPredprobDist()] to generate numeric of decisions `decisions` and
#' random looks `all_sizes` for `decision1 == TRUE`.
#'
#' @inheritParams h_get_decision_one_predprob
#' @inheritParams h_predprobdist
#' @inheritParams h_get_decisionDist
#' @return A list with the following elements :
#'  - `decision` : decision `flag` with `TRUE` for Go, `FALSE` for Stop, `NA` for Gray zone.
#'  - `all_sizes` : resulting number of look size, anything below maximum
#'   look size is an indicated interim, Futility or Efficacy or both.
#'
#' @keywords internal
h_decision_one_predprobDist <- function(
    nnE,
    nnF,
    nnr,
    truep,
    xS,
    nS,
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
    orig_nnr) {
  assert_numeric(nnE, lower = 1, sorted = TRUE, any.missing = FALSE)
  assert_numeric(nnF, lower = 1, sorted = TRUE, any.missing = FALSE)
  assert_numeric(nnr, lower = 1, sorted = TRUE)
  assert_number(truep, lower = 0, upper = 1)

  index_look <- 1
  Nmax <- max(nnr)
  NmaxControl <- max(nnF)
  decision <- NA
  response <- stats::rbinom(Nmax, size = 1, prob = truep)

  ### Decision 1:
  # The criteria for Decision 1 for Interim looks are :
  # Interim GO =  P(successful trial at final) > phiU
  # Interim STOP = P(successful trial at final) < phiL
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
      all_looks <- orig_nnr[index_look]
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
      all_looks <- orig_nnr[index_look]
    }
    index_look <- index_look + 1
  }
  # The criteria for Decision 1 for Final looks are:
  # Final GO = P(RR > p0 + deltaE | data) => tT
  # Final STOP = P(RR > p0 + deltaF | data ) < tT
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
      decision <- ifelse(final_eff_qU >= tT, TRUE, NA)
      all_looks <- orig_nnr[index_look]
    }
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
    all_looks <- orig_nnr[index_look]
  }
  list(
    decision = decision,
    all_sizes = size_look,
    all_looks = all_looks
  )
}

#' Generating random decision and sample size looks for `decision1 == FALSE` or default option
#'
#' A helper function for [ocPredprobDist()] to generate numeric of decisions `decisions` and
#' random looks `all_sizes` for `decision1 == TRUE`.
#'
#' @inheritParams h_get_decision_two_predprob
#' @inheritParams h_predprobdist
#' @inheritParams h_decision_one_predprobDist
#'
#' @return A list with the following elements:
#'  - `decision` : decision `flag` with `TRUE` for Go, `FALSE` for Stop, `NA` for Gray zone.
#'  - `all_sizes` : resulting number of look size, anything below maximum
#'   look size is an indicated interim, Futility or Efficacy or both.
#'
#' @keywords internal
h_decision_two_predprobDist <- function(
    nnE,
    nnF,
    nnr,
    truep,
    xS,
    nS,
    parE,
    parS,
    tT,
    tF,
    phiU,
    phiFu,
    deltaE,
    deltaF,
    weights,
    weightsS,
    relativeDelta,
    orig_nnr) {
  assert_numeric(nnE, lower = 1, sorted = TRUE, any.missing = FALSE, )
  assert_numeric(nnF, lower = 1, sorted = TRUE, any.missing = FALSE, )
  assert_numeric(nnr, lower = 1, sorted = TRUE, any.missing = FALSE, )
  assert_number(truep, lower = 0, upper = 1)

  index_look <- 1
  Nmax <- max(nnr)
  NmaxControl <- max(nnF)
  decision <- NA
  response <- stats::rbinom(Nmax, size = 1, prob = truep)
  # The criteria for Decision 2 for Interim looks are :
  # Interim GO : P ( successful at final) > phiU
  # Interim STOP : P ( unsuccessful at final ) > phiFu
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
      decision <- ifelse(interim_qU > phiU, FALSE, decision)
      all_looks <- orig_nnr[index_look]
    }
    if (size_look %in% nnF) {
      interim_qU <- 1 - predprobDist(
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
      decision <- ifelse(interim_qU > phiFu, FALSE, decision)
      all_looks <- orig_nnr[index_look]
    }
    index_look <- index_look + 1
  }
  # The criteria for Decision 2 for Futility looks are :
  # Final GO = P( RR > p0 + delta ) > tT
  # Final STOP = P( RR < p1 - delta ) > tF
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
        parS = parS
      )
    }
    decision <- ifelse(final_eff_qU > tT, TRUE, NA)
    all_looks <- orig_nnr[index_look]
  }
  if (size_look %in% nnF) {
    final_fu_qU <- 1 - postprobDist(
      x = sum(response[1:size_look]),
      n = size_look,
      xS = xS,
      nS = nS,
      delta = deltaF,
      relativeDelta = relativeDelta,
      parE = parE,
      parS = parS
    )
    decision <- ifelse(final_fu_qU > tF, FALSE, decision)
    all_looks <- orig_nnr[index_look]
  }
  list(
    decision = decision,
    all_sizes = size_look,
    all_looks = all_looks
  )
}

#' Calculate operating characteristics for predictive probability method
#' with beta prior on SOC response rate (gray zone allowed in the final analysis for Decision 2)
#'
#' For formulas and expressions for predictive probability, refer to [predprobDist()].
#'
#' The rules for Stop, Go and Gray Zone (where applicable) are the same as in [ocPredprob()].
#' However when compared to [ocPredprob()], where the predictive probability of `response rate > p0`
#' was used to reach decisions, here the `response rate - p0 > 0` is replaced by `response rate - p0 > delta`,
#' and the reason is that `p0` is now also uncertain, which is quantified by a posterior distribution.
#' To therefore understand the margin of difference between the experimental and control group, refer
#' to the two ways of calculating delta in [postprobDist()].
#'
#' The margin `delta` from [postprobDist()] is no longer used however where a Go decision is evaluated,
#' the margin `deltaE` is instead employed, and where a Stop decision is evaluated,
#' the margin `deltaF` is instead employed in lieu of `delta`.
#' Decision 1 and Decision 2 share the same Go rule for interim and final,
#' with the margin of difference is accounted for in the following manner for the final looks:
#' - `Pr(P_E > p0 + deltaE | data) > tT ` for the absolute case
#' - `Pr(P_E > (1-p0)*deltaE | data) > tT` for the relative case
#'
#' As mentioned, the rules of [ocPredprobDist()] and [ocPredprob()] for both Decision 1 and Decision 2 are the same,
#' and the margin of difference differ depending on whether it is a Go or Stop decision
#' (`deltaE` and `deltaF` is employed instead of `delta`).
#' We highlight here another distinction between [ocPredprob()] and [ocPredprobDist()],
#' seen in the evaluation for the final futility look in [ocPredprobDist()] where `p1` is used instead of `p0` :
#' - `Pr(P_E < p1 + deltaF | data) > tF` for the absolute case
#' - `Pr(P_E < (1-p1)*deltaF | data) > tF` for the relative case
#'
#' @return A list with the following elements:
#' - `oc`: matrix with operating characteristics with the following details:
#'    - `ExpectedN`: expected number of patients in the trials
#'    - `PrStopEarly`: probability to stop the trial early (before reaching the
#'                    maximum sample size)
#'    - `PrEarlyEff`: probability of Early Go decision
#'    - `PrEarlyFut`: probability of for Early Stop decision
#'    - `PrEfficacy`: probability of Go decision
#'    - `PrFutility`: probability of Stop decision
#'    - `PrGrayZone`: probability between Go and Stop ,"Evaluate" or Gray decision zone
#' - `Decision` : numeric of results with `TRUE` as Go decision, `FALSE` as Stop and `NA` as gray zone.
#' - `SampleSize` : numeric of sample sizes from `nnE` or `nnF` or both.
#' - `wiggled_nnE` : user input for `nnE` with random distance applied.
#' - `wiggled_nnF` : user input for `nnF` with random distance applied.
#' - `wiggled_dist` : magnitude of random distance applied in order of input looks.
#' - `params` : all user input arguments.
#'
#' @example examples/ocPredprobDist.R
#' @export
#' @inheritParams h_get_looks
#' @inheritParams h_decision_one_predprobDist
#' @inheritParams ocPredprob
ocPredprobDist <- function(
    nnE,
    truep,
    deltaE,
    deltaF,
    phiU,
    xS = 0,
    nS = 0,
    relativeDelta = FALSE,
    tT = 1 - tF,
    tF = 1 - tT,
    phiL = 1 - phiFu,
    phiFu = 1 - phiL,
    parE = c(1, 1),
    parS = c(1, 1),
    weights = weights,
    weightsS = weightsS,
    sim = 50000,
    wiggle = FALSE,
    nnF = nnE,
    decision1 = TRUE) {
  Nmax <- max(unique(nnE, nnF))

  assert_numeric(nnE, any.missing = FALSE, sorted = TRUE)
  assert_number(sim, lower = 1, finite = TRUE)
  assert_flag(wiggle)
  assert_numeric(nnF, lower = 1, any.missing = FALSE, sorted = TRUE)
  assert_flag(decision1)

  nn <- sort(unique(c(nnF, nnE)))
  if (sim < 50000) {
    warning("Advise to use sim >= 50000 to achieve convergence")
  }
  decision <- vector(length = sim)
  all_sizes <- vector(length = sim)
  all_looks <- vector(length = sim)
  for (k in seq_len(sim)) {
    if (length(nn) != 1 && wiggle) {
      # if we have more than one look in nnF and nnE, we don't wiggle
      dist <- h_get_distance(nn = nn)
      nnr <- h_get_looks(dist = dist, nnE = nnE, nnF = nnF)
      nnrE <- nnr$nnrE
      nnrF <- nnr$nnrF
      orig_nnE <- nnE
      orig_nnF <- nnF
    } else {
      dist <- 0
      nnrE <- nnE
      nnrF <- nnF
      orig_nnE <- nnrE
      orig_nnF <- nnrF
    }
    nnr <- unique(sort(c(nnrE, nnrF)))
    orig_nnr <- unique(sort(c(orig_nnE, orig_nnF)))
    tmp <- if (decision1) {
      h_decision_one_predprobDist(
        nnE = nnrE,
        nnF = nnrF,
        nnr = nnr,
        truep = truep,
        xS = xS,
        nS = nS,
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
        orig_nnr = orig_nnr
      )
    } else {
      h_decision_two_predprobDist(
        nnE = nnrE,
        nnF = nnrF,
        nnr = nnr,
        truep = truep,
        xS = xS,
        nS = nS,
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
        orig_nnr = orig_nnr
      )
    }
    decision[k] <- tmp$decision
    all_sizes[k] <- tmp$all_sizes
    all_looks[k] <- tmp$all_looks
  }
  oc <- h_get_oc(all_sizes = all_sizes, Nmax = Nmax, decision = decision)
  list(
    oc = oc,
    Decision = decision,
    Looks = all_looks,
    SampleSize = all_sizes,
    wiggled_nnrE = nnrE,
    wiggled_nnrF = nnrF,
    dist = dist,
    params = as.list(match.call(expand.dots = FALSE))
  )
}
