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
#' The max sample size or the sample size of final look.
#'
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
                                   Nmax,
                                   orig_nnr) {
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
  assert_numeric(orig_nnr)

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
      all_looks <- orig_nnr[index_look]
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
      all_looks <- orig_nnr[index_look]
    }
    nActive <- length(xActive)
    nControl <- length(xControl)
    all_sizes <- size_look
    index_look <- index_look + 1
    size_look <- nnr[index_look]
  }
  list(
    decision = decision,
    all_looks = all_looks,
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
  assert_true(Nmax >= max(all_sizes))

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
#' as in [ocPostprobDist()] where the only difference here is to emulate a
#' randomized-controlled trial setting.
#'
#' The returned value is a list with the following elements:
#' - `oc`: matrix with operating characteristics with the following details:
#' - `ExpectedN`: expected number of patients in the trials in both treatment and SOC group
#' - `ExpectedNactive` : the mean of the number of patients in treatment arm
#' - `ExpectedNcontrol`: the mean of the number of patients in control arm
#' - `PrStopEarly`: probability to stop the trial early (before reaching the maximum sample size)
#' - `PrEarlyEff`: probability of Early Go decision
#' - `PrEarlyFut`: probability of Early Stop decision
#' - `PrEfficacy`: probability of Go decision
#' - `PrFutility`: probability of Stop decision
#' - `PrGrayZone`: probability of Evaluate or "Gray Zone" decision (between Go and Stop)
#' - `Decision` : numeric of results with `TRUE` as Go, `FALSE` as Stop and `NA` as Evaluate decision.
#' - `SampleSize` : numeric of sample sizes from `nnE` or `nnF` or both.
#' - `wiggled_nnE` : user input for `nnE` with random distance applied.
#' - `wiggled_nnF` : user input for `nnF` with random distance applied.
#' - `wiggled_dist` : magnitude of random distance applied in order of input looks.
#' - `params` : all user input arguments.
#'
#' @inheritParams h_get_decisionDist
#' @inheritParams h_get_decisionDist_rct
#' @inheritParams ocPostprobDist
#' @typed pE : number
#'  Response rate in Treatment group.
#' @typed pS : number
#'  Response rate in Control group.
#' @example examples/ocRctPostprobDist.R
#' @export
ocRctPostprobDist <- function(nnE,
                              pE,
                              pS,
                              deltaE,
                              deltaF,
                              relativeDelta = FALSE,
                              tL,
                              tU,
                              parE = c(a = 1, b = 1),
                              parS = c(a = 1, b = 1),
                              randRatio = 1,
                              sim,
                              wiggle = FALSE,
                              nnF = nnE) {
  assert_numeric(nnE, min.len = 1, lower = 1, upper = max(nnE), any.missing = FALSE)
  assert_number(pE, lower = 0, upper = 1)
  assert_number(pS, lower = 0, upper = 1)
  assert_number(deltaE, upper = 1, finite = TRUE)
  assert_number(deltaF, upper = 1, finite = TRUE)
  assert_flag(relativeDelta)
  assert_number(tL, lower = 0, upper = 1)
  assert_number(tU, lower = 0, upper = 1)
  assert_numeric(parE, lower = 0, finite = TRUE, any.missing = FALSE)
  assert_numeric(parS, lower = 0, finite = TRUE, any.missing = FALSE)
  assert_number(sim, lower = 1, finite = TRUE)
  assert_flag(wiggle)
  assert_numeric(nnF, min.len = 0, any.missing = FALSE)

  if (sim < 50000) {
    warning("Advise to use sim >= 50000 to achieve convergence")
  }
  decision <- all_sizes <- all_looks <- nActive <- nControl <- vector(length = sim)
  nnE <- sort(nnE)
  nnF <- sort(nnF)
  nn <- sort(unique(c(nnF, nnE)))
  nL <- length(nn)
  nn <- sort(unique(c(nnF, nnE)))

  nL <- length(nn)
  Nstart <- nn[1]
  Nmax <- nn[nL]

  for (k in seq_len(sim)) {
    if (length(nn) != 1 && wiggle) {
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
    nnr <- unique(c(nnrE, nnrF))
    orig_nnr <- unique(sort(c(orig_nnE, orig_nnF)))
    tmp <- h_get_decisionDist_rct(
      nnr = nnr,
      nnrE = nnrE,
      nnrF = nnrF,
      pE = pE,
      pS = pS,
      parE = parE,
      parS = parS,
      tL = tL,
      tU = tU,
      deltaE = deltaE,
      deltaF = deltaF,
      relativeDelta = relativeDelta,
      Nmax = Nmax,
      orig_nnr = orig_nnr
    )
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
    ExpectedN = mean(all_sizes),
    ExpectedNactive = mean(nActive),
    ExpectedNcontrol = mean(nControl),
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
