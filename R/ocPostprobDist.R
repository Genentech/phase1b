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
#' @inheritParams h_get_oc
#' @typed deltaE : number
#' margin by which the response rate in the treatment group should
#' be better than in the standard of care or control or `S` group in Efficacy looks only.
#' Note that this can also be negative as well.
#' @typed deltaF : number
#' margin by which the response rate in the treatment group should
#' be better than in the standard of care or control or `S` group in Futility looks only.
#' Note that this can also be negative as well.
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
                               relativeDelta,
                               orig_nnr) {
  assert_numeric(nnr, finite = TRUE, any.missing = FALSE)
  assert_numeric(nnrE, max.len = length(nnr), any.missing = FALSE)
  assert_numeric(nnrF, max.len = length(nnr), any.missing = FALSE)
  assert_number(truep, lower = 0, upper = 1)
  assert_numeric(parE, lower = 0, finite = TRUE, any.missing = FALSE)
  assert_numeric(parS, lower = 0, finite = TRUE, any.missing = FALSE)
  assert_number(tL, lower = 0, upper = 1)
  assert_number(tU, lower = 0, upper = 1)
  assert_number(deltaE, finite = TRUE)
  assert_number(deltaF, finite = TRUE)
  assert_flag(relativeDelta)
  assert_numeric(orig_nnr)

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
      all_looks <- orig_nnr[index_look]
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
      all_looks <- orig_nnr[index_look]
    }
    all_sizes <- size_look
    index_look <- index_look + 1
    size_look <- nnr[index_look]
  }
  list(
    decision = decision,
    all_sizes = all_sizes,
    all_looks = all_looks
  )
}

#' Calculate Operating characteristics of Posterior Probability method
#' with Beta Prior on the Control or Standard of Care Arm
#'
#' `r lifecycle::badge("experimental")`
#'
#' The trial is stopped for Efficacy if the posterior probability of assumed
#' true response rate or `truep` is better than the control arm by at least
#' `deltaE`, is larger than `tU`. Analogously, The trial is stopped for Efficacy
#' if the posterior probability of assumed true response rate or `truep` is
#' better than the control arm by at least `deltaF`, is larger than `tL`
#' for Futility looks. If these criteria are not met, the trial is continued
#' and at the maximum sample size, it may reach no decision or "gray zone".The
#' following represents the stopping criteria for Efficacy and Futility
#' respectively.
#'
#' Stop criteria for Efficacy :
#'
#' `Pr(RR > P_S + deltaE) > tU`
#'
#' Stop criteria for Futility :
#'
#' `Pr(RR < P_S + deltaF) > tL`
#'
#' Where `truep` is the assumed true rate of response and `p1` and `p0` respectively are
#' the thresholds for Efficacy and Futility respectively.
#'
#' Resulting operating characteristics include the following:
#'
#' - `ExpectedN`: expected number of patients in the trials
#' - `PrStopEarly`: probability to stop the trial early (before reaching the
#' maximum sample size)
#' - `PrEarlyEff`: probability of Early Go decision
#' - `PrEarlyFut`: probability of for Early Stop decision
#' - `PrEfficacy`: probability of Go decision
#' - `PrFutility`: probability of Stop decision
#' - `PrGrayZone`: probability between Go and Stop ,"Evaluate" or Gray decision zone
#'
#' @inheritParams ocPostprob
#' @inheritParams postprobDist
#' @inheritParams h_get_decisionDist
#' @inheritParams h_get_distance
#' @typed deltaE : number
#'  margin of which the response rate in the treatment group should be better
#'  than in the standard of care of `group`. Delta for Efficacy is used to
#'  calculate `P(truep > P_S + deltaE)` which should
#'  exceed threshold `tU` to to stop for Efficacy. `DeltaE` can also be negative in
#'  non-inferiority setting. See also note.
#' @typed deltaF : number
#'  margin of which the response rate in the treatment group should be better
#'  than in the standard of care of `group`. Delta for Futility is used to
#'  calculate  `P(truep > P_S - deltaF)` which should
#'  exceed threshold `tL` to stop for Futility.`DeltaF` can also be negative in
#'  non-inferiority setting. See also note.
#'
#' @note
#'
#' ## Delta :
#'
#' The desired improvement is denoted as `deltaE` or `deltaF` and two options
#' exist in its use: The absolute case when `relativeDelta = FALSE` and
#' relative as when `relativeDelta = TRUE`.
#'
#' 1. The absolute case is when we define an absolute delta, greater than `P_S`,
#' the response rate of the standard of care or control or `S` group such that
#' the posterior is `Pr(truep > P_S + deltaE | data)` for Efficacy looks
#' or `Pr(truep > P_S + deltaF | data)` for Futility looks.
#'
#' 2. In the relative case, we suppose that the treatment group's
#' response rate is assumed to be greater than `P_S + (1-P_S) * delta` such that
#' the posterior is `Pr(truep > P_S + (1 - P_S) * deltaE | data)` for Efficacy looks
#' or `Pr(truep > P_S + (1 - P_S) * deltaF | data)` for Futility looks.
#'
#' @example examples/ocPostprobDist.R
#' @export
ocPostprobDist <- function(nnE,
                           truep,
                           deltaE,
                           deltaF,
                           relativeDelta = FALSE,
                           tL,
                           tU,
                           parE = c(a = 1, b = 1),
                           parS = c(a = 1, b = 1),
                           wiggle = TRUE,
                           sim = 50000,
                           nnF = nnE) {
  assert_numeric(nnE, min.len = 1, lower = 1, upper = max(nnE), any.missing = FALSE)
  assert_number(truep, lower = 0, upper = 1)
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
  decision <- vector(length = sim)
  all_sizes <- vector(length = sim)
  all_looks <- vector(length = sim)

  nnE <- sort(nnE)
  nnF <- sort(nnF)
  nn <- sort(unique(c(nnF, nnE)))
  nL <- length(nn)
  Nstart <- nn[1]
  Nmax <- nn[nL]

  # simulate a clinical trial sim times
  for (k in seq_len(sim)) {
    if (length(nn) != 1 && wiggle) {
      # randomly generate look locations
      dist <- h_get_distance(nn = nn)
      nnr <- h_get_looks(dist = dist, nnE = nnE, nnF = nnF) # we generate sim number of looks
      nnrE <- nnr$nnrE
      nnrF <- nnr$nnrF
      orig_nnE <- nnE
      orig_nnF <- nnF
    } else {
      dist <- NULL
      nnrE <- nnE
      nnrF <- nnF
      orig_nnE <- nnrE
      orig_nnF <- nnrF
    }
    nnr <- unique(c(nnrE, nnrF))
    orig_nnr <- unique(c(orig_nnE, orig_nnF))
    tmp <- h_get_decisionDist(
      nnr = nnr,
      nnrE = nnrE,
      nnrF = nnrF,
      truep = truep,
      parE = parE,
      tL = tL,
      tU = tU,
      deltaE = deltaE,
      deltaF = deltaF,
      relativeDelta = relativeDelta,
      orig_nnr = orig_nnr
    )
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
    union_nn = nnr,
    input_nnE = nnE,
    input_nnF = nnF,
    wiggled_nnE = nnrE,
    wiggled_nnF = nnrF,
    wiggle_dist = dist,
    params = as.list(match.call(expand.dots = FALSE))
  )
}
