#' Generating random decision and sample size looks for `decision1 == TRUE` or default option
#'
#' A helper function for [ocPredprob()] to generate numeric of decisions `decisions` and
#' random looks `all_sizes` for `decision1 == TRUE`.
#'
#' @inheritParams h_get_decision
#' @typed nnr : numeric
#'  union of `nnE` and `nnF`.
#' @typed truep : number
#'  assumed true response rate or true rate (scenario).
#' @typed p0 : number
#'  lower Futility threshold of response rate.
#' @typed parE : numeric
#'  alpha and beta parameters for the prior on the treatment population.
#'  Default set at alpha = 1, beta = 1, or uniform prior.
#' @typed nnE : numeric
#'  sample size or sizes where study can be stopped for Efficacy decision. If `0` or `NULL` and
#'  `length(nnE) = 1` then no Efficacy looks are performed.
#' @typed nnF : numeric
#'  sample size or sizes where study can be stopped for Efficacy decision. If `0` or `NULL` and
#'  `length(nnF) = 1` then no Futility looks are performed.
#' @typed tT : number
#'  threshold of which assumed `truep` exceeds acceptable threshold of `p0`.
#' @typed phiU : number
#'  upper threshold on the predictive probability.
#' @typed phiL : number
#'  lower threshold on the predictive probability.
#'
#' @return A list with the following elements:
#'  - `decision` : decision `flag` with `TRUE` for Go, `FALSE` for Stop, `NA` for Gray zone.
#'  - `all_sizes` : resulting number of look size, anything below maximum
#'   look size is an indicated interim, Futility or Efficacy or both.
#'
#' @keywords internal
#'
h_get_decision_one_predprob <- function(nnr, truep, p0, parE = c(1, 1), nnE, nnF, tT, phiU, phiL, orig_nnr) {
  assert_numeric(nnr, lower = 1, sorted = TRUE)
  assert_number(truep, lower = 0, upper = 1)
  assert_numeric(nnE, lower = 1, any.missing = FALSE, sorted = TRUE)
  assert_numeric(nnF, lower = 1, any.missing = FALSE, sorted = TRUE)
  assert_number(tT, lower = 0, upper = 1)
  assert_number(phiU, lower = 0, upper = 1)
  assert_number(phiL, lower = 0, upper = 1)
  assert_numeric(orig_nnr)

  index_look <- 1
  Nmax <- max(nnr)
  decision <- NA
  response <- stats::rbinom(Nmax, size = 1, prob = truep)
  while (is.na(decision) && index_look < length(nnr)) {
    size_look <- nnr[index_look]
    if (size_look %in% nnE) {
      interim_qU <- predprob(
        x = sum(response[1:size_look]),
        n = size_look,
        Nmax = Nmax,
        p = p0,
        thetaT = tT,
        parE = parE
      )$result
      decision <- ifelse(interim_qU > phiU, FALSE, decision)
      all_looks <- orig_nnr[index_look]
    }
    if (size_look %in% nnF) {
      interim_qU <- predprob(
        x = sum(response[1:size_look]),
        n = size_look,
        Nmax = Nmax,
        p = p0,
        thetaT = tT,
        parE = parE
      )$result
      decision <- ifelse(interim_qU < phiL, FALSE, decision)
      all_looks <- orig_nnr[index_look]
    }
    index_look <- index_look + 1
  }
  if (is.na(decision)) {
    size_look <- nnr[index_look]
    if (size_look %in% nnE) {
      final_eff_qU <- postprob(
        x = sum(response[1:size_look]),
        n = size_look,
        p = p0,
        parE = parE,
        log.p = FALSE
      )
    }
    decision <- ifelse(final_eff_qU > tT, TRUE, NA)
    all_looks <- orig_nnr[index_look]
  }
  if (size_look %in% nnF) {
    final_fu_qU <- postprob(
      x = sum(response[1:size_look]),
      n = size_look,
      p = p0,
      parE = parE,
      log.p = FALSE
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

#' Generating random decision and sample size looks for `decision1 == FALSE`
#'
#' A helper function for [ocPredprob()] to generate numeric of decisions `decisions` and
#' random looks `all_sizes` for `decision1 == FALSE`.
#'
#' @inheritParams h_get_decision_one_predprob
#' @typed phiFu : number
#'  upper threshold on the predictive probability.
#' @typed p1 : number
#'  upper Futility threshold of response rate.
#' @typed tF : number
#'  threshold of which assumed `truep` does not exceed threshold of `p1`.
#'
#' @return A list with the following elements:
#'  - `decision` : decision `flag` with `TRUE` for Go, `FALSE` for Stop, `NA` for Gray zone.
#'  - `all_sizes` : resulting number of look size, anything below maximum
#'   look size is an indicated interim, Futility or Efficacy or both.
#'
#' @keywords internal
#'
h_get_decision_two_predprob <- function(nnr, truep, p0, p1, parE = c(1, 1), nnE, nnF, tT, tF, phiFu, phiU, orig_nnr) {
  assert_numeric(nnr, lower = 1, sorted = TRUE)
  assert_number(truep, lower = 0, upper = 1)
  assert_numeric(nnE, lower = 1, any.missing = FALSE, sorted = TRUE)
  assert_numeric(nnF, lower = 1, any.missing = FALSE, sorted = TRUE)
  assert_number(tT, lower = 0, upper = 1)
  assert_number(tF, lower = 0, upper = 1)
  assert_number(phiFu, lower = 0, upper = 1)
  assert_number(phiU, lower = 0, upper = 1)
  assert_numeric(orig_nnr)

  index_look <- 1
  Nmax <- max(nnr)
  decision <- NA
  response <- stats::rbinom(Nmax, size = 1, prob = truep)
  while (is.na(decision) && index_look < length(nnr)) {
    size_look <- nnr[index_look]
    if (size_look %in% nnE) {
      # GO when P(success at final) > phiU
      interim_qU <- predprob( # success at final is defined by P(p > p0) > tT
        x = sum(response[1:size_look]),
        n = size_look,
        Nmax = Nmax,
        p = p0,
        thetaT = tT,
        parE = parE
      )$result
      decision <- ifelse(interim_qU > phiU, TRUE, NA)
      all_looks <- orig_nnr[index_look]
    }
    if (size_look %in% nnF) {
      # STOP when P (failure at final ) > phiFu
      interim_qU <- 1 - predprob( # failure at final is defined as P(p < p1) > tF
        x = sum(response[1:size_look]),
        n = size_look,
        Nmax = Nmax,
        p = p1,
        thetaT = tF,
        parE = parE
      )$result
      decision <- ifelse(interim_qU > phiFu, FALSE, decision)
      all_looks <- orig_nnr[index_look]
    }
    index_look <- index_look + 1
  }
  if (is.na(decision)) {
    size_look <- nnr[index_look]
    if (size_look %in% nnE) { # for efficacy looks at FINAL
      final_qU <- postprob(
        # based on all data, the posterior probability is a GO when P(p > p0) > tT
        x = sum(response[1:size_look]),
        n = size_look,
        p = p0,
        parE = parE,
        log.p = FALSE
      )
      decision <- ifelse(final_qU > tT, TRUE, NA)
      all_looks <- orig_nnr[index_look]
    }
    if (size_look %in% nnF) { # for futility looks at FINAL
      # based on all data, the posterior probability is a STOP when P(p < p1) > tF
      final_qU <- 1 - postprob(
        x = sum(x = response[1:size_look]),
        n = size_look,
        p = p1,
        parE = parE,
        log.p = FALSE
      )
      decision <- ifelse(final_qU > tF, FALSE, decision)
      all_looks <- orig_nnr[index_look]
    }
  }
  list(
    decision = decision,
    all_sizes = size_look,
    all_looks = all_looks
  )
}

#' Operating Characteristics for Predictive Probability method
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Calculate operating characteristics for predictive probability method.
#'
#' It is assumed that the true response rate is `truep`. Trials can stop for Futility
#' or Efficacy. Trials can also stop at Interim or Final for Futility or Efficacy.
#'
#' There are two variations of decision rule, Decision 1 and Decision 2, to
#' evaluate decision at Interim or Final, for Futility or Efficacy. Decision 1 is
#' used when `decision1 == TRUE` which is the default setting.
#'
#' ## Decision 1:
#' The criteria for Decision 1 for Interim looks are :
#' - interim GO =  P(successful trial at final) > phiU
#' - interim STOP = P(successful trial at final) < phiL
#'
#' The criteria for Decision 1 for Final looks are:
#' - Final GO = P( RR > p0 | data) > tT
#' - Final STOP = P( RR > p0 | data ) < tT
#'
#' ## Decision 2:
#' The criteria for Decision 2 for Interim looks are :
#' - Interim GO : P ( success at final) > phiU
#' - Interim STOP : P (failure at final ) > phiFu
#'
#' The criteria for Decision 2 for Futility looks are :
#' - Final GO = P( RR > p0) > tT
#' - Final STOP = P( RR  < p1) > tF
#'
#' @inheritParams h_get_decision_one_predprob
#' @inheritParams h_get_decision_two_predprob
#' @typed sim : number
#'  number of simulations.
#' @typed wiggle : flag
#'  generate random look locations (not default). If `wiggle = TRUE` and `nnE = nnF`, then all wiggled
#'  looks are the same between `nnE` and `nnF`.
#' @typed decision1 : flag
#'  Flag if `decision1 = TRUE` then Decision 1 rules will be used, otherwise Decision 2 rules will be used.
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
#' @example examples/ocPredprob.R
#' @export
ocPredprob <- function(nnE,
                       truep,
                       p0,
                       phiU,
                       p1 = p0,
                       tT = 1 - tF,
                       tF = 1 - tT,
                       phiL = 1 - phiFu,
                       phiFu = 1 - phiL,
                       parE = c(1, 1),
                       sim = 50000,
                       wiggle = FALSE,
                       nnF = nnE,
                       decision1 = TRUE) {
  assert_numeric(nnE, lower = 1, any.missing = FALSE, sorted = TRUE)
  assert_number(sim, lower = 1, finite = TRUE)
  assert_flag(wiggle)
  assert_numeric(nnF, lower = 1, any.missing = FALSE, sorted = TRUE)
  assert_flag(decision1)

  nn <- sort(unique(c(nnF, nnE)))
  Nmax <- max(nn)
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
      dist <- NULL
      nnrE <- nnE
      nnrF <- nnF
      orig_nnE <- nnrE
      orig_nnF <- nnrF
    }
    nnr <- unique(sort(c(nnrE, nnrF)))
    orig_nnr <- unique(c(orig_nnE, orig_nnF))
    tmp <- if (decision1) {
      h_get_decision_one_predprob(
        nnr = nnr,
        truep = truep,
        p0 = p0,
        parE = parE,
        nnE = nnrE,
        nnF = nnrF,
        tT = tT,
        phiU = phiU,
        phiL = phiL,
        orig_nnr = orig_nnr
      )
    } else {
      h_get_decision_two_predprob(
        nnr = nnr,
        truep = truep,
        p0 = p0,
        p1 = p1,
        parE = parE,
        nnE = nnrE,
        nnF = nnrF,
        tT = tT,
        tF = tF,
        phiFu = phiFu,
        phiU = phiU,
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
