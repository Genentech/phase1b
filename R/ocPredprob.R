#' @include predprob.R
NULL

#' Generating random decision and sample size looks for `decision1 == TRUE` or default option
#'
#' A helper function for `ocPredprob` to generate numeric of decisions `decisions` and
#' random looks `all_sizes` for `decision1 == TRUE`.
#'
#' @typed nnr : numeric
#'  union of `nnE`and `nnF` from `ocPredprob`.
#' @typed truep : number
#'  assumed true response rate or true rate (scenario).
#' @typed p0 : number
#'  lower Futility threshold of response rate.
#' @typed parE : numeric
#'  alpha and beta parameters for the prior on the treatment population.
#'  Default set at alpha = 1, beta = 1, or uniform prior.
#' @typed nnE : numeric
#'  sample size or sizes where study can be stopped for Efficacy decision.
#' @typed nnF : numeric
#'  sample size or sizes where study can be stopped for Efficacy decision.
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
h_get_decision_one_predprob <- function(nnr, truep, p0, parE = c(1, 1), nnE, nnF, tT, phiU, phiL) {
  index_look <- 1
  Nmax <- max(nnr)
  assert_numeric(nnr, lower = 1, sorted = TRUE)
  assert_number(truep, lower = 0, upper = 1)
  assert_number(p0, lower = 0, upper = 1)
  assert_numeric(parE, min.len = 2, any.missing = FALSE)
  assert_numeric(nnE, lower = 1, any.missing = FALSE, sorted = TRUE)
  assert_numeric(nnF, lower = 1, any.missing = FALSE, sorted = TRUE)
  assert_number(tT, lower = 0, upper = 1)
  assert_number(phiU, lower = 0, upper = 1)
  assert_number(phiL, lower = 0, upper = 1)
  decision <- NA
  response <- stats::rbinom(max(nnr), size = 1, truep)
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
    }
    index_look <- index_look + 1
  }
  if (is.na(decision)) {
    assert_numeric(nnE, lower = 1, any.missing = FALSE, sorted = TRUE)
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
  }
  assert_numeric(nnF, lower = 1, any.missing = FALSE, sorted = TRUE)
  if (size_look %in% nnF) {
    final_fu_qU <- postprob(
      x = sum(response[1:size_look]),
      n = size_look,
      p = p0,
      parE = parE,
      log.p = FALSE
    )
    decision <- ifelse(final_fu_qU < tT, FALSE, decision)
  }
  list(
    decision = decision,
    all_sizes = size_look
  )
}

#' Title
#'
#' @typed nnr
#' @typed truep
#' @typed p0
#' @typed p1
#' @typed parE
#' @typed nnE
#' @typed nnF
#' @typed tT
#' @typed tF
#' @typed phiFu
#' @typed phiU
#'
#' @return
#' @export
#'
#' @examples
h_get_decision_two_predprob <- function(nnr, truep, p0, p1, parE = c(1, 1), nnE, nnF, tT, tF, phiFu, phiU) {
  index_look <- 1
  assert_numeric(nnr)
  all_sizes <- decision <- NA
  response <- stats::rbinom(max(nnr), size = 1, truep)
  assert_numeric(response, lower = 0, upper = 1)
  while (is.na(decision) && index_look < length(nnr)) { # as long as there is no decision...
    size_look <- nnr[index_look]
    if (size_look %in% nnE) {
      interim_qU <- predprob(
        x = sum(x = response[1:size_look]),
        n = size_look,
        Nmax = nnE[length(nnE)],
        p = p0,
        thetaT = tT,
        parE = parE
      )$result
      decision <- ifelse(interim_qU > phiU, TRUE, NA)
    }
    if (size_look %in% nnF) {
      interim_qU <- 1 - predprob(
        x = sum(x = response[1:size_look]),
        n = size_look,
        Nmax = nnF[length(nnF)],
        p = p1,
        thetaT = tF,
        parE = parE
      )$result
      decision <- ifelse(interim_qU > phiFu, FALSE, decision)
    }
    index_look <- index_look + 1
  }
  if (is.na(decision)) {
    if (size_look %in% nnE) { # for efficacy looks at FINAL
      final_qU <- postprob(
        # based on all data, the posterior probability is a GO when P(truep > p0) > tT
        x = response,
        n = size_look,
        p = truep,
        parE = c(1, 1),
        log.p = FALSE
      )
      decision <- ifelse(final_qU > tT, TRUE, NA)
    }
    if (size_look %in% nnF) { # for futility looks at FINAL
      # based on all data, the posterior probability is a STOP when P(truep > p0) > tF
      final_qU <- 1 - postprob(
        x = response,
        n = size_look,
        p = truep,
        parE = c(1, 1),
        log.p = FALSE
      )
      decision <- ifelse(final_qU > tF, FALSE, decision)
    }
  }
  list(
    decision = decision,
    all_sizes = size_look
  )
}

#' Calculate operating characteristics for predictive probability method
#' (gray zone allowed in the final analysis)
#'
#' Decision rule 1:The trial is stopped for efficacy if the predictive probability of a
#' successful trial is larger than phiU, and stopped for futility if it is below
#' phiL. A trial is successful if after the maximum number of patients the
#' posterior probability of the treatment having more than p0 response rate is
#' above tT. Otherwise the decision is "failure". In this case, there is no gray zone.
#'
#' Decision rule 2:A variation can be requested when skipping the argument phiL and utilizing the arguments
#' p1, tFu & PhiFu. The trial can be stopped for futility if the predictive
#' probability of an unsuccessful trial is larger than phiFu. In this case, the
#' decision is "failure" when the posterior probability of having the treatment
#' response rate at most p1 is above tFu at the maximum number of patients.
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
#' @param nn vector of look locations for efficacy
#' (if futility looks should be different, please specify also \code{nnF})
#' @param p true rate (scenario)
#' @param p0 threshold on the response rate
#' @param p1 futility threshold on the response rate. Specify only when decision rule 2 is used.
#' @param tT threshold for the probability to be above the response rate p0
#' at the end of the trial
#' @param tFu threshold for the probability to be under the response rate p1
#' at the end of the trial. Specify only when decision rule 2 is used.
#' @param phiL lower threshold on the predictive probability
#' @param phiU upper threshold on the predictive probability
#' @param phiFu threshold on the predictive probability for futility. Specify only
#' when decision rule 2 is used. phiL argument should be skipped in this case.
#' @param parE beta parameters for the prior on the treatment proportion
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
#' params: multiple parameters
#'
#' @example examples/ocPredprob.r
#' @export
ocPredprob <- function(nn, p, p0, p1 = p0, tT, tFu = 1 - tT, phiL = 1 - phiFu, phiU, phiFu = 1 - phiL,
                       parE = c(1, 1),
                       ns = 10000, nr = FALSE, d = NULL, nnF = nn) {
  # Calculate operating characteristics via simulation
  # nn: vector of look locations
  # s: decision reject H0 (TRUE) or fail to reject (FALSE)
  #    during trial if continuing (NA)

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
    # set parameter d for randomly generating look locations
    d <- floor(min(nn - c(0, nn[-nL])) / 2)
  }
  nnr <- nn
  nnrE <- nnE
  nnrF <- nnF
  for (k in 1:ns) {
    # simulate a clinical trial ns times
    if (nr && (d > 0)) {
      # randomly generate look locations
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
        qL <- 1 - predprob(x = sum(x[1:i]), n = i, Nmax = Nmax, p = p1, thetaT = 1 - tFu, parE = parE)$result

        s[k] <- ifelse(qL > phiFu, FALSE, NA)
      }
      if (i %in% nnrE) {
        q <- predprob(x = sum(x[1:i]), n = i, Nmax = Nmax, p = p0, thetaT = tT, parE = parE)$result

        s[k] <- ifelse(q >= phiU & !(i < Nmax & phiU == 1), TRUE, s[k])
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
