#' @include postprob.R
NULL

#' Operating Characteristics for Posterior Probability method
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Calculate operating characteristics for posterior probability method.
#'
#' It is assumed that the true response rate is `truep`.
#' The trial is stopped for efficacy if the posterior probability to be
#' above `p1` is larger than `tU`, and stopped for futility if the posterior
#' probability to be below `p0` is larger than `tL`:
#'
#' Stop criteria for Efficacy : `P_E(p > p1) > tU`
#'
#' Stop criteria for Futility : `P_E(p < p0) > tL`
#'
#'
#' Resulting Operating Characteristics include the following:
#'
#' - `ExpectedN`: expected number of patients in the trials
#' - `PrStopEarly`: probability to stop the trial early (before reaching the
#' maximum sample size)
#' - `PrEarlyEff`: probability of Early Go decision
#' - `PrEarlyFut`: probability to decide for Futility early
#' - `PrEfficacy`: probability of Go decision
#' - `PrFutility`: Probability of Stop decision
#' - `PrGrayZone`: probability between Go and Stop ,"Evaluate" or Gray decision zone
#'
#' @typed nnE : numeric
#'  sample size or sizes where study can be stopped for efficacy decision. If different for futility decision,
#'  specify in `nnF`.
#' @typed truep : number
#'  assumed true response rate nor true rate (scenario).
#' @typed p0 : number
#'  lower efficacy threshold of response rate.
#' @typed p1 : number
#'  upper efficacy threshold of response rate.
#' @typed tL : number
#'  posterior probability threshold for being below `p0`.
#' @typed tU : number
#'  posterior probability threshold for being above `p1`.
#' @typed parE : numeric
#'  beta parameters for the prior on the treatment proportion.
#' @typed sim : number
#'  number of simulations.
#' @typed wiggle : logical
#'  generate random look locations (not default)
#'  if `TRUE`, specify `dist` (see @details)
#' @typed dist : "`numeric` or `NULL`" #TODO ( was dl)... check Roxytypes
#'  distance for random looks around the look locations in `nn`.
#'  If `NULL`, only one location look will be set at nnE or nnF or n
#' @typed nnF : numeric
#'  sample size or sizes where study can be stopped for futility decision. If different for futility decision,
#'  specify in `nnF`.
#'  ## From helper functions
#' @typed nnrE : numeric
#'  same as `nnE` but if wiggle room and distance applied.
#' @typed nnrF : numeric
#'  same as `nnF` but if wiggle room and distance applied.
#
#'
#' @return A list with the following elements:
#'
#' - `oc`: matrix with operating characteristics (see Details section)
#' Decision: vector of the decisions made in the simulated trials
#' (`TRUE` for success, `FALSE` for failure, `NA` for no
#' decision)
#' SampleSize: vector of the sample sizes in the simulated trials
#' - `nn`: vector of look locations that was supplied
#' - `nnE`: vector of efficacy look locations
#' - `nnF`: vector of futility look locations
#' - `params`: multiple parameters
#' -  `Decision` : resulting decision, one of `TRUE` for GO, `FALSE`for STOP, `NA` for Gray zone
#'
#' @details
#' ## About arguments
#'
#' `ExpectedN` is an average of the simulated sample sizes.
#'  If `wiggle = TRUE`, one can specify `dist`, though the algorithm will generate it if `dist = NULL`
#'  If `nnF = NULL`, no Futility or decision to Stop will be analysed. Note that `nnF = c(0)` is equivalent.
#'  As default, `nnF` is set to the identical looks of `nnE`, and if `wiggle = TRUE`, all looks are the same, e.g.
#'  `nnE = nnF` when wiggle and distance is applied.
#'
#' ## About helper function
#'
#' `get_distance` inputs `dist` into `get_looks` and thereafter contributes to arguments in `get_decision`.
#' Finally, `get_oc` generates a list of parameters such as `decisions`, `all_sizes` and operating characteristics (oc).
#'
#'
#' @example examples/ocPostprob.R
#' @export
#'

#-- helper functions for OcPostProb
#-- get_distance
get_distance <- function(nn) {
  assert_numeric(nn, unique = TRUE, sorted = TRUE)
  dist0 <- floor(min(nn - c(0, nn[-length(nn)])) / 2)
  assert_numeric(dist0, sorted = TRUE)
  dist <- sample(-dist0:dist0,
    size = length(nn) - 1,
    replace = TRUE,
    prob = 2^(c(-dist0:0, rev(-dist0:(-1))) / 2)
  )
  dist
}

#-- get_looks helper function
get_looks <- function(dist, nnE, nnF) {
  assert_numeric(nnE)
  assert_numeric(nnF)
  nn <- unique(c(nnE, nnF))
  assert_numeric(nn)
  assert_numeric(dist)
  nnr <- nn + c(dist, 0)
  list(
    nnrE = nnr[nn %in% nnE],
    nnrF = nnr[nn %in% nnF]
  )
}

#-- get_decision helper function
get_decision <- function(nnr, response, truep, p0, p1, parE = c(1, 1), nnE, nnF, tL, tU) {
  index_look <- 1
  assert_numeric(nnr)
  size_look <- nnr[index_look]
  all_sizes <- decision <- NA
  response <- stats::rbinom(max(nnr), 1, truep)
  assert_numeric(response, lower = 0, upper = 1)
  while (is.na(decision) && index_look <= length(nnr)) {
    if (size_look %in% nnF) {
      qL <- 1 - postprob(x = sum(response[1:size_look]), n = size_look, p = p0, parE = parE) # for each
      assert_number(qL, lower = 0, upper = 1)
      decision <- ifelse(qL >= tL, FALSE, NA)
    }
    if (size_look %in% nnE) {
      qU <- postprob(x = sum(response[1:size_look]), n = size_look, p = p1, parE = parE)
      assert_number(qU, lower = 0, upper = 1)
      decision <- ifelse(qU < tU, decision, TRUE)
    }
    all_sizes <- size_look
    index_look <- index_look + 1
    size_look <- nnr[index_look]
    # }
  }
  list(
    decision = decision,
    all_sizes = all_sizes
  )
}

#-- get_oc helper function
get_oc <- function(all_sizes, nnr, decision, nnrE, nnrF) {
  sim <- length(all_sizes)
  assert_logical(decision, len = sim)
  assert_numeric(all_sizes)
  assert_numeric(nnrE, lower = 0, upper = max(nnrE))
  assert_numeric(nnrF, lower = 0, upper = max(nnrF))
  data.frame(
    ExpectedN = mean(all_sizes, na.rm = TRUE),
    PrStopEarly = mean(all_sizes < max(nnrF), na.rm = TRUE),
    PrEarlyEff = sum(decision * (all_sizes < max(nnrE)), na.rm = TRUE) / sim,
    PrEarlyFut = sum((1 - decision) * (all_sizes < max(nnrF)), na.rm = TRUE) / sim,
    PrEfficacy = sum(decision, na.rm = TRUE) / sim,
    PrFutility = sum(1 - decision, na.rm = TRUE) / sim,
    PrGrayZone = sum(is.na(decision)) / sim
  )
}

#-- ocPostprob
ocPostprob <- function(nnE, truep, p0, p1, tL, tU, parE = c(1, 1),
                       sim = 1000, wiggle = FALSE, randomdist = NULL, nnF = nnE) {
  nn <- sort(unique(c(nnF, nnE)))
  decision <- vector(length = sim)
  all_sizes <- vector(length = sim)
  assert_logical(decision)
  assert_logical(all_sizes)
  for (k in 1:sim) {
    if (length(nn) != 1 && wiggle && is.null(randomdist)) {
      dist <- get_distance(nn = nn)
      nnr <- get_looks(dist = dist, nnE = nnE, nnF = nnF)
      nnrE <- nnr$nnrE
      nnrF <- nnr$nnrF
    } else {
      nnrE <- nnE
      nnrF <- nnF
    }
    nnr <- unique(c(nnrE, nnrF))
    tmp <- get_decision(
      nnr = nnr, response = response,
      truep = truep, p0 = p0, p1 = p1,
      parE = c(1, 1), nnE = nnrE,
      nnF = nnrF, tL = tL, tU = tU
    )
    decision[k] <- tmp$decision
    all_sizes[k] <- tmp$all_sizes
  }
  oc <- get_oc(all_sizes = all_sizes, nnr = nnr, decision = decision, nnrE = nnrE, nnrF = nnrF)
  list(
    oc = oc,
    Decision = decision,
    SampleSize = all_sizes,
    union_nn = nnr,
    input_nnE = nnE,
    input_nnF = nnF,
    wiggled_Eff_n = nnrE,
    wiggled_Fut_n = nnrF,
    wiggle_dist = dist,
    params = as.list(match.call(expand.dots = FALSE))
  )
}
