#' @include postprob.R
NULL

#' Generating random distance in given looks for sample sizes for efficacy and futility.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A helper function for `ocPostprob` to generate random distance's wiggle room around looks `nn`.
#' Numeric looks `nn` must be of minimum two elements and will generate `length(nn)-1` distances.
#'
#' @param nn : number or numeric
#' the union of `nnE` and `nnF` (if futility analysis or looks exists) supplied
#'
#' @return A numeric with `length(nn)-1` elements.
#'
#' @export
#'
#' @examples examples / ocPostprob.R
get_distance <- function(nn) {
  assert_numeric(nn, unique = TRUE, sorted = TRUE, min.len = 1)
  dist0 <- floor(min(nn - c(0, nn[-length(nn)])) / 2)
  assert_numeric(dist0, sorted = TRUE)
  sample(-dist0:dist0,
    size = length(nn) - 1,
    replace = TRUE,
    prob = 2^(-abs(-dist0:dist0) / 2)
  )
}


#' Generating looks
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A helper function for `ocPostprob` that applies the numeric element of `dist` to looks `nn`.
#'
#' @typed dist : numeric or logical
#' Distance for random looks around the look locations in `nn`,
#' where `dist`is generated from `get_distance` in a numeric of at least one element.
#' If `NULL`, only one location look will be set at `nnE` or `nnF`.
#' @typed nnE : numeric
#' Sample size or sizes where study can be stopped for efficacy decision. If different for futility decision,
#' specify in `nnF`.
#' @typed nnF : numeric
#' Sample size or sizes where study can be stopped for futility decision if different from efficacy decision.
#'
#' @return Uses distance from `get_distance` to add to looks, creating wiggled looks:
#' `nnrE`is the result for efficacy looks with random distance added.
#' `nnrF`is the result for futility looks with random distance added.
#'
#' @export
#'
#' @examples examples / ocPostProb.R
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

#' Generating random decision and sample size looks.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A helper function for `ocPostprob` to generate numeric of decisions `decisions` and random looks `all_sizes`.
#'
#' @inheritParams get_looks
#' @typed nnr : numeric
#' union of `nnE`and `nnF`.
#' @typed response : numeric
#' A numeric of Bernoulli successes based on `size_look`
#' @typed truep : number
#'  assumed true response rate or true rate (scenario).
#' @typed p0 : number
#'  lower efficacy threshold of response rate.
#' @typed p1 : number
#'  upper efficacy threshold of response rate.
#' @typed tL : number
#'  posterior probability threshold for being below `p0`.
#' @typed tU : number
#'  posterior probability threshold for being above `p1`.
#' @typed parE : numeric
#'  Alpha and beta parameters for the prior on the treatment proportion.
#'  Default set at alpha = 1, beta = 1, or uniform prior.
#'
#' @return A list of the following objects :
#'  - `decision` : resulting numeric of decision, one of `TRUE` for GO, `FALSE`for STOP, `NA` for Gray zone
#'  - `all_sizes` : resulting numeric of look size, anything below maximum
#'                  look size is an indicated interim, futility or efficacy or both
#'
#' @export
#'
#' @examples
get_decision <- function(nnr, response, truep, p0, p1, parE = c(1, 1), nnE, nnF, tL, tU) {
  index_look <- 1
  assert_numeric(nnr)
  size_look <- nnr[index_look]
  all_sizes <- decision <- NA
  response <- stats::rbinom(max(nnr), size = 1, truep)
  assert_numeric(response, lower = 0, upper = 1)
  while (is.na(decision) && index_look <= length(nnr)) {
    if (size_look %in% nnF) {
      qL <- 1 - postprob(x = sum(response[1:size_look]), n = size_look, p = p0, parE = parE)
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
  }
  list(
    decision = decision,
    all_sizes = all_sizes
  )
}

#' Creating list for operating characteristics.
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Generates operating characteristics.
#'
#' @inheritParams get_looks
#' @inheritParams get_decision
#' @typed nnrE : numeric
#' Looks with random distance, if applied on `nnE`.
#' @typed nnrF : numeric
#' Looks with random distance, if applied on `nnF`.
#' @typed all_sizes : numeric
#' Sample sizes of all looks simulated `length(sim)` times if `dist` applied.
#' @typed decision : numeric
#' Go, Stop or Gray Zone decisions of all looks simulated `length(sim)` times.
#'
#' @return A list of results containing :
#'
#' - `ExpectedN`: expected number of patients in the trials
#' - `PrStopEarly`: probability to stop the trial early (before reaching the
#' maximum sample size)
#' - `PrEarlyEff`: probability of Early Go decision
#' - `PrEarlyFut`: probability to decide for Futility early
#' - `PrEfficacy`: probability of Go decision
#' - `PrFutility`: Probability of Stop decision
#' - `PrGrayZone`: probability between Go and Stop ,"Evaluate" or Gray decision zone
#' @export
#'
#' @examples examples / ocPostprob.R
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
#' Stop criteria for Efficacy :
#'
#' `P_E(p > p1) > tU`
#'
#' Stop criteria for Futility :
#'
#' `P_E(p < p0) > tL`
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
#' @inheritParams get_looks
#' @inheritParams get_decision
#' @typed sim : number
#'  number of simulations
#' @typed wiggle : logical
#'  generate random look locations (not default)
#'  if `TRUE`, optional to specify `dist` (see @details)
#' @typed randomdist : logical
#'  Random distance added to looks. if `NULL`, and `wiggle = TRUE`, function will
#'  generate and add a random distance within range of the closest looks.
#'
#' @return A list with the following elements:
#'
#' - `oc`: matrix with operating characteristics (see @details section)
#' - `nn`: vector of look locations that was supplied
#' - `nnE`: vector of efficacy look locations
#' - `nnF`: vector of futility look locations # TODO
#' - `params`: multiple parameters# TODOs
#'
#' @details
#' ## About arguments
#'
#' `ExpectedN` is an average of the simulated sample sizes.
#'  If `wiggle = TRUE`, one can specify `dist`, though the algorithm will generate it if `dist = NULL`.
#'  If `nnF = NULL`, no Futility or decision to Stop will be analysed. Note that `nnF = c(0)` is equivalent.
#'  As default, `nnF` is set to the identical looks of `nnE`, and if `wiggle = TRUE`, all looks are the same, e.g.
#'  `nnE = nnF` when wiggle and distance is applied.
#'
#' @example examples/ocPostprob.R
#' @export
ocPostprob <- function(nnE, truep, p0, p1, tL, tU, parE = c(1, 1),
                       sim = 50000, wiggle = FALSE, randomdist = NULL, nnF = nnE) {
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