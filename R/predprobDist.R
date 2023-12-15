#' @include dbetabinom.R
#' @include postprobDist.R
NULL

#' The predictive probability of success in single arm studies.
#'
#' The helper function to generate the predictive probability of success based only on treatment group (`E`)
#' as there is no control or standard of care (`S`), indicated by `NmaxControl == 0`.
#'
#' @typed x : number
#'  number of successes in the `E` group at interim.
#' @typed mE : number
#'  number of successes in the remaining `Nmax-n` number of patients in the treatment `E` group.
#' @typed Nmax : number
#'   maximum number of patients in the `E` group at final analysis.
#' @typed delta : number
#'   difference between response rates to be met.
#' @typed relativeDelta : flag
#'  If `TRUE`, then a `relativeDelta` is used. Represents that a minimum
#'  response rate in magnitude of `delta` of the `S` non-responding patients. See [postprobDist()].
#' @typed parE : numeric
#'  parameters for beta distribution. If it is a matrix, it needs to have 2 columns,
#'  and each row corresponds to each component of a beta-mixture distribution
#'  for the `E` group.
#' @typed parS : numeric
#'  parameters for beta distribution. If it is a matrix, it needs to have 2 columns,
#'  and each row corresponds to each component of a beta-mixture distribution for the `S` group.
#' @typed weights : numeric
#'  the mixture weights of the beta mixture prior.
#' @typed weightsS : numeric
#'  weights for the `S` group.
#' @typed thetaT : number
#'  threshold on the probability to be used.
#' @typed density : numeric
#'  the beta binomial mixed density for future success in `Nmax-n` patients in the `E` group.
#'
#' @return A `list` is returned with names `result` for predictive probability and
#'  `table` of numeric values with counts of responses in the remaining patients,
#'  probabilities of these counts, corresponding probabilities to be above threshold,
#'  and trial success indicators.
#'
#' @keywords internal

h_predprobdist_single_arm <- function(x,
                                      mE,
                                      n,
                                      Nmax,
                                      delta,
                                      relativeDelta,
                                      parE,
                                      parS,
                                      weights,
                                      weightsS,
                                      thetaT) {
  assert_number(x, lower = 0, upper = Nmax)
  assert_number(mE, lower = 0)
  assert_number(x + mE, upper = Nmax)
  assert_number(thetaT, lower = 0, upper = 1)
  posterior_y <- postprobDist(
    x = x + c(0:mE),
    n = Nmax,
    delta = delta,
    relativeDelta = relativeDelta,
    parE = parE,
    weights = weights,
    parS = parS,
    weightsS = weightsS
  )
  activeBetamixPost <- h_getBetamixPost(x = x, n = n, par = parE, weights = weights)
  density_y <- with(
    activeBetamixPost,
    dbetabinomMix(x = 0:mE, m = mE, par = par, weights = weights)
  )
  assert_numeric(posterior_y, lower = 0, upper = 1, finite = TRUE, any.missing = FALSE)
  assert_numeric(density_y, lower = 0, upper = 1, finite = TRUE, any.missing = FALSE)
  list(
    result = sum(density_y * (posterior_y > thetaT)),
    table = data.frame(
      counts = c(0:mE),
      cumul_counts = x + (0:mE),
      density = density_y,
      posterior = posterior_y,
      success = (posterior_y > thetaT)
    )
  )
}


#' The predictive probability of success in two-arm studies.
#'
#' The helper function to generate the predictive probability of success
#' based on the difference in treatment group (`E`) and control or
#' standard of care (`S`) group.
#'
#' @inheritParams h_predprobdist_single_arm
#'
#' @typed n : number
#'  number of patients in the `E` group at interim.
#' @typed NmaxControl : number
#'   maximum number of patients in the `S` group at final analysis.
#'
#'
#' @return A `list` is returned with names `result` for predictive probability and
#'  `table` of numeric values with counts of responses in the remaining patients, `density` for
#'  probabilities of these counts, `posterior` for corresponding probabilities to be above threshold,
#'  and `success`for trial success indicators.
#'
#' @keywords internal

h_predprobdist <- function(x,
                           n,
                           xS,
                           nS,
                           Nmax,
                           NmaxControl,
                           delta,
                           relativeDelta,
                           parE,
                           parS,
                           weights,
                           weightsS,
                           thetaT) {
  assert_number(x, lower = 0, upper = Nmax)
  assert_number(n, lower = 0, upper = Nmax)
  assert_number(xS, lower = 0, upper = NmaxControl)
  assert_number(nS, lower = 0, upper = NmaxControl)
  assert_number(Nmax, lower = x)
  assert_number(NmaxControl, lower = xS)
  assert_number(thetaT, lower = 0, upper = 1)
  mS <- NmaxControl - nS
  mE <- Nmax - n
  assert_number(xS + mS, lower = 0, upper = NmaxControl)
  assert_number(x + mE, lower = 0, upper = Nmax)
  controlBetamixPost <- h_getBetamixPost(
    x = xS,
    n = nS,
    par = parS,
    weights = weightsS
  )
  density_z <- with(
    controlBetamixPost,
    dbetabinomMix(x = 0:mS, m = mS, par = par, weights = weights)
  )
  activeBetamixPost <- h_getBetamixPost(x = x, n = n, par = parE, weights = weights)
  density_y <- with(
    activeBetamixPost,
    dbetabinomMix(x = 0:mE, m = mE, par = par, weights = weights)
  )
  # determine resulting posterior probabilities:
  outcomesY <- x + c(0:mE)
  outcomesZ <- xS + c(0:mS)
  density_yz <- posterior_yz <- matrix(
    nrow = 1 + mE,
    ncol = 1 + mS,
    dimnames =
      list(
        0:mE,
        0:mS
      )
  )
  for (i in seq_along(outcomesY)) {
    for (j in seq_along(outcomesZ)) {
      posterior_yz[i, j] <-
        postprobDist(
          x = outcomesY[i],
          n = Nmax,
          xS = outcomesZ[j],
          nS = NmaxControl,
          delta = delta,
          relativeDelta = relativeDelta,
          parE = parE,
          weights = weights,
          parS = parS,
          weightsS = weightsS
        )
      density_yz[i, j] <- density_y[i] * density_z[j] # check each
    }
  }
  ret <- list(
    result = sum(density_yz * (posterior_yz > thetaT)),
    table = data.frame(
      counts = c(0:mE),
      cumul_counts = x + (0:mE)
    ),
    density = density_yz,
    posterior = posterior_yz,
    success = (posterior_yz > thetaT)
  )
  ret
}


#' Compute the predictive probability that the trial will be
#' successful, with a prior distribution on the standard of care or `S`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Compute the predictive probability of trial success given current data.
#'
#' Success means that at the end of the trial the posterior probability is:
#' `Pr(P_E > P_S + delta_0 | data) >= thetaT`.
#'
#' Where `P_E` is the response rate of the treatment or `E`group and
#' `P_S` is the response rate of the standard of care of `S` group.
#'
#' Then the predictive probability for success is:
#' `pp = sum over i: Pr(Y = i | x , n)*I{Pr(P_E > P_S + delta | x,Y=i) >= thetaT}`,
#' where `Y` is the number of future responses in the treatment group and `x` is
#' the current number of responses in the treatment group out of `n`.
#' Please see note in [postprobDist()] for definition of `delta` and `relativeDelta`.
#' Prior is `P_E ~ beta(a, b)` and uniform which is a `beta(1,1)`.
#' However, a beta mixture prior can also be specified. Analogously
#' for `P_S` either a classic beta prior or a beta mixture prior can be
#' specified.
#'
#' Also data on the `S` might be available. Then the predictive probability is
#' more generally defined as :
#' `pp = sum over i, j: Pr(Y = i | x, n)*Pr(Z = j | xS, nS )*I{Pr(P_E > P_S + delta | x,xS, Y=i, Z=j ) >= thetaT}`
#' where `Z` is the future number of responses in the `S` group, and `xS` is the
#' current number of responses in the `S` group.
#'
#' In the case where `NmaxControl = 0`, a table with the following contents will be included in the return output :
#' - `counts`: `Y = i`, number of future successes in `Nmax-n` subjects in ascending order.
#' - `cumul_counts`: `Y = i`, number of future successes in `Nmax-n` subjects.
#' - `density`: `Pr(Y = i|x)` using beta-(mixture)-binomial distribution.
#' - `posterior`: `Pr(P_E > P_S + delta | x, Y = i)` using beta posterior.
#' - `success`: indicator `I( b > thetaT )`.
#'
#' If `NmaxControl` is not zero, i.e., when data on the control treatment
#' is available in this trial, then a list with will be included with the
#' following elements:
#' - `counts`: `Y = i`, number of future successes in `Nmax-n` subjects in ascending order.
#' - `cumul_counts`: `Y = i`, number of future successes in `Nmax-n` subjects.
#' - `density`: `Pr(Y = i|x)` using beta-(mixture)-binomial distribution.
#' - `posterior`: `Pr(P_E > P_S + delta | x, Y = i)` using beta posterior.
#' - `success`: indicator `I( b > thetaT )`.
#'
#' @inheritParams h_predprobDist
#'
#' @return A `list` is returned with names `result` for predictive probability including or separately a
#'  `table` of numeric values with counts of responses in the remaining patients, `density` for
#'  probabilities of these counts, `posterior` for corresponding probabilities to be above threshold,
#'  and `success`for trial success indicators.
#'
#' @references Lee, J. J., & Liu, D. D. (2008). A predictive probability
#' design for phase II cancer clinical trials. Clinical Trials, 5(2),
#' 93-106. doi:10.1177/1740774508089279
#'
#' @example examples/predprobDist.R
#'
#' @export
predprobDist <- function(x, n,
                         xS = 0,
                         nS = 0,
                         Nmax,
                         NmaxControl = 0,
                         delta = 0,
                         relativeDelta = FALSE,
                         parE = c(a = 1, b = 1),
                         weights,
                         parS = c(a = 1, b = 1),
                         weightsS,
                         thetaT) {
  # ensure reasonable numbers
  stopifnot(
    n <= Nmax,
    nS <= NmaxControl,
    x <= n,
    xS <= nS
  )
  mE <- Nmax - n # remaining active patients
  # if par is a vector => situation where there is only one component
  if (is.vector(parE)) {
    stopifnot(identical(length(parE), 2L))
    parE <- t(parE)
  }
  if (missing(weights)) {
    weights <- rep(1, nrow(parE))
  }
  # if parS is a vector => situation where there is only one component
  if (is.vector(parS)) {
    stopifnot(identical(length(parS), 2L))
    parS <- t(parS)
  }
  if (missing(weightsS)) {
    weightsS <- rep(1, nrow(parS))
  }
  if (NmaxControl == 0) {
    ret <- h_predprobdist_single_arm(
      x = x,
      mE = mE,
      n = n,
      Nmax = Nmax,
      delta = delta,
      relativeDelta = relativeDelta,
      parE = parE,
      parS = parS,
      weights = weights,
      weightsS = weightsS,
      thetaT = thetaT
    )
  } else {
    ret <- h_predprobdist(
      x = x,
      n = n,
      xS = xS,
      nS = nS,
      Nmax = Nmax,
      NmaxControl = NmaxControl,
      delta = delta,
      relativeDelta = relativeDelta,
      parE = parE,
      parS = parS,
      weights = weights,
      weightsS = weightsS,
      thetaT = thetaT
    )
  }
  ret
}
