#' @include dbetabinom.R
#' @include postprobDist.R
NULL

#' The predictive probability of success in single arm studies.
#'
#' The helper function to generate the predictive probability of success based only on treatment group (`E`)
#' as there is no control or standard of care (`S`)., indicated by `NmaxControl == 0`.
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
#'  response rate in magnitude of `delta` of the `S` non-responding patients. See `[(postprobDist)]`.
#' @typed parE : numeric
#'  parameters for beta distribution. If it is a matrix, it needs to have 2 columns,
#'  and each row corresponds to each component of a beta-mixture distribution
#'  for the `E` group. See details.
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
#' @keywords internal
#'
#' @return A `list` is returned with names `result` for predictive probability and
#'  `table` of numeric values with counts of responses in the remaining patients,
#'  probabilities of these counts, corresponding probabilities to be above threshold,
#'  and trial success indicators.
#'
h_predprobdist_single_arm <- function(x,
                                      mE,
                                      Nmax,
                                      delta,
                                      relativeDelta,
                                      parE,
                                      parS,
                                      weights,
                                      weightsS,
                                      thetaT,
                                      density) {
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
  assert_numeric(posterior_y, lower = 0, upper = 1, finite = TRUE, any.missing = FALSE)
  assert_numeric(density, lower = 0, upper = 1, finite = TRUE, any.missing = FALSE)
  list(
    result = sum(density * (posterior_y > thetaT)),
    table = data.frame(
      counts = c(0:mE),
      cumul_counts = x + (0:mE),
      density = density,
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
#' @typed n : number
#'  number of patients in the `E` group at interim.
#' @typed x : number
#'  number of successes in the `E` group at interim.
#' @typed xS : number
#'  number of successes in the `S` group at interim.
#' @typed Nmax : number
#'   maximum number of patients in the `E` group at final analysis.
#' @typed NmaxControl : number
#'   maximum number of patients in the `S` group at final analysis.
#' @inheritParams h_predprobdist_single_arm
#'
#' @keywords internal
#'
#' @return A `list` is returned with names `result` for predictive probability and
#'  `table` of numeric values with counts of responses in the remaining patients, `density` for
#'  probabilities of these counts, `posterior` for corresponding probabilities to be above threshold,
#'  and `success`for trial success indicators.
#'
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
    dbetabinomMix(x = 0:mS, m = mS, par = parS, weights = weights)
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
#' successful, with a prior distribution on the SOC
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Compute the predictive probability of trial success given current data.
#' Success means that at the end of the trial the posterior probability
#' `Pr(P_E > P_S + delta_0 | data) >= thetaT`. Then the
#' predictive probability for success is:
#' `pp = sum over i: Pr(Y = i | x , n)*I{Pr(P_E > P_S + delta | x,Y=i) >= thetaT}`,
#' where `Y` is the number of future responses in the treatment group and `x` is
#' the current number of responses in the treatment group out of `n`.
#' Prior is `P_E ~ beta(a, b)` and uniform which is a beta(1,1).
#' However, also a beta mixture prior can be specified. Analogously
#' for `P_S` either a classic beta prior or a beta mixture prior can be
#' specified.
#'
#' Also data on the SOC might be available. Then the predictive probability is
#' more generally defined as
#' `pp = sum over i, j: Pr(Y = i | x, n)*Pr(Z = j | xS, nS )*I{Pr(P_E > P_S + delta | x,xS, Y=i, Z=j ) >= thetaT}`
#' where `Z` is the future number of responses in the SOC group, and `xS` is the
#' current number of responses in the SOC group.
#'
#' In the case where `NmaxControl = 0`, a table with the following contents will be included in the return output :
#' - `i`: `Y = i`, number of future successes in `Nmax-n` subjects.
#' - `density`: `Pr(Y = i|x)` using beta-(mixture)-binomial distribution.
#' - `posterior`: `Pr(P_E > P_S + delta | x, Y = i)` using beta posterior.
#' - `success`: indicator `I(b>thetaT)`.
#'
#' A table with the following contents will be
#' included in the  return value
#' (in the case that NmaxControl is zero):
#' i: Y=i (number of future successes in Nmax-n subjects)
#' py: Pr(Y=i|x) using beta-(mixture)-binomial distribution
#' b: Pr(P_E > P_S + delta | x, Y=i)
#' bgttheta: indicator I(b>thetaT)
#'
#' If `NmaxControl` is not zero, i.e., when data on the control treatment
#' is available in this trial, then a list with will be included with the
#' following elements:
#' pyz: matrix with the probabilities Pr(Y=i, Z=j | x, xS)
#' b: matrix with Pr(P_E > P_S + delta | x, xS, Y=i, Z=j)
#' bgttheta: matrix of indicators I(b>thetaT)
#'
#' @details
#'
#' # Delta from postprobDist :
#'
#' The desired improvement is denoted as `delta`. There are two options in using `delta`.
#' The absolute case when `relativeDelta = FALSE` and relative as when `relativeDelta = TRUE`.
#'
#' 1. The absolute case is when we define an absolute delta, greater than `P_S`,
#' the response rate of the `SOC` group such that
#' the posterior is `Pr(P_E > P_S + delta | data)`.
#'
#' 2. In the relative case, we suppose that the treatment group's
#' response rate is assumed to be greater than `P_S + (1-P_S) * delta` such that
#' the posterior is `Pr(P_E > P_S + (1 - P_S) * delta | data)`.
#'
#' @typed x : number
#' number of successes in the treatment group at interim
#' @typed n : number
#' number of patients in the treatment group at interim
#' @typed xS : number
#' number of successes in the SOC group at interim
#' @typed nS : number
#' number of patients in the SOC group
#' @typed Nmax : number
#' maximum number of patients in the treatment group at the end of the trial
#' @typed NmaxControl : number
#' maximum number of patients at the end of the trial in the
#' SOC group
#' @typed delta :
#' margin by which the response rate in the treatment group should
#' be better than in the SOC group
#' @typed relativeDelta :
#' see `[postprobDist()]`
#' @typed parE :
#' the beta parameters matrix, with K rows and 2 columns,
#' corresponding to the beta parameters of the K components. default is a
#' uniform prior.
#' @typed weights :
#' the mixture weights of the beta mixture prior. Default are
#' uniform weights across mixture components.
#' @typed parS :
#' beta prior parameters in the SOC group
#' @typed weightsS :
#' weights for the SOC group
#' @typed thetaT :
#' threshold on the probability to be used
#' @return A `list` is returned with names `result` for predictive probability and
#'  `table` (single arm case)  or list `tables` with ... (to be completed)
#'
#' @references Lee, J. J., & Liu, D. D. (2008). A predictive probability
#' design for phase II cancer clinical trials. Clinical Trials, 5(2),
#' 93-106. doi:10.1177/1740774508089279
#'
#' @example examples/predprobDist.R
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
  # remaining active patients to be seen:
  mE <- Nmax - n
  # if par is a vector => situation where there is only one component
  if (is.vector(parE)) {
    # check that it has exactly two entries
    stopifnot(identical(length(parE), 2L))
    # and transpose to matrix with one row
    parE <- t(parE)
  }
  # if prior weights of the beta mixture are not supplied
  if (missing(weights)) {
    weights <- rep(1, nrow(parE))
    # (don't need to be normalized, this is done in h_getBetamixPost)
  }
  # if parS is a vector => situation where there is only one component
  if (is.vector(parS)) {
    # check that it has exactly two entries
    stopifnot(identical(length(parS), 2L))
    # and transpose to matrix with one row
    parS <- t(parS)
  }
  # if prior weights of the beta mixture are not supplied
  if (missing(weightsS)) {
    weightsS <- rep(1, nrow(parS))
  }
  # now compute updated parameters for beta mixture distribution on the
  # treatment proportion
  activeBetamixPost <- h_getBetamixPost(x = x, n = n, par = parE, weights = weights)
  # now with the beta binomial mixture:
  py <- with(
    activeBetamixPost,
    dbetabinomMix(x = 0:mE, m = mE, par = par, weights = weights)
  )
  if (NmaxControl == 0) {
    # here is the only difference to predprob.R:
    # how to compute the posterior probabilities
    b <- postprobDist(
      x = x + c(0:mE),
      n = Nmax,
      delta = delta,
      relativeDelta = relativeDelta,
      parE = parE,
      weights = weights,
      parS = parS,
      weightsS = weightsS
    )
    ret <- list(
      result = sum(py * (b > thetaT)),
      table = data.frame(
        counts = c(0:mE),
        density = py,
        posterior = b,
        success = (b > thetaT)
      )
    )
  } else {
    # in this case also data on the SOC is available!
    # determine remaining sample size and probabilities of response
    # counts in future SOC patients:
    mS <- NmaxControl - nS
    controlBetamixPost <- h_getBetamixPost(x = xS, n = nS, par = parS, weights = weightsS)
    pz <- with(
      controlBetamixPost,
      dbetabinomMix(x = 0:mS, m = mS, par = par, weights = weights)
    )
    # determine resulting posterior probabilities:
    outcomesY <- x + c(0:mE)
    outcomesZ <- xS + c(0:mS) # 15 more chances to get success counts
    pyz <- b <- matrix(
      nrow = 1 + mE,
      ncol = 1 + mS,
      dimnames =
        list(
          0:mE,
          0:mS
        )
    )
    for (i in seq_along(outcomesY)) { # outside?
      for (j in seq_along(outcomesZ)) {
        # calculate the posterior probability for this combination
        # of counts
        b[i, j] <-
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
        # what are the joint probabilitiesD of active and control counts?
        # => because they are independent, just multiply them
        pyz[i, j] <- py[i] * pz[j] # this is the matrix that Daniel was talking about
      }
    }
    # should we print something? predprob part
    ret <- list(
      result = sum(pyz * (b > thetaT)),
      tables = list(
        pyz = pyz,
        b = b,
        success = (b > thetaT)
      )
    )
  }
  ret
}
