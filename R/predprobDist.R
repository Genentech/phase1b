##' @include dbetabinom.R
##' @include postprobDist.R
{}

##' Compute the predictive probability that the trial will be
##' successful, with a prior distribution on the SOC
##'
##' Compute the predictive probability of trial success given current data.
##' Success means that at the end of the trial the posterior probability
##' Pr(P_E > P_S + delta_0 | data) >= thetaT. Then the
##' predictive probability for success is:
##' pp = sum over i: Pr(Y=i|x,n)*I{Pr(P_E > P_S + delta|x,Y=i)>=thetaT},
##' where Y is the number of future responses in the treatment group and x is
##' the current number of responses in the treatment group (out of n).
##' Prior is P_E ~ beta(a, b), default uniform which is a beta(1,1).
##' However, also a beta mixture prior can be specified. Analogously
##' for P_S either a classic beta prior or a beta mixture prior can be
##' specified.
##'
##' Also data on the SOC might be available. Then the predictive probability is
##' more generally defined as
##' pp = sum over i, j: Pr(Y=i|x,n)*Pr(Z=j|xS, nS)*I{Pr(P_E > P_S +
##' delta|x,xS,Y=i,Z=j)>=thetaT}
##' where Z is the future number of responses in the SOC group, and xS is the
##' current number of responses in the SOC group.
##'
##' A table with the following contents will be
##' included in the \code{tables} attribute of the return value
##' (in the case that NmaxControl is zero):
##' i: Y=i (number of future successes in Nmax-n subjects)
##' py: Pr(Y=i|x) using beta-(mixture)-binomial distribution
##' b: Pr(P_E > P_S + delta | x, Y=i)
##' bgttheta: indicator I(b>thetaT)
##'
##' If NmaxControl is not zero, i.e., when data on the control treatment
##' is available in this trial, then a list with will be included with the
##' following elements:
##' pyz: matrix with the probabilities Pr(Y=i, Z=j | x, xS)
##' b: matrix with Pr(P_E > P_S + delta | x, xS, Y=i, Z=j)
##' bgttheta: matrix of indicators I(b>thetaT)
##'
##' @param x number of successes (in the treatment group)
##' @param n number of patients (in the treatment group)
##' @param xS number of successes in the SOC group (default: 0)
##' @param nS number of patients in the SOC group (default: 0)
##' @param Nmax maximum number of patients at the end of the trial (in the
##' treatment group)
##' @param NmaxControl maximum number of patients at the end of the trial in the
##' SOC group (default: 0)
##' @param delta margin by which the response rate in the treatment group should
##' be better than in the SOC group (default: 0)
##' @param relativeDelta see \code{\link{postprobDist}}
##' @param parE the beta parameters matrix, with K rows and 2 columns,
##' corresponding to the beta parameters of the K components. default is a
##' uniform prior.
##' @param weights the mixture weights of the beta mixture prior. Default are
##' uniform weights across mixture components.
##' @param parS beta prior parameters in the SOC group (default: uniform)
##' @param weightsS weights for the SOC group (default: uniform)
##' @param thetaT threshold on the probability to be used
##' @return The predictive probability, a numeric value. In addition, a
##' list called \code{tables} is returned as attribute of the returned number.
##'
##' @references Lee, J. J., & Liu, D. D. (2008). A predictive probability
##' design for phase II cancer clinical trials. Clinical Trials, 5(2),
##' 93-106. doi:10.1177/1740774508089279
##'
##' @example examples/predprobDist.R
##' @export
predprobDist <- function(x, n,
                         xS = 0, nS = 0,
                         Nmax, NmaxControl = 0,
                         delta = 0,
                         relativeDelta = FALSE,
                         parE = c(a = 1, b = 1),
                         weights,
                         parS = c(a = 1, b = 1),
                         weightsS,
                         thetaT) {
  ## ensure reasonable numbers
  stopifnot(
    n <= Nmax,
    nS <= NmaxControl,
    x <= n,
    xS <= nS
  )

  ## remaining active patients to be seen:
  mE <- Nmax - n

  ## if par is a vector => situation where there is only one component
  if (is.vector(parE)) {
    ## check that it has exactly two entries
    stopifnot(identical(length(parE), 2L))

    ## and transpose to matrix with one row
    parE <- t(parE)
  }

  ## if prior weights of the beta mixture are not supplied
  if (missing(weights)) {
    weights <- rep(1, nrow(parE))
    ## (don't need to be normalized, this is done in getBetamixPost)
  }

  ## if parS is a vector => situation where there is only one component
  if (is.vector(parS)) {
    ## check that it has exactly two entries
    stopifnot(identical(length(parS), 2L))

    ## and transpose to matrix with one row
    parS <- t(parS)
  }

  ## if prior weights of the beta mixture are not supplied
  if (missing(weightsS)) {
    weightsS <- rep(1, nrow(parS))
  }

  ## now compute updated parameters for beta mixture distribution on the
  ## treatment proportion
  activeBetamixPost <- getBetamixPost(x = x, n = n, par = parE, weights = weights)

  ## now with the beta binomial mixture:
  py <- with(
    activeBetamixPost,
    dbetabinomMix(x = 0:mE, m = mE, par = par, weights = weights)
  )

  if (NmaxControl == 0) {
    ## here is the only difference to predprob.R:
    ## how to compute the posterior probabilities
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

    ret <- structure(sum(py * (b > thetaT)),
      tables =
        round(
          cbind(
            i = c(0:mE),
            py,
            b,
            bgttheta = (b > thetaT)
          ),
          4
        )
    )
  } else {

    ## in this case also data on the SOC is available!

    ## determine remaining sample size and probabilities of response
    ## counts in future SOC patients:
    mS <- NmaxControl - nS

    controlBetamixPost <- getBetamixPost(
      x = xS, n = nS, par = parS,
      weights = weightsS
    )

    pz <- with(
      controlBetamixPost,
      dbetabinomMix(x = 0:mS, m = mS, par = par, weights = weights)
    )

    ## determine resulting posterior probabilities:
    outcomesY <- x + c(0:mE)
    outcomesZ <- xS + c(0:mS)

    pyz <- b <- matrix(
      nrow = 1 + mE,
      ncol = 1 + mS,
      dimnames =
        list(
          0:mE,
          0:mS
        )
    )

    for (i in seq_along(outcomesY))
    {
      for (j in seq_along(outcomesZ))
      {
        ## calculate the posterior probability for this combination
        ## of counts
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

        ## what are the joint probabilities of active and control counts?
        ## => because they are independent, just multiply them
        pyz[i, j] <- py[i] * pz[j]
      }
    }

    ## should we print something?
    ret <- structure(sum(pyz * (b > thetaT)),
      tables =
        list(
          pyz = pyz,
          b = b,
          bgttheta = (b > thetaT)
        )
    )
  }

  return(ret)
}

## todo: predprobDistFail
