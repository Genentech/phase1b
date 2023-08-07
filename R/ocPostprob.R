##' @include postprob.R
NULL
##' Calculate operating characteristics for posterior probability method
##'
##' The trial is stopped for efficacy if the posterior probability to be
##' above p1 is larger than tU, and stopped for futility if the posterior
##' probability to be below p0 is larger than tL.
##'
##' Returned operating characteristics in a matrix include:
##' ExpectedN: expected number of patients in the trials
##' PrStopEarly: probability to stop the trial early (before reaching the
##' maximum sample size)
##' PrEarlyEff: probability to decide for efficacy early
##' PrEarlyFut: probability to decide for futility early
##' PrEfficacy: probability to decide for efficacy
##' PrFutility: probability to decide for futility
##' PrGrayZone: probability of no decision at the end ("gray zone")
##'
##' @param nn vector of look locations for efficacy
##' (if futility looks should be different, please specify also \code{nnF})
##' @param p true rate (scenario)
##' @param p0 lower efficacy threshold
##' @param p1 upper efficacy threshold
##' @param tL probability threshold for being below p0
##' @param tU probability threshold for being above p1
##' @param parE beta parameters for the prior on the treatment proportion
##' @param ns number of simulations
##' @param nr generate random look locations? (not default)
##' @param d distance for random looks around the look locations in \code{nn}
##' @param nnF vector of look locations for futility
##' (default: same as efficacy)
##' @return A list with the following elements:
##' oc: matrix with operating characteristics (see Details section)
##' Decision: vector of the decisions made in the simulated trials
##' (\code{TRUE} for success, \code{FALSE} for failure, \code{NA} for no
##' decision)
##' SampleSize: vector of the sample sizes in the simulated trials
##' nn: vector of look locations that was supplied
##' nnE: vector of efficacy look locations
##' nnF: vector of futility look locations
##' params: multiple parameters
##'
##' @example examples/ocPostprob.R
##' @export
ocPostprob <- function(nn, p, p0, p1, tL, tU, parE = c(1, 1),
                       ns = 10000, nr = FALSE, d = NULL, nnF = nn) {
  # Calculate operating characteristics via simulation
  # nn: vector of look locations
  # s: decision reject H0 (T) or fail to reject (F)
  #    during trial if continuing (NA)

  ## copy nn to nnE:
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
        qL <- 1 - postprob(x = sum(x[1:i]), n = i, p = p0, parE = parE)
        s[k] <- ifelse(qL >= tL, FALSE, NA)
      }

      if (i %in% nnrE) {
        qU <- postprob(x = sum(x[1:i]), n = i, p = p1, parE = parE)
        s[k] <- ifelse(qU < tU, s[k], TRUE)
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
