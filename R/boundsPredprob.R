##' @include predprob.R
##' @include postprob.R

##' Decision cutpoints for boundary (based on predictive probability)
##'
##' This function is used to identify the efficacy boundary and futility
##' boundary based on predictive probabilities, i.e.:
##' Efficacy boundary: find minimum x (xU) where
##' Pr(Pr(P > p | x, Y) >= tT | x) > phiU,
##' Futility boundary: find maximum x (xL) where
##' Pr(Pr(P > p | x, Y) >= tT | x) < phiL
##'
##' @param nvec  a vector of number of patients
##' @param Nmax  maximum number of patients at the end of the trial
##' (default: maximum of \code{nvec})
##' @param p  threshold on the response rate
##' @param tT  threshold on the posterior probability to be above p
##' @param phiL  futility boundary predictive probability threshold
##' @param phiU  efficacy boundary predictive probability threshold
##' @param a  the alpha parameter of a beta prior of treatment group
##' @param b  the beta parameter of a beta prior of treatment group
##' @return A matrix where for each sample size in \code{nvec}, this function
##' returns the maximum number of responses that meet the futility
##' threshold (xL), its corresponding response rate (pL), predictive probability
##' (ppL) and posterior probability (postL), the upper bound of one
##' sided 95% CI for the response rate based on an
##' exact binomial test (UciL), and the same boundary parameters for efficacy:
##' the minimal number of responses that meet the efficacy threshold (xU),
##' the corresponding response rate (pU), predictive probability
##' (ppL) and posterior probability (postU), the lower bound of one sided
##' 95% CI for the response rate based on exact binomial test (LciU).
##'
##' @importFrom stats binom.test
##'
##' @example examples/boundsPredprob.R
##' @export
##' @keywords graphics
boundsPredprob <- function(nvec, Nmax = max(nvec), p, tT, phiL, phiU, a, b) {
  znames <- c(
    "xL", "pL", "ppL", "postL", "UciL",
    "xU", "pU", "ppU", "postU", "LciU"
  )
  z <- matrix(NA, length(nvec), length(znames))
  dimnames(z) <- list(nvec, znames)
  k <- 0
  for (n in nvec) {
    k <- k + 1
    # initialize so will return NA if 0 or n in "continue" region
    xL <- NA
    xU <- NA
    for (x in 0:n) {
      pp <- predprob(x, n, Nmax, p, tT, parE = c(a, b))
      if (pp <= phiL) {
        xL <- x
      }
      if (pp >= phiU) {
        xU <- x
        # done: leave innermost for loop
        break
      }
    }
    # reset xU to NA if phiU=1 and n<Nmax
    if (n < Nmax && phiU == 1) {
      xU <- NA
    }
    # calculate predictive and posterior probabilities at boundaries
    ppL <- predprob(xL, n, Nmax, p, tT, parE = c(a, b))
    ppU <- predprob(xU, n, Nmax, p, tT, parE = c(a, b))
    postL <- postprob(xL, n, p, parE = c(a, b))
    postU <- postprob(xU, n, p, parE = c(a, b))
    # calculate lower CI at boundaries
    UciL <- ifelse(!is.na(xL), stats::binom.test(xL, n, alt = "less")$conf.int[2], NA)
    LciU <- ifelse(!is.na(xU), stats::binom.test(xU, n, alt = "greater")$conf.int[1], NA)
    z[k, ] <- c(xL, xL / n, ppL, postL, UciL, xU, xU / n, ppU, postU, LciU)
  }
  return(round(data.frame(nvec, z), 4))
}
