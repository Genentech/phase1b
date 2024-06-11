#' Decision cutpoints for boundary (based on predictive probability) for Decision 1 rule.
#'
#' This function is used to identify the efficacy boundary and futility
#' boundary based on the following rules:
#' Efficacy boundary: find minimum x (xU) where
#' Pr(Pr(P > p0 | x, Y, a, b) >= tT | x) >= phiU,
#' Futility boundary: find maximum x (xL) where
#' Pr(Pr(P > p0 | x, Y, a, b) >= tT | x) =< phiL
#'
#' @inheritParams predprob
#' @inheritParams ocPredprob
#' @inheritParams boundsPostprob
#' @return A matrix for each same size in `nvec`. For each sample size, the following is returned:
#' - `xL` : the maximum number of responses that meet the futility.
#'          threshold
#' - `pL` : response rate corresponding to `xL`.
#' - `ppL` : predictive probability corresponding to `xL`
#' - `postL`: posterior probability corresponding to `xL`.
#' - `Ucil` : upper bound of one sided 95% CI for the response rate based on an
#'            exact binomial test.
#' - `xU` : the minimal number of responses that meet the efficacy threshold.
#' - `pU` : response rate corresponding to `xU`.
#' - `postL`: posterior probability corresponding to `xU`.
#' - `ppU` : predictive probability corresponding to `xU`
#' - `LciU` : lower bound of one sided 95% CI for the response rate based on exact
#'            binomial test.
#'
#' @importFrom stats binom.test
#'
#' @example examples/boundsPredprob.R
#' @export
#' @keywords graphics
boundsPredprob <- function(nvec, Nmax = max(nvec), p0, tT, phiL, phiU, a, b) {
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
      pp <- predprob(x, n, Nmax, p0, tT, parE = c(a, b))$result
      if (pp >= phiU) { # Efficacy look Pr(Pr(P > p0 | x, Y, a, b) >= tT | x) >= phiU,
        xU <- x
        ppU <- ppL
      }
      predprob(x, n, Nmax, p0, tT, parE = c(a, b))$result
      if (pp <= phiL) { # Futility look Pr(Pr(P > p0 | x, Y, a, b) >= tT | x) =< phiL
        xL <- x
        ppL <- pp
        # done: leave innermost for loop
        break
      }
    }
    # reset xU to NA if phiU = 1 and n < Nmax
    if (n < Nmax && phiU == 1) {
      xU <- NA
    }
    # calculate predictive and posterior probabilities at boundaries
    ppL <- predprob(xL, n, Nmax, p0, tT, parE = c(a, b))$result
    ppU <- predprob(xU, n, Nmax, p0, tT, parE = c(a, b))$result
    postL <- postprob(xL, n, p0, parE = c(a, b))
    postU <- postprob(xU, n, p0, parE = c(a, b))
    # calculate lower CI at boundaries
    UciL <- ifelse(!is.na(xL), stats::binom.test(xL, n, alt = "less")$conf.int[2], NA)
    LciU <- ifelse(!is.na(xU), stats::binom.test(xU, n, alt = "greater")$conf.int[1], NA)
    z[k, ] <- c(
      xL,
      xL / n,
      ppL,
      postL,
      UciL,
      xU,
      xU / n,
      ppU,
      postU,
      LciU
    )
  }
  return(round(data.frame(nvec, z), 4))
}
