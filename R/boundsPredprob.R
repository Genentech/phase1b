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
#' @return A matrix for each same size in `looks`. For each sample size, the following is returned:
#' - `xL` : the maximum number of responses that meet the futility.
#'          threshold
#' - `pL` : response rate corresponding to `xL`.
#' - `predL` : predictive probability corresponding to `xL`
#' - `postL`: posterior probability corresponding to `xL`.
#' - `Ucil` : upper bound of one sided 95% CI for the response rate based on an
#'            exact binomial test.
#' - `xU` : the minimal number of responses that meet the efficacy threshold.
#' - `pU` : response rate corresponding to `xU`.
#' - `predU` : predictive probability corresponding to `xU`
#' - `postL`: posterior probability corresponding to `xU`.
#' - `LciU` : lower bound of one sided 95% CI for the response rate based on exact
#'            binomial test.
#'
#' @importFrom stats binom.test
#'
#' @example examples/boundsPredprob.R
#' @export
#' @keywords graphics
boundsPredprob <- function(looks, Nmax = max(looks), p0, tT, phiL, phiU, parE = c(1, 1), weights) {
  assert_numeric(looks)
  assert_number(p0, lower = 0, upper = 1)
  assert_number(tT, lower = 0, upper = 1)
  assert_numeric(parE, min.len = 2, any.missing = FALSE)
  znames <- c(
    "xL", "pL", "predL", "postL", "UciL",
    "xU", "pU", "predU", "postU", "LciU"
  )
  z <- matrix(NA, length(looks), length(znames))
  dimnames(z) <- list(looks, znames)
  k <- 0
  if (missing(weights)) {
    weights <- rep(1, nrow(parE))
  }
  assert_numeric(weights, min.len = 0, len = nrow(par), finite = TRUE)
  for (n in looks) {
    k <- k + 1
    # initialize so will return NA if 0 or n in "continue" region
    xL <- NA
    xU <- NA
    for (x in 0:n) {
      predprob <- predprob(x = x, n = n, Nmax = max(looks), p = p0, thetaT = tT, parE = parE, weights = weights)$result
      if (predprob <= phiL) { # Futility look, Rule Pr(Pr(P > p0 | x, Y, a, b) >= tT | x) =< phiL
        xL <- x
        predL <- predprob
      }
      if (predprob >= phiU) { # Efficacy look, Rule Pr(Pr(P > p0 | x, Y, a, b) >= tT | x) >= phiU,
        xU <- x
        predU <- predprob
        break
      }
    }
    # reset xU to NA if phiU = 1 and n < Nmax
    if (n < Nmax && phiU == 1) {
      xU <- NA
    }
    # calculate predictive and posterior probabilities at boundaries
    postL <- postprob(xL, n, p0, parE = parE)
    postU <- postprob(xU, n, p0, parE = parE)
    # calculate lower CI at boundaries
    UciL <- ifelse(!is.na(xL), stats::binom.test(xL, n, alt = "less")$conf.int[2], NA)
    LciU <- ifelse(!is.na(xU), stats::binom.test(xU, n, alt = "greater")$conf.int[1], NA)
    z[k, ] <- c(
      xL,
      xL / n,
      predL,
      postL,
      UciL,
      xU,
      xU / n,
      predU,
      postU,
      LciU
    )
  }
  return(round(data.frame(looks, z), 4))
}
