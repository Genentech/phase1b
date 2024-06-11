#' Decision cutpoints for boundary (based on posterior probability)
#'
#' This function is used to identify the efficacy and futility
#' boundaries based on the following rules:
#' Efficacy boundary: find minimum x (xU) where Pr(P > p1 |x, n, a, b) >= tU and
#' Futility boundary: find maximum x (xL) where Pr(P < p0 | x, n, a, b) >= tL
#'
#' @inheritParams postprob
#' @inheritParams ocPostprob
#' @typed nvec : numeric
#'  A vector of number of patients in each look.
#' @return A matrix for each same size in `nvec`. For each sample size, the following is returned:
#' - `xL` : the maximum number of responses that meet the futility.
#'          threshold
#' - `pL` : response rate corresponding to `xL`.
#' - `postL`: posterior probability corresponding to `xL`.
#' - `pL_upper_ci` : upper bound of one sided 95% CI for the response rate `pL` based on an
#'            exact binomial test.
#' - `xU` : the minimal number of responses that meet the efficacy threshold.
#' - `pU` : response rate corresponding to `xU`.
#' - `postU` : posterior probability corresponding to `xU`.
#' - `pU_lower_ci` : lower bound of one sided 95% CI for the response rate `pU` based on exact
#'            binomial test.
#'
#' @example examples/boundsPostprob.R
#' @export
boundsPostprob <- function(nvec, p0, p1 = p0, tL, tU, a, b) {
  z <- matrix(NA, nrow = length(nvec), ncol = 8)
  # dimnames(z) <- list(nvec, c(
  #   "xL", "pL", "postL",
  #   "xU", "pU", "postU"
  # ))
  znames <- c(
    "xL", "pL", "postL", "pL_upper_ci",
    "xU", "pU", "postU", "pU_lower_ci"
  )
  dimnames(z) <- list(nvec, znames)
  k <- 0
  for (n in nvec) {
    k <- k + 1
    # initialize so will return NA if 0 or n in "continue" region
    xL <- NA
    xU <- NA
    for (x in 0:n) {
      postp <- postprob(x, n, p0, parE = c(a, b)) # futility look
      if (postp >= tL) { # Rule is P(RR < p0) > tL
        postL <- postp
        xL <- x
      }
      postp <- 1 - postprob(x, n, p1, parE = c(a, b)) # efficacy look
      if (postp >= tU) { # Rule is P(RR > p1) > tU
        postU <- postp
        xU <- x
        break
      }
    }
    # calculate lower CI at boundaries
    pL_upper_ci <- ifelse(!is.na(xL), stats::binom.test(xL, n, alt = "less")$conf.int[2], NA)
    pU_lower_ci <- ifelse(!is.na(xU), stats::binom.test(xU, n, alt = "greater")$conf.int[1], NA)
    z[k, ] <- c(
      xL,
      xL / n,
      postL,
      pL_upper_ci,
      xU,
      xU / n,
      postU,
      pU_lower_ci
    )
    break
  }
  round(data.frame(nvec, z), 4)
}
