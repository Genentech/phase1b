#' Decision cutpoints for boundary (based on posterior probability)
#'
#' This function is used to identify the efficacy and futility
#' boundaries based on the following rules:
#' Efficacy boundary: find minimum x (xU) where Pr(RR > p1 |x, n, a, b) >= tU and
#' Futility boundary: find maximum x (xL) where Pr(RR < p0 | x, n, a, b) >= tL
#'
#' @inheritParams postprob
#' @inheritParams ocPostprob
#' @typed looks : numeric
#'  A vector of number of patients in each look.
#' @return A matrix for each same size in `looks`. For each sample size, the following is returned:
#' - `xL` : the maximum number of responses that meet the futility threshold.
#' - `pL` : response rate corresponding to `xL`.
#' - `postL`: posterior probability corresponding to `xL`, i.e. Pr(RR < p0 | xL, n, a, b).
#' - `pL_upper_ci` : upper bound of one sided 95% CI for the response rate `pL` based on an
#'            exact binomial test.
#' - `xU` : the minimal number of responses that meet the efficacy threshold.
#' - `pU` : response rate corresponding to `xU`.
#' - `postU` : posterior probability corresponding to `xU`, i.e. Pr(RR > p1 |xU, n, a, b).
#' - `pU_lower_ci` : lower bound of one sided 95% CI for the response rate `pU` based on exact
#'            binomial test.
#'
#' @example examples/boundsPostprob.R
#' @export
boundsPostprob <- function(looks, p0, p1 = p0, tL, tU, parE = c(1, 1), weights) {
  assert_numeric(looks)
  assert_number(p0, lower = 0, upper = 1)
  assert_number(p1, lower = 0, upper = 1)
  assert_number(tL, lower = 0, upper = 1)
  assert_number(tU, lower = 0, upper = 1)
  assert_numeric(parE, min.len = 2, any.missing = FALSE)
  z <- matrix(NA, nrow = length(looks), ncol = 8)
  znames <- c(
    "xL", "pL", "postL", "pL_upper_ci",
    "xU", "pU", "postU", "pU_lower_ci"
  )
  dimnames(z) <- list(looks, znames)
  k <- 0
  parE <- t(parE)
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
      postp_fut <- 1 - postprob(x = x, n = x, p = p0, parE = parE, weights = weights) # futility look
      if (postp_fut >= tL) { # Rule is P(RR < p0) > tL
        postL <- postp_fut
        xL <- x
      }
      postp_eff <- postprob(x = x, n = n, p1 = p1, parE = parE, weights = weights) # efficacy look
      if (postp_eff >= tU) { # Rule is P(RR > p1) > tU
        postU <- postp_eff
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
  }
  round(data.frame(looks, z), 4)
}
