#' Decision cutpoints for boundary (based on posterior probability)
#'
#' This function is used to identify the efficacy and futility
#' boundaries based on posterior probabilities, i.e.:
#' Efficacy boundary: find minimum x (xU) where Pr(P>p0|x,n,a,b) >= tU and
#' Futility boundary: find maximum x (xL) where Pr(P>p1|x,n,a,b) <= tL
#'
#' @param nvec a vector of number of patients
#' @param p0 the efficacy threshold parameter in the postprob function
#' @param p1 the futility threshold parameter in the postprob function
#' (default = p0)
#' @param tL futility boundary probability threshold
#' @param tU efficacy boundary probability threshold
#' @param a  the alpha parameter of the beta prior of treatment group
#' @param b  the beta parameter of the beta prior of treatment group
#' @return A matrix where for each sample size in \code{nvec}, this function
#' returns the maximum number of responses that meet the futility
#' threshold (xL), its corresponding response rate (pL), posterior probability
#' (postL), upper bound of one sided 95% CI for the response rate based on an
#' exact binomial test (UciL), and the same boundary parameters for efficacy:
#' the minimal number of responses that meet the efficacy threshold (xU),
#' the corresponding response rate (pU), posterior probability (postU) and
#' the lower bound of one sided 95% CI for the response rate based on exact
#' binomial test (LciU).
#'
#' @importFrom stats binom.test
#'
#' @example examples/boundsPostprob.R
#' @export
#' @keywords graphics
boundsPostprob <- function(nvec, p0, p1 = p0, tL, tU, a, b) {
  z <- matrix(NA, length(nvec), 6)
  dimnames(z) <- list(nvec, c(
    "xL", "pL", "postL",
    "xU", "pU", "postU"
  ))
  znames <- c(
    "xL", "pL", "postL", "UciL",
    "xU", "pU", "postU", "LciU"
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
      postp <- postprob(x, n, p1, parE = c(a, b))
      if (postp <= tL) {
        xL <- x
      }
      if (p0 != p1) {
        postp <- postprob(x, n, p0, parE = c(a, b))
      }
      if (postp >= tU) {
        xU <- x
        # done: leave innermost for loop
        break
      }
    }
    # calculate posterior probabilities at boundaries
    postL <- postprob(xL, n, p1, parE = c(a, b))
    postU <- postprob(xU, n, p0, parE = c(a, b))
    # calculate lower CI at boundaries
    UciL <- ifelse(!is.na(xL), stats::binom.test(xL, n, alt = "less")$conf.int[2], NA)
    LciU <- ifelse(!is.na(xU), stats::binom.test(xU, n, alt = "greater")$conf.int[1], NA)
    z[k, ] <- c(xL, xL / n, postL, UciL, xU, xU / n, postU, LciU)
  }
  return(round(data.frame(nvec, z), 4))
}
