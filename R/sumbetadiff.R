##' @include betadiff.R
NULL

##' Mode and Credible Interval Calculation for The Difference between Two Beta Distributions
##'
##'
##' A function to summerize the characters of a betadiff distribution \code{\link{betadiff}}
##'
##'
##' @param parX two parameters of X's beta distribution (Control)
##' @param parY two parameters of Y's beta distribution (Treatment)
##' @param level credible interval
##' @param cutB  a cutoff value for the upper bound
##' @param cutW  a cutoff value for the lower bound
##'
##' @return sumbetadiff gives the mode, credible interval of the distribution function,
##' along with the probabilities of above cutB and below cutW.
##'
##' @importFrom stats optimize integrate rbeta quantile
##'
##' @example examples/sumbetadiff.R
##' @export
sumBetadiff <- function(parY, parX,
                        level = 0.9, ## CI level
                        cutB,
                        cutW) {
  res <- try(
    {
      mode <- stats::optimize(
        f = dbetadiff,
        interval = c(-0.999, 0.999),
        parY = parY,
        parX = parX,
        maximum = TRUE
      )$maximum

      lower <- qbetadiff(
        p = (1 - level) / 2,
        parY = parY,
        parX = parX
      )

      upper <- qbetadiff(
        p = (1 + level) / 2,
        parY = parY,
        parX = parX
      )

      ## for Go:
      Out.go <- stats::integrate(
        f = dbetadiff,
        parY = parY,
        parX = parX,
        lower = cutB,
        upper = 1,
        subdivisions = 1000L,
        rel.tol = .Machine$double.eps^0.1
      )$value

      ## for Stop:
      Out.nogo <- stats::integrate(
        f = dbetadiff,
        parY = parY,
        parX = parX,
        lower = -1,
        upper = cutW,
        subdivisions = 1000L,
        rel.tol = .Machine$double.eps^0.1
      )$value

      list(
        mode = mode,
        ci = c(lower, upper),
        go = Out.go,
        nogo = Out.nogo
      )
    },
    silent = TRUE
  )

  ## if there were any errors, fall back to Monte Carlo estimation
  if (inherits(res, "try-error")) {
    samples <- stats::rbeta(n = 2e6, parY[1], parY[2]) -
      rbeta(n = 2e6, parX[1], parX[2])

    lower <- stats::quantile(samples, prob = (1 - level) / 2)
    upper <- stats::quantile(samples, prob = (1 + level) / 2)

    Out.go <- mean(samples > cutB)
    Out.nogo <- mean(samples < cutW)

    samples <- cut(samples, breaks = seq(from = -1, to = 1, length = 801))
    samples <- table(samples)
    mode <- seq(from = -1, to = 1, by = 0.0025)[which.max(samples)]

    res <- list(
      mode = mode,
      ci = c(lower, upper),
      go = Out.go,
      nogo = Out.nogo
    )
  }

  return(res)
}
