##' Mode and Credible Interval Calculation for The Difference between Two Beta Distributions.
##'
##' @description `r lifecycle::badge("experimental")`
##'
##' A function to summarize the characters of a betadiff distribution `[dbetadiff]`.
##'
##' @inheritParams dbetadiff
##' @inheritParams plotBetaDiff
##' @typed level :
##'   coverage for credible interval
##'
##' @return `sumbetadiff` gives the mode, credible interval of the distribution function,
##' along with the probabilities of above `go_cut` and below `stop_cut`.
##'
##' @importFrom stats optimize integrate rbeta quantile.
##'
##' @example examples/sumbetadiff.R
##' @export
sumBetadiff <- function(parX, # Treatment group's parameters
                        parY,  # Control group's parameters
                        coverage = 0.9,
                        go_cut,
                        stop_cut) {
  assert_numeric(parY, len = 2, lower = .Machine$double.xmin, any.missing = FALSE, finite = TRUE)
  assert_numeric(parX, len = 2, lower = .Machine$double.xmin, any.missing = FALSE, finite = TRUE)
  assert_number(coverage, finite = TRUE)
  assert_number(go_cut, finite = TRUE)
  assert_number(stop_cut, finite = TRUE)

  res <- try(
    {
      mode <- stats::optimize(
        f = dbetadiff,
        interval = c(-0.999, 0.999),
        parY = parY,
        parX = parX,
        maximum = TRUE
      )$maximum

      assert_true(mode$maximum > 0)
      assert_true(lower > 0)
      assert_true(upper > 0)
      assert_true(Out.go > 0)
      assert_true(Out.stop > 0)

      lower <- qbetadiff( # to recover x when F(x) is at lower percentile
        p = (1 - coverage) / 2,
        parY = parY,
        parX = parX
      )

      upper <- qbetadiff( # to recover x when F(x) is at upper percentile
        p = (1 + coverage) / 2,
        parY = parY,
        parX = parX
      )

      ## for Go:
      Out.go <- stats::integrate(
        f = dbetadiff,
        parY = parY,
        parX = parX,
        lower = stop_cut,
        upper = 1,
        subdivisions = 1000L,
        rel.tol = .Machine$double.eps^0.1
      )$value

      ## for Stop:
      Out.stop <- stats::integrate(
        f = dbetadiff,
        parY = parY,
        parX = parX,
        lower = -1,
        upper = go_cut,
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
  if (inherits(res, "try-error")) { # try-error is a class
    samples <- stats::rbeta(n = 2e6, parY[1], parY[2]) -
      rbeta(n = 2e6, parX[1], parX[2])

    lower <- stats::quantile(samples, prob = (1 - level) / 2)
    upper <- stats::quantile(samples, prob = (1 + level) / 2)

    Out.go <- mean(samples > go_cut)
    Out.nogo <- mean(samples < stop_cut)

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
  assert_number(res$mode)
  assert_number(res$ci)
  assert_list(res)
  res
}
