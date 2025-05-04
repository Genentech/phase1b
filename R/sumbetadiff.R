##' Mode and Credible Interval Calculation for The Difference between Two Beta Distributions
##'
##' @description `r lifecycle::badge("experimental")`
##'
##' A function to summarize the characters of a betadiff distribution [dbetadiff()].
##' May require use of random sample generator to calculate, use [set.seed()] to reproduce results.
##'
##' @inheritParams dbetadiff
##' @inheritParams plotBetaDiff
##' @typed conf_level : numeric
##'   coverage for credible interval
##'
##' @return `sumbetadiff` gives the mode, credible interval of the distribution function,
##' along with the probabilities of above `go_cut` and below `stop_cut`.
##'
##' @importFrom stats optimize integrate
##'
##' @example examples/sumbetadiff.R
##' @export
sumBetaDiff <- function(parX, # Treatment group's parameters
                        parY, # Control group's parameters
                        coverage = 0.9,
                        go_cut,
                        stop_cut) {
  assert_numeric(parY, len = 2, lower = .Machine$double.xmin, any.missing = FALSE, finite = TRUE)
  assert_numeric(parX, len = 2, lower = .Machine$double.xmin, any.missing = FALSE, finite = TRUE)
  assert_number(coverage, finite = TRUE)
  assert_number(go_cut, finite = TRUE)
  assert_number(stop_cut, finite = TRUE)

  result <- try(
    {
      mode <- stats::optimize(
        f = dbetadiff,
        interval = c(-0.999, 0.999),
        parY = parY,
        parX = parX,
        maximum = TRUE
      )$maximum

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
      # Prob for Go:
      auc_go <- stats::integrate(
        f = dbetadiff,
        parX = parX,
        parY = parY,
        lower = go_cut,
        upper = 1,
        subdivisions = 1000L,
        rel.tol = .Machine$double.eps^0.1
      )$value

      # Prob for Stop:
      auc_stop <- stats::integrate(
        f = dbetadiff,
        parX = parX,
        parY = parY,
        lower = -1,
        upper = stop_cut,
        subdivisions = 1000L,
        rel.tol = .Machine$double.eps^0.1
      )$value

      list(
        mode = mode,
        ci = c(lower, upper),
        go = auc_go,
        stop = auc_stop
      )
    },
    silent = TRUE
  )
  ## if there were any errors, fall back to Monte Carlo estimation
  if (inherits(result, "try-error")) { # try-error is a class
    samples <- stats::rbeta(n = 2e6, parY[1], parY[2]) -
      rbeta(n = 2e6, parX[1], parX[2])

    lower <- stats::quantile(samples, prob = (1 - coverage) / 2)
    upper <- stats::quantile(samples, prob = (1 + coverage) / 2)

    auc_go <- mean(samples > go_cut)
    auc_stop <- mean(samples < stop_cut)

    samples <- cut(samples, breaks = seq(from = -1, to = 1, length = 801))
    samples <- table(samples)
    mode <- seq(from = -1, to = 1, by = 0.0025)[which.max(samples)]

    assert_number(mode, upper = 1, na.ok = FALSE)
    assert_number(lower, lower = -1, upper = 1, na.ok = FALSE)
    assert_number(upper, lower = 0, upper = 1, na.ok = FALSE)
    assert_number(auc_go, upper = 1, na.ok = FALSE)
    assert_number(auc_stop, upper = 1, na.ok = FALSE)

    result <- list(
      mode = mode,
      ci = c(lower, upper),
      go = auc_go,
      stop = auc_stop
    )
  }
  result
}
