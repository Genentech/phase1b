#' Mode and Credible Interval Calculation for The Difference between Two Beta Distributions
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A function to summarize the characters of a betadiff distribution [dbetadiff()].
#' May require use of random sample generator to calculate, use [set.seed()] to reproduce results.
#'
#' @inheritParams dbetadiff
#' @inheritParams plotBetaDiff
#' @typed ci_level : numeric
#'   level for credible interval
#'
#' @return List with the mode, credible interval for the difference,
#' along with the `go` and `stop` probabilities.
#'
#' @importFrom stats optimize integrate
#'
#' @example examples/sumBetaDiff.R
#' @export
sumBetaDiff <- function(parX, # Treatment group's parameters
                        parY, # Control group's parameters
                        ci_level = 0.9,
                        go_cut, # a meaningful improvement threshold
                        stop_cut) { # a poor improvement threshold
  assert_numeric(parY, len = 2, lower = .Machine$double.xmin, any.missing = FALSE, finite = TRUE)
  assert_numeric(parX, len = 2, lower = .Machine$double.xmin, any.missing = FALSE, finite = TRUE)
  assert_number(ci_level, finite = TRUE)
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
        p = (1 - ci_level) / 2,
        parY = parY,
        parX = parX
      )

      upper <- qbetadiff( # to recover x when F(x) is at upper percentile
        p = (1 + ci_level) / 2,
        parY = parY,
        parX = parX
      )

      # Prob for Go:
      prob_go <- stats::integrate(
        f = dbetadiff,
        parX = parX,
        parY = parY,
        lower = go_cut,
        upper = 1,
        subdivisions = 1000L,
        rel.tol = .Machine$double.eps^0.1
      )$value

      # Prob for Stop:
      prob_stop <- stats::integrate(
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
        go = prob_go,
        stop = prob_stop
      )
    },
    silent = TRUE
  )
  # if there were any errors, fall back to Monte Carlo estimation
  if (inherits(result, "try-error")) { # try-error is a class
    samples <- stats::rbeta(n = 2e6, parY[1], parY[2]) -
      rbeta(n = 2e6, parX[1], parX[2])

    lower <- stats::quantile(samples, prob = (1 - ci_level) / 2)
    upper <- stats::quantile(samples, prob = (1 + ci_level) / 2)

    prob_go <- mean(samples > go_cut)
    prob_stop <- mean(samples < stop_cut)

    samples <- cut(samples, breaks = seq(from = -1, to = 1, length = 801))
    samples <- table(samples)
    mode <- seq(from = -1, to = 1, by = 0.0025)[which.max(samples)]

    result <- list(
      mode = mode,
      ci = c(lower, upper),
      go = prob_go,
      stop = prob_stop
    )
  }
  result
}
