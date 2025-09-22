#' Return summary statistics under one response scenario
#'
#' This function will calculate the summary statistics for a specific response
#' outcome scenario.
#'
#' @inheritParams postprob
#' @inheritParams sumBetaDiff
#' @typed round : number
#' Digit rounding of the output statistics
#'
#' @return A vector with the results.
#'
#' @importFrom phase1b sumBetaDiff
#'
#' @example examples/sumTable.R
#' @export
sumTable <- function(x,
                     n,
                     go_cut,
                     stop_cut,
                     parX,
                     parY = c(0.5, 0.5),
                     round = 2) {
  assert_numeric(x, lower = 0, upper = n, finite = TRUE)
  assert_number(n, lower = 0, finite = TRUE)
  assert_number(go_cut, finite = TRUE)
  assert_number(stop_cut, finite = TRUE)
  assert_numeric(parY, len = 2, lower = .Machine$double.xmin, any.missing = FALSE, finite = TRUE)
  assert_numeric(parX, len = 2, lower = .Machine$double.xmin, any.missing = FALSE, finite = TRUE)
  assert_count(round)

  tmp <- sumBetaDiff(
    parX = parX,
    parY =
      c(
        x + parY[1],
        n - x + parY[2]
      ),
    go_cut = go_cut,
    stop_cut = stop_cut
  )
  summaries <- round(c(
    x,
    x / n * 100,
    tmp$mode * 100,
    tmp$ci * 100,
    tmp$go * 100,
    tmp$stop * 100
  ), round)

  summaries <- as.data.frame(summaries)

  rownames(summaries) <- c(
    "responders",
    "obs ORR [%]",
    "mode [%]",
    "CI lower [%]",
    "CI upper [%]",
    "prob.go [%]",
    "prob.nogo [%]"
  )
  summaries
}
