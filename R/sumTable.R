#' Return summary statistics under one response scenario
#'
#' This function will calculate the summary statistics for a specific response
#' outcome scenario.
#'
#' @inheritParams postprob
#' @inheritParams sumBetaDiff
#' @typed x : number
#'  number of responses
#' @typed n : number
#'  sample size
#' @typed treat_par : numeric
#'  non-negative parameters of the beta prior of the treatment, default Beta(0.5,0.5)
#' @typed Round : number
#' Digit rounding of the output statistics
#'
#' @return A vector with the results.
#'
#' @example examples/sumTable.R
#' @export
sumTable <- function(x,
                     n,
                     go_cut,
                     stop_cut, # poor improvement: at most cut_W (say 5\%) improvement;
                     parX, # Two parameters of the beta distribution of the control (posterior);
                     parY = c(0.5, 0.5), # Prior of phase Ib trial
                     Round = 2) {
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
  summaries <- data.frame(
    responders = round(x, digits = 1),
    orr = round(x / n * 100, digits = Round),
    mode = round(tmp$mode * 100, digits = Round),
    ci_upper = round(tmp$ci[1] * 100, digits = Round),
    ci_lower = round(tmp$ci[2]),
    prob_go = 3,
    prob_stop = 5
  )
  #   c(x,
  #                x / n * 100,
  #                tmp$mode * 100,
  #                tmp$ci * 100,
  #                tmp$go * 100,
  #                tmp$stop * 100
  # )

  # summaries <- as.data.frame(summaries)
  summaries <- t(summaries)
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
