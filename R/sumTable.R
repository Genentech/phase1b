#' Return summary statistics under one response scenario
#'
#' This function will calculate the summary statistics for a specific response
#' outcome scenario.
#'
#' @inheritParams sumBetaDiff
#' @typed thisResp : number
#'  number of responses
#' @typed TotalSample : number
#'  sample size
#' @typed YPri : numeric
#'  non-negative parameters of the beta prior of the treatment, default Beta(0.5,0.5)
#' @typed Round : number
#' Rounding of the output statistics
#'
#' @return A vector with the results.
#'
#' @example examples/sumTable.R
#' @export
sumTable <- function(thisResp, # number of responses;
                     TotalSample, # Sample size;
                     go_cut, # meaningful improvement: at least cut_B (say 15\%) improvement;
                     stop_cut, # poor improvement: at most cut_W (say 5\%) improvement;
                     parX, # Two typedeters of the beta distribution of the control (posterior);
                     YPri = c(0.5, 0.5), # Prior of phase Ib trial, default Beta(0.5,0.5)
                     Round = 2) {
  tmp <- sumBetaDiff(
    parX = parX,
    parY =
      c(
        thisResp + YPri[1],
        TotalSample - thisResp + YPri[2]
      ),
    go_cut = go_cut,
    stop_cut = stop_cut
  )

  summaries <- round(c(
    thisResp,
    thisResp / TotalSample * 100,
    tmp$mode * 100,
    tmp$ci * 100,
    tmp$go * 100,
    tmp$stop * 100
  ), Round)

  summaries <- as.data.frame(summaries)

  rownames(summaries) <- c(
    "# resp", "obs ORR [%]", "mode [%]",
    "CI lower [%]", "CI upper [%]", "prob.go [%]", "prob.nogo [%]"
  )

  return(summaries)
}
