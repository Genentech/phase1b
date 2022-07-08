##' Return summary statistics under one response scenario
##'
##' This function will calculate the summary statistics for a specific response
##' outcome scenario.
##'
##' @param thisResp number of responses
##' @param TotalSample sample size
##' @param cut_B a meaningful improvement threshold: at least cut_B (e.g. 15%) improvement
##' @param cut_W a poor improvement threshold: at most cut_W (e.g. 5%) improvement
##' @param parX non-negative parameters of the beta distribution of the control (posterior)
##' @param YPri non-negative parameters of the beta prior of the treatment, default Beta(0.5,0.5)
##' @param Round Rounding of the output statistics
##'
##' @return A vector with the results.
##'
##' @example examples/sumTable.R
##' @export
sumTable <- function(thisResp, # number of responses;
                     TotalSample, # Sample size;
                     cut_B, # meaningful improvement: at least cut_B (say 15\%) improvement;
                     cut_W, # poor improvement: at most cut_W (say 5\%) improvement;
                     parX, # Two parameters of the beta distribution of the control (posterior);
                     YPri = c(0.5, 0.5), # Prior of phase Ib trial, default Beta(0.5,0.5)
                     Round = 2) {
  tmp <- sumBetadiff(
    parX = parX,
    parY =
      c(
        thisResp + YPri[1],
        TotalSample - thisResp + YPri[2]
      ),
    cutB = cut_B,
    cutW = cut_W
  )

  summaries <- round(c(thisResp, thisResp / TotalSample * 100, tmp$mode * 100, tmp$ci * 100, tmp$go * 100, tmp$nogo * 100), Round)

  summaries <- as.data.frame(summaries)

  rownames(summaries) <- c("# resp", "obs ORR [%]", "mode [%]", "CI lower [%]", "CI upper [%]", "prob.go [%]", "prob.nogo [%]")

  return(summaries)
}
