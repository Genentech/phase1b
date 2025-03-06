#' Display the operating characteristics using an oc object
#'
#' Reads results from [ocPredprob()]
#' etc. and displays a bar plot of the operating characteristics
#'
#' @typed oc : list
#' returned oc parameters
#' @return nothing, only plots as side effect
#'
#' @importFrom graphics barplot title
#'
#' @example examples/plotOc.R
#' @export
#' @keywords graphics
plotOc <- function(oc) {
  if (wiggle == FALSE) {
    data <- table(oc$Decision, oc$SampleSize) / oc$params$sim
  } else {

  }

  ggplot(oc, aes(x = name, y = value)) +
    geom_bar(stat = "identity") +
    ggtitle("Percentage of trials that Go and Stop per look") +
    ylabs("Percentage %") +
    xlabs("Looks and sample size")



  ## plot function for oc.predprob or oc.postprob, or the dist versions of them
  graphics::barplot(table(oc$Decision, oc$SampleSize) / oc$params$sim, beside = TRUE)


  ## get the parameter
  parDat <- lapply(z$params, deparse)

  ## get parameters, which are saved in parDat
  allParNames <- c(
    "p", "p0", "p1",
    "delta", "deltaE", "deltaF", "relativeDelta",
    "phiL", "phiU", "tL", "tU", "tT", "parE", "parS"
  )
  parInds <- which(names(parDat) %in% allParNames)

  graphics::title(paste(names(parDat[parInds]), parDat[parInds],
    sep = "=", collapse = ", "
  ))
  graphics::title(xlab = paste(dimnames(z$oc)[[2]], signif(z$oc, 3),
    sep = " = ", collapse = ", "
  ))

  return(invisible())
}
