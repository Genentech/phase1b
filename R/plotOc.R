#' Helper function for simulation result as input for `plotOc()`
#'
#' Data frame input for bar plot for simulated results of :
#' - `[ocPostprob()]`
#' - `[ocPostprobDist()]`
#' - `[ocPostpred()]`
#' - `[ocPostpredDist()]`
#' - `[ocRctPostprobDist()]`
#' - `[ocRctPredprobDist()]`
#'
#' @inheritParams h_get_oc
#' @typed all_looks : numeric
#' original looks before adjustment by `wiggle = TRUE`, if applied.
#'
#' @keywords internal
#'
h_get_dataframe_oc <- function(decision, sample_size, all_looks) {
  assert_logical(decision)
  assert_numeric(sample_size)
  assert_numeric(all_looks)
  df <- data.frame(
    decision = decision,
    sample_size = sample_size,
    look = all_looks
  )
  # summarise into frequency table
  df <- df %>%
    dplyr::group_by(decision, look) %>%
    dplyr::summarise(prop = sum(length(decision)) / nrow(df)) %>%
    dplyr::as_tibble()
  # setting levels of factors
  all_decision <- c(TRUE, FALSE, NA)
  all_looks <- unique(sort(all_looks))
  df$decision <- factor(df$decision, levels = all_decision)
  df$look <- factor(df$look, levels = all_looks)
  df <- df %>%
    tidyr::complete(decision, look, fill = list(prop = 0))
  df
}

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
vintage_plotOc <- function(oc) {
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
}
