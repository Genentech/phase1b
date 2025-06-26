#' Helper function for simulation result as input for `plotOc()`
#'
#' @inheritParams h_get_oc
#' @typed all_looks : numeric
#' original looks before adjustment by `wiggle = TRUE`, if applied.
#' Different to `all_sizes` which is after the adjustment, if made.
#'
#' @return A data frame or tibble with the following variables :
#'  - `decision` : decision `flag` with `TRUE` for Go, `FALSE` for Stop, `NA` for Gray zone.
#'  - `look` : resulting number of look size, anything below maximum
#'   look size is an indicated interim, Futility or Efficacy or both.
#'  - `prop` : proportion of responders by `decision` and `look`.
#'
#' @keywords internal
#'
h_get_dataframe_oc <- function(decision, all_sizes, all_looks) {
  df <- data.frame(
    decision = decision,
    all_sizes = all_sizes,
    all_looks = all_looks # original looks
  )
  # summarise into frequency table
  df <- df |>
    dplyr::group_by(decision, all_looks) |>
    dplyr::summarise(prop = sum(length(decision)) / nrow(df)) |>
    tibble::as_tibble()
  # setting levels of factors
  decision_levels <- c(TRUE, FALSE, NA)
  look_levels <- unique(sort(all_looks))
  df$decision <- factor(df$decision, levels = decision_levels)
  df$look <- factor(df$all_looks, levels = look_levels)
  df <- df |>
    tidyr::complete(decision, all_looks, fill = list(prop = 0))
  df
}


#' Display the operating characteristics results using an `oc` object
#'
#' Plots results from simulated results of :
#' - `[ocPostprob()]`
#' - `[ocPostprobDist()]`
#' - `[ocPostpred()]`
#' - `[ocPostpredDist()]`
#' - `[ocRctPostprobDist()]`
#' - `[ocRctPredprobDist()]`
#'
#' @inheritParams h_get_dataframe_oc
#' @typed wiggle_status : flag
#' from `wiggle` flag in object.
#' @return `[ggplot()]` object
#'
#' @example examples/plotOc.R
#'
#' @importFrom ggplot2 geom_bar ggtitle
#'
#' @export
#' @keywords graphics
plotOc <- function(decision, all_sizes, all_looks, wiggle_status) {
  assert_logical(decision)
  assert_numeric(all_sizes)
  assert_numeric(all_looks)
  assert_flag(wiggle_status)
  df <- h_get_dataframe_oc(
    decision = decision,
    all_sizes = all_sizes,
    all_looks = all_looks
  )
  barplot <-
    ggplot2::ggplot(df, ggplot2::aes(fill = decision, x = all_looks, y = prop)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::ggtitle(
      "Results from simulation : \nProportion of Go/Stop/Grey zone decisions per interim/final analysis"
    ) +
    ggplot2::theme(title = ggplot2::element_text(size = 13)) +
    ggplot2::ylab("percentage") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12)) +
    ggplot2::xlab("look (n)") +
    ggplot2::scale_fill_manual(
      values = c("#009E73", "#FF0046", "lightgrey"),
      labels = c("Go", "Stop", "Grey zone")
    ) +
    ggplot2::labs(fill = "Decision")
  generic_title <-
    "Results from simulation : \nProportion of Go/Stop/Grey zone decisions per interim/final analysis"
  wiggle_warning_footnote <- paste("\nNote that sample sizes may differ slightly from the ones labeled")

  if (wiggle_status) {
    barplot +
      ggplot2::ggtitle(label = generic_title) +
      ggplot2::labs(caption = wiggle_warning_footnote) +
      ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = 10))
  } else {
    barplot +
      ggplot2::ggtitle(generic_title)
  }
}
