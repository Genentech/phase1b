#' Plot a summary plot corresponding to the sumTable output
#'
#' This function will return a plot showing a curve of the prob of a meaningful improvement over estimated diff
#' and a curve of the prob of a poor improvement over estimated diff
#'
#' @typed data : data.frame
#'  sourced [`data.frame`] from [(sumTable)]
#' @typed efficacious_prob : number
#'  a cut off for the probability of a meaningful improvement
#' @typed futile_prob : number
#'  a cut off for the probability of a poor improvement
#' @return the [`ggplot`] item which was imputed to the function
#'
#' @importFrom ggplot2 geom_line geom_area ggtitle  theme_light annotate xlab ylab
#' @importFrom tibble remove_rownames
#'
#' @example examples/plotDecision.R
#' @export
#' @keywords graphics
plotDecision <- function(data, efficacious_prob, futile_prob) {
  assert_data_frame(data, any.missing = FALSE)
  assert_number(efficacious_prob, finite = TRUE)
  assert_number(futile_prob, finite = TRUE)

  data <- data.frame(t(data))

  data <- tibble::remove_rownames(data)

  colnames(data) <- c(
    "responders",
    "obs",
    "mode",
    "ci_lower",
    "ci_upper",
    "prob_go",
    "prob_stop"
  )

  go_shade <- data[data$prob_go > efficacious_prob, ]

  stop_shade <- data[data$prob_stop > futile_prob, ]

  annotation_go <- paste0(
    "Probability of Go is ", efficacious_prob, "% when difference is at least ",
    min(data$mode[data$prob_go > efficacious_prob]), "%"
  )
  annotation_stop <- paste0(
    "Probability of Stop is ", futile_prob, "% when difference is at most ",
    max(data$mode[data$prob_stop > futile_prob]), "%"
  )

  ggplot2::ggplot(data) +
    ggplot2::geom_line(
      ggplot2::aes(x = mode, y = prob_go),
      linewidth = 1.5, colour = "#009E73"
    ) +
    ggplot2::theme_light() +
    ggplot2::scale_x_continuous(breaks = seq(from = 0, to = round(max(data$mode), digits = 1), by = 5)) +
    ggplot2::geom_area(
      data = go_shade,
      mapping = ggplot2::aes(x = mode, y = prob_go),
      fill = "#009E73"
    ) +
    ggplot2::geom_line(
      data = data,
      mapping = ggplot2::aes(x = mode, y = prob_stop),
      linewidth = 1.5, colour = "#FF0046"
    ) +
    ggplot2::geom_area(
      data = stop_shade,
      mapping = ggplot2::aes(x = mode, y = prob_stop),
      fill = "#FF0046"
    ) +
    ggplot2::ggtitle("Probability of Difference and respective Go and Stop probabilities.") +
    ggplot2::xlab("Difference between treatment in Response Rate (%)") +
    ggplot2::ylab("Probability (%)") +
    ggplot2::annotate("text", x = mean(data$mode), y = 90, label = annotation_go) +
    ggplot2::annotate("text", x = mean(data$mode), y = 85, label = annotation_stop)
}
