#' Plot a summary plot corresponding to the sumTable output
#'
#' This function will return a plot showing a curve of the prob of a meaningful improvement over estunated diff
#' and a curve of the prob of a poor improvement over estunated diff
#'
#' @inheritParams plotBetaDiff
#' @typed data : data.frame
#'  sourced [`data.frame`] from [(sumTable)]
#' @return the \code{data} item which was imputed to the function
#'
#' @importFrom graphics lines abline polygon plot par grid mtext box axis
#'
#' @example examples/plotDecision.R
#' @export
#' @keywords graphics
plotDecision <- function(data, go_cut, stop_cut) {
  assert_data_frame(data, any.missing = FALSE)
  assert_number(go_cut, finite = TRUE)
  assert_number(stop_cut, finite = TRUE)

  data <- data.frame(t(data))

  data <- tibble::remove_rownames(data)

  colnames(data) <- c("responders",
                      "obs",
                      "mode",
                      "ci_lower",
                      "ci_upper",
                      "prob_go",
                      "prob_stop")

  go_shade <- data[data$prob_go > go_cut,]

  stop_shade <- data[data$prob_stop > stop_cut,]

  ggplot2::ggplot(go_shade) +
    geom_area(ggplot2::aes(x = mode, y = prob_go))

  ggplot2::ggplot(data) +
    ggplot2::geom_line(ggplot2::aes(x = mode, y = prob_go), linewidth = 1.5, colour = "#009E73") +
    ggplot2::theme_light() +
    ggplot2::geom_area(data = go_shade, mapping = ggplot2::aes(x = mode, y = prob_go), fill = "#009E73") +
    ggplot2::geom_line(data = data, ggplot2::aes(x = mode, y = prob_stop), linewidth = 1.5, colour = "#FF0046") +
    ggplot2::geom_area(data = stop_shade, mapping = ggplot2::aes(x = mode, y = prob_stop), fill = "#FF0046") +
    ggplot2::ggtitle("Prob") +
    ggplot2::xlab("") +
    ggplot2::ylab("")

}
