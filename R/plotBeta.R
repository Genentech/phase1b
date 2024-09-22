#' Plot the Beta distribution
#'
#' This function will plot the PDF of a beta distribution
#'
#' @inheritParams dbetabinom
#' @typed alpha : number
#'  first parameter of the Beta distribution
#' @typed beta : number
#'  second parameter of the Beta distribution
#' @return A beta distribution density plot
#'
#' @importFrom graphics axis
#'
#' @example examples/plotBeta.R
#' @export
#' @keywords graphics
plotBeta <- function(alpha, beta, ...) {
  x_support <- seq(from = 0, to = 1, length = 1000)
  data <- data.frame(
    grid = x_support,
    xticks = seq(from = 0, to = 1, by = 0.25),
    density = dbeta(x_support, alpha, beta)
  )
  ggplot2::ggplot(data) +
    ggplot2::geom_line(ggplot2::aes(x = grid, y = density)) +
    ggplot2::ggtitle(paste("Beta density with alpha =", alpha, "and beta =", beta, "parameters.")) +
    ggplot2::xlab("response rate") +
    ggplot2::ylab(quote(f(x))) +
    ggplot2::theme(axis.ticks.x = ggplot2::element_line(linewidth = 0.5)) +
    ggplot2::scale_x_continuous(labels = scales::percent_format())
}

#' Plot Diff Between two Beta distributions
#'
#' This function will plot the PDF of a difference between two Beta distributions
#'
#' @typed parY : numeric
#'  non-negative parameters of the treatment Beta distribution.
#' @typed parX : numeric
#'  non-negative parameters of the historical control Beta distribution
#' @typed cut_B : number
#'  a meaningful improvement threshold, the lower boundary of a meaningfully improvement in response rate
#' @typed cut_W : number
#'  a poor improvement threshold, the upper boundary of a meaningfully poor improvement in response rate
#' @typed shade : flag
#'  paint the two areas under the curve, default value = TRUE
#' @typed note : flag
#'  show values of the colored area, default value = TRUE
#' @typed ... :
#'  additional arguments to `ggplot()`
#' @return a ggplot object
#'
#' @example examples/myPlotDiff.R
#'
#' @importFrom graphics par axis polygon mtext
#' @importFrom stats integrate
#'
#' @export
#' @keywords graphics
plotBetaDiff <- function(parY, # parameters of experimental arm
                         parX, # parameters of control or SOC
                         Go_cut = 0.20, # a meaningful improvement threshold
                         Stop_cut = 0.1, # a poor improvement threshold
                         shade = TRUE, # paint the two areas under the curve
                         note = TRUE) { # show values of the colored area
  diff <- seq(from = -1, to = 1, length = 1000)
  data <- data.frame(
    grid = diff,
    xticks = seq(from = 0, to = 1, by = 0.25),
    density = dbetadiff(z = diff, parY = parY, parX = parX)
  )
  data$Stop <- ifelse(diff > -1 & diff < Stop_cut, TRUE, FALSE)
  data$Go <- ifelse(diff > Go_cut & diff < 1, TRUE, FALSE)

  Go_auc <- integrate(
    f = dbetadiff,
    parY = parY,
    parX = parX,
    lower = Go_cut, # Calculate probability of Go, if difference was at least `Go_cut`.
    upper = 1
  )
  Stop_auc <- integrate(
    f = dbetadiff,
    parY = parY,
    parX = parX,
    lower = -1,
    upper = Stop_cut # Calculate probability of Stop, if difference was at most `Stop_cut`.
  )

  Go_label <- paste("Probability of Go is", round(Go_auc$value * 100, digits = 2), "%")
  Stop_label <- paste("Probability of Stop is", round(Stop_auc$value * 100, digits = 2), "%")
  plot_title <- paste("According to Beta difference density", Go_label, "and", Stop_label)

  pbetadiff_plot <- ggplot2::ggplot(data = data, mapping = aes(x = grid, y = density)) +
    ggplot2::geom_line(colour = "#888888") +
    xlab("Difference between treatment") +
    ggplot2::ylab(quote(f(x))) +
    ggplot2::ggtitle(plot_title)

  if (shade == TRUE) {
    pbetadiff_plot +
      ggplot2::geom_area(data = filter(data, Go == TRUE), fill = "#009E73") +
      ggplot2::geom_area(data = filter(data, Stop == TRUE), fill = "#D55E00")
  }

  if (note == TRUE) {
    pbetadiff_plot +
      ggplot2::annotate("text", x = -0.5, y = 4.25, label = Go_label, colour = "#009E73") +
      ggplot2::annotate("text", x = -0.5, y = 4.75, label = Stop_label, colour = "#D55E00")
  }
}
