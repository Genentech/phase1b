#' Plot the Beta distribution
#'
#' This function will plot the PDF of a beta distribution
#'
#' @inheritParams dbetabinom
#' @typed alpha : number
#'  first parameter of the Beta distribution
#' @typed beta : number
#'  second parameter of the Beta distribution
#'
#' @return A beta distribution density plot
#'
#' @importFrom graphics axis
#'
#' @example examples/plotBeta.R
#' @export
#' @keywords graphics
plotBeta <- function(alpha, beta) {
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
#' @param parY non-negative parameters of the treatment Beta distribution.
#' @param parX non-negative parameters of the historical control Beta distribution
#' @param cut_B a meaningful improvement threshold
#' @param cut_W a poor improvement throshold
#' @param shade paint the two areas under the curve, default value=1 as "yes". other numbers stands for "no";
#' @param note show values of the colored area, default value=1 as "yes". other numbers stands for "no"
#' @param \dots additional arguments to \code{plot}
#' @return nothing, only produces the plot as side effect
#'
#' @example examples/myPlotDiff.R
#'
#' @importFrom graphics par axis polygon mtext
#' @importFrom stats integrate
#'
#' @export
#' @keywords graphics
myPlotDiff <- function(parY, # parameters of phase Ib trial;
                       parX, # parameters of HC;
                       cut_B = 0.20, # a meaningful improvement threshold;
                       cut_W = 0.1, # a poor improvement threshold;
                       shade = 1, # paint the two areas under the curve, default: yes. other numbers stands for "no";
                       note = 1, # show values of the colored area, default: yes. other numbers stands for "no";
                       ...) {
  if (note == 1) {
    graphics::par(mar = c(5, 15, 1, 15) + .1)
  } else {
    graphics::par(mar = c(5, 5, 1, 5) + .1)
  }
  grid <- seq(from = -0.5, to = 0.75, length = 1000)
  xticks <- seq(from = -1, to = 1, by = 0.25)



  graphics::plot(
    x = grid,
    y = dbetadiff(grid, parY = parY, parX = parX),
    ylab = "",
    xaxt = "n",
    yaxt = "n",
    type = "l",
    xaxs = "i",
    yaxs = "i",
    ...
  )

  graphics::axis(
    side = 1, at = xticks,
    labels =
      paste(ifelse(xticks >= 0, "+", ""),
        xticks * 100, "%",
        sep = ""
      )
  )

  ## now color the go / stop prob areas

  if (shade == 1) {
    ## first stop:
    stopGrid <- grid[grid <= cut_W]
    nStop <- length(stopGrid)

    graphics::polygon(
      x =
        c(
          stopGrid,
          rev(stopGrid)
        ),
      y =
        c(
          rep(0, nStop),
          dbetadiff(rev(stopGrid), parY = parY, parX = parX)
        ),
      col = "red"
    )

    A_value <- stats::integrate(
      f = dbetadiff,
      parY = parY,
      parX = parX,
      lower = -1,
      upper = cut_W
    )
    if (note == 1) {
      graphics::mtext(
        paste("Prob(diff< ", round(cut_W * 100), "%)=",
          sprintf("%1.2f%%", 100 * as.numeric(A_value$value)),
          sep = ""
        ),
        side = 2, line = 1, las = 1, cex = 1
      )
    }

    ## then go:
    goGrid <- grid[grid >= cut_B]
    nGo <- length(goGrid)

    graphics::polygon(
      x =
        c(
          goGrid,
          rev(goGrid)
        ),
      y =
        c(
          rep(0, nGo),
          dbetadiff(rev(goGrid), parY = parY, parX = parX)
        ),
      col = "green"
    )

    B_value <- stats::integrate(
      f = dbetadiff,
      parY = parY,
      parX = parX,
      lower = cut_B,
      upper = 1
    )

    if (note == 1) {
      graphics::mtext(
        paste(
          sprintf("%1.2f%%", 100 * as.numeric(B_value$value)),
          "=Prob(diff> ",
          round(cut_B * 100), "%)",
          sep = ""
        ),
        side = 4,
        line = 1,
        las = 1,
        cex = 1
      )
    }
  }
}
