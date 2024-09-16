#' Plot the Beta distribution
#'
#' This function will plot the PDF of a beta distribution
#'
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
plotBetaDiff <- function(parY, # parameters of phase Ib trial;
                         parX, # parameters of HC;
                         cut_B = 0.20, # a meaningful improvement threshold;
                         cut_W = 0.1, # a poor improvement threshold;
                         shade = TRUE, # paint the two areas under the curve,
                         # default: yes. other numbers stands for "no";
                         note = TRUE, # show values of the colored area,
                         # default: yes. other numbers stands for "no";
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
