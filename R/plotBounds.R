#' @include boundsPredprob.R
#' @include boundsPostprob.R
NULL

#' Plot the boundary decision cutpoints
#'
#' This function will plot the response rate on the futility and efficacy
#' boundaries, and is used together with \code{\link{boundsPredprob}}
#' and \code{\link{boundsPostprob}}
#'
#' @param z an output of \code{\link{boundsPredprob}} and
#' \code{\link{boundsPostprob}}
#' @param area color the futility/efficacy stop area
#' @param grid show the grid of y-axis or not
#' @param yt  indicate the y axis: response rate is "p", number of responses
#' is "x"
#' @param add  add the boundary lines or not
#' @param cols specific the color of `[1]` efficacy area `[2]` futility area
#' `[3]` efficacy boundary `[4]` futility boundary
#' @param lwds line width (a two dimensional vector)
#' @param ltype line type
#' @param lpch  a value of plotting characters or symbols
#' @param lcex a value giving the amount by which plotting characters and
#' symbols should be scaled relative to the default.
#' @param gy a value to adjust the gray level of the plot (default 20),
#' applies when \code{grid==TRUE}
#' @return nothing, only produces the plot as side effect
#'
#' @importFrom graphics lines abline polygon plot
#'
#' @example examples/plotBounds.R
#' @export
#' @keywords graphics
plotBounds <- function(z, area = TRUE, grid = TRUE, yt = "x", add = FALSE,
                       cols = c("green", "red", "darkgreen", "orange"),
                       lwds = c(3, 3), ltype = "l", lpch = 16, lcex = 1, gy = 20) {
  n <- nrow(z)
  nmin <- min(z$nvec)
  nmax <- max(z$nvec)
  if (yt == "x") {
    z1 <- z$xL
    z2 <- z$xU
    yU <- nmax
    yU2 <- nmin
    ylabel <- "Number of Responses"
    gridy <- seq(0, yU, by = floor(yU / gy))
  } else if (yt == "p") {
    z1 <- z$pL
    z2 <- z$pU
    yU <- 1
    yU2 <- 1
    ylabel <- "Response Rate"
    gridy <- seq(0, yU, by = yU / gy)
  } else {
    stop("yt can only be x or p")
  }
  if (add) {
    graphics::lines(z$nvec, z2,
      lwd = lwds[1], col = cols[3], type = ltype,
      pch = lpch, cex = lcex
    )
    graphics::lines(z$nvec, z1,
      lwd = lwds[2], col = cols[4], type = ltype,
      pch = lpch, cex = lcex
    )
    return(invisible())
  }
  graphics::plot(z$nvec, rep(0, n),
    xlim = c(0, max(z$nvec)), ylim = c(0, yU), type = "n",
    xlab = "n", ylab = ylabel
  )
  if (grid) {
    graphics::abline(h = gridy, col = "gray")
  }
  if (area) {
    graphics::polygon(c(z$nvec, nmax, nmin), c(z2, yU, yU2),
      lwd = lwds[1],
      col = cols[1], border = cols[1]
    )
    graphics::polygon(c(z$nvec, nmax, nmin), c(z1, 0, 0),
      lwd = lwds[2],
      col = cols[2], border = cols[2]
    )
  } else {
    graphics::lines(z$nvec, z2,
      lwd = lwds[1], col = cols[1], type = ltype,
      pch = lpch, cex = lcex
    )
    graphics::lines(z$nvec, z1,
      lwd = lwds[2], col = cols[2], type = ltype,
      pch = lpch, cex = lcex
    )
  }
  return(invisible())
}
