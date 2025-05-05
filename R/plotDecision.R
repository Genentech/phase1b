#' Plot a summary plot corresponding to the sumTable output
#'
#' This function will return a plot showing a curve of the prob of a meaningful improvement over estunated diff
#' and a curve of the prob of a poor improvement over estunated diff
#' @param data the output object of \code{\link{sumTable}}
#' @param Pos_cut a cut off for the prob of a meaningful improvement
#' @param Neg_cut a cut off for the prob of a poor improvement
#' @return the \code{data} item which was imputed to the function
#'
#' @importFrom graphics lines abline polygon plot par grid mtext box axis
#'
#' @example examples/plotDecision.R
#' @export
#' @keywords graphics
plotDecision <- function(data, Pos_cut, Neg_cut) {
  xticks <- seq(from = -50, to = 100, by = 10)

  # x <- as.numeric(data[1, ]) # number of response;
  x <- data[1, ]

  x.mode <- as.numeric(data[3, ]) # the response diff mode

  y <- as.numeric(data[6, ]) # The Prob of a meaningful improvement
  y <- data[6, ]

  z <- as.numeric(data[7, ]) # The Prob of a not meaningful improvement

  # Look up decision area;
  ShadeData <- t(data)

  colnames(ShadeData)

  # Green area;
  G_area <- ShadeData[ShadeData[, "prob.go [%]"] > Pos_cut, ]
  # Red ares;
  R_area <- ShadeData[ShadeData[, "prob.nogo [%]"] > Neg_cut, ]

  graphics::par(mar = c(5, 4, 4, 1) + .1)


  graphics::plot(x.mode, y,
    type = "n", xlim = range(x.mode), bty = "n", ylab = "Probability (%)", xaxt = "n", xaxs = "i", yaxs = "i",
    xlab = expression(paste("Estimated diff.", sep = "")), ylim = c(0, 100), panel.first = grid()
  )



  # find the interaction;


  # Green area;
  aboveG <- ShadeData[, "prob.go [%]"] > Pos_cut
  # Red ares;
  aboveR <- ShadeData[, "prob.nogo [%]"] > Neg_cut

  # Points always intersect when above=TRUE, then FALSE or reverse
  intersect.pointsG <- which(diff(aboveG) != 0)
  intersect.pointsR <- which(diff(aboveR) != 0)
  # Find the slopes for each line segment.
  x1.slopesG <- (ShadeData[intersect.pointsG + 1, "prob.go [%]"] - ShadeData[intersect.pointsG, "prob.go [%]"]) /
    (ShadeData[intersect.pointsG + 1, "mode [%]"] - ShadeData[intersect.pointsG, "mode [%]"])
  x1.slopesR <- (ShadeData[intersect.pointsR + 1, "prob.nogo [%]"] - ShadeData[intersect.pointsR, "prob.nogo [%]"]) /
    (ShadeData[intersect.pointsR + 1, "mode [%]"] - ShadeData[intersect.pointsR, "mode [%]"])

  x2.slopes <- 0
  # Find the intersection for each segment.
  x.pointsG <- ShadeData[intersect.pointsG, "mode [%]"] +
    ((Pos_cut - ShadeData[intersect.pointsG, "prob.go [%]"]) / (x1.slopesG))
  y.pointsG <- Pos_cut

  x.pointsR <- ShadeData[intersect.pointsR, "mode [%]"] +
    ((Neg_cut - ShadeData[intersect.pointsR, "prob.nogo [%]"]) / (x1.slopesR))
  y.pointsR <- Neg_cut


  graphics::polygon(c(R_area[, "mode [%]"], x.pointsR, x.pointsR, rev(R_area[, "mode [%]"])),
    c(rep(0, dim(R_area)[1] + 1), y.pointsR, rev(R_area[, "prob.nogo [%]"])),
    col = "red"
  )

  graphics::mtext(
    paste(
      "Est. Diff=",
      round(x.pointsR),
      "%, Prob.nogo=",
      round(y.pointsR),
      "%",
      sep = ""
    ),
    side = 3,
    line = 2
  )

  graphics::polygon(
    c(
      x.pointsG, G_area[, "mode [%]"],
      rev(G_area[, "mode [%]"]), x.pointsG
    ),
    c(
      rep(0, dim(G_area)[1] + 1),
      rev(G_area[, "prob.go [%]"]), y.pointsG
    ),
    col = "green"
  ) # meaningful part;

  graphics::mtext(
    paste(
      "Est. Diff=",
      round(x.pointsG),
      "%,Prob. go=",
      round(y.pointsG),
      "%",
      sep = ""
    ),
    side = 3,
    line = 1
  )

  graphics::lines(x.mode, y, col = "green", lwd = 3, type = "l") # Plot PDF of beta(R,NR);

  graphics::lines(x.mode, z, col = "red", lwd = 3, type = "l")



  if (Pos_cut == Neg_cut) {
    graphics::abline(h = Pos_cut, col = "black", lwd = 2)
  }

  graphics::box()

  LablePoint2 <- unique(sort(c(
    ceiling(min(x.mode) * 10) / 10,
    xticks, floor(max(x.mode) * 10) / 10
  ))) ## Can be modified

  graphics::axis(1, at = LablePoint2, labels = paste(LablePoint2, "%", sep = ""), las = 1, lwd = 2, cex.axis = 1)

  data
}
