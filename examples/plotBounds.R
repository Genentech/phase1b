# examples
plotBounds(
  boundsPostprob(
    looks = c(10, 20, 30, 40), p0 = 0.20,
    tL = 0.10, tU = 0.90, parE = c(1, 1)
  ),
  yt = "p", add = TRUE
)
plotBounds(boundsPredprob(
  looks = c(10, 20, 30, 40), p0 = 0.20, tT = 0.80,
  phiL = 0.10, phiU = 0.90,
), yt = "x")
plotBounds(boundsPredprob(
  looks = c(10, 20, 30, 40), p0 = 0.20, tT = 0.80,
  phiL = 0.10, phiU = 0.90,
), yt = "p")
