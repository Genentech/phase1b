# examples
plotBounds(boundsPredprob(
  nvec = c(10, 20, 30, 40), p = 0.20, tT = 0.80,
  phiL = 0.10, phiU = 0.90, a = 1, b = 1
), yt = "x")
plotBounds(boundsPredprob(
  nvec = c(10, 20, 30, 40), p = 0.20, tT = 0.80,
  phiL = 0.10, phiU = 0.90, a = 1, b = 1
), yt = "p")
plotBounds(
  boundsPostprob(
    nvec = c(10, 20, 30, 40), p0 = 0.20,
    tL = 0.10, tU = 0.90, a = 1, b = 1
  ),
  yt = "p", add = TRUE
)
