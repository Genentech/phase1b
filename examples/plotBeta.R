# plotBeta
plotBeta(alpha = 4, beta = 5)
plotBeta(alpha = 1, beta = 1)

# plotBetDiff
parX <- c(1, 52) # parameters of experimental arm
parY <- c(5.5, 20.5) # parameters of control or SOC
plotBetaDiff(
  parY = parY,
  parX = parX,
  Go_cut = 0.3,
  Stop_cut = 0.1,
  shade = TRUE,
  note = TRUE
)
