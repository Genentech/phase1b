# The beta distribution and acceptable bounds for
# a meaningful improvement of 0.20 and worsening of 0.05
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
