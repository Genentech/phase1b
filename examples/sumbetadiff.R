parX <- c(1, 52) # Control group's parameters
parY <- c(5.5, 20.5) # Treatment group's parameters
sumBetaDiff(
  parX = parX,
  parY = parY,
  ci_level = 0.9,
  go_cut = 0.6,
  stop_cut = 0.2
)
