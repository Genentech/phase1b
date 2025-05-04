# The beta distribution and acceptable bounds for
# a meaningful improvement of 0.30 and worsening of 0.1
parX <- c(1, 52) # Control group's parameters
parY <- c(5.5, 20.5) # Treatment group's parameters
plotBetaDiff(
  parX = parX,
  parY = parY,
  go_cut = 0.3,
  stop_cut = 0.1, # below a difference of 10%, is an unsuccessful trial
  shade = TRUE,
  note = TRUE
)

# a larger Go_cut with uniform prior
plotBetaDiff(
  parX = c(1, 1), # Control group's parameters
  parY = c(1, 1), # Treatment group's parameters
  go_cut = 0.3,
  stop_cut = 0.1, # below a difference of 10%, is an unsuccessful trial
  shade = TRUE,
  note = TRUE
)
