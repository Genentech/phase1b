# The beta distribution and acceptable bounds for
# a meaningful improvement of 0.20 and worsening of 0.1
parX <- c(1, 52) # prior  parameters of experimental arm
parY <- c(5.5, 20.5) # prior  parameters of control or SOC
plotBetaDiff(
  parY = parY,
  parX = parX,
  Go_cut = 0.3,
  Stop_cut = 0.1, # below a difference of 10%, is an unsuccessful trial
  shade = TRUE,
  note = TRUE
)

# a larger Go_cut with uniform prior
plotBetaDiff(
  parY = c(1, 1), # prior  parameters for experimental arm
  parX = c(1, 1), # prior parameters for control or SOC arm
  Go_cut = 0.3,
  Stop_cut = 0.1, # below a difference of 10%, is an unsuccessful trial
  shade = TRUE,
  note = TRUE
)
