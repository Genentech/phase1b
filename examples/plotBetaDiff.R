# The beta distribution and acceptable bounds for
# a meaningful improvement of 0.30 and worsening of 0.1
parE <- c(1, 52) # prior  parameters of experimental arm
parS <- c(5.5, 20.5) # prior  parameters of control or SOC
plotBetaDiff(
  parS = parS,
  parE = parE,
  Go_cut = 0.3,
  Stop_cut = 0.1, # below a difference of 10%, is an unsuccessful trial
  shade = TRUE,
  note = TRUE
)

# a larger Go_cut with uniform prior
plotBetaDiff(
  parS = c(1, 1), # prior  parameters for experimental arm
  parE = c(1, 1), # prior parameters for control or SOC arm
  Go_cut = 0.3,
  Stop_cut = 0.1, # below a difference of 10%, is an unsuccessful trial
  shade = TRUE,
  note = TRUE
)
