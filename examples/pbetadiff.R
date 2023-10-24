# The following examples use these parameters:
parX <- c(1, 52)
parY <- c(5.5, 20.5)

# pbetadiff ----
# Calculate probability based on quantile `Q(Z) =< 0.122838`
pbetadiff(
  q = 0.122838,
  parY = parY,
  parX = parX
)

# Calculate probability based on quantile `Q(Z) =< 0.5`
pbetadiff(
  q = 0.5,
  parY = parY,
  parX = parX
)
