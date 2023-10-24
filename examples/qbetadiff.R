# The following examples use these parameters:
parX <- c(1, 52)
parY <- c(5.5, 20.5)

# qbetadiff ----
# Calculate quantile when at there is at least 20% of difference.
test <- qbetadiff(
  p = 0.2,
  parY = parY,
  parX = parX
)
