# The following examples use these parameters:
parX <- c(1, 52) # Control group parameters
parY <- c(5.5, 20.5) # Treatment group parameters

# The difference between Control and Treatment is denoted as z.
z <- seq(from = -1, to = 1, length = 100)
plot(z, dbetadiff(z, parY = parY, parX = parX),
  type = "l"
)

# Calculate probability of Go, if difference was at least 15%.
test <- integrate(
  f = dbetadiff,
  parY = parY,
  parX = parX,
  lower = 0.15,
  upper = 1
)
str(test)
test$value

# Calculate probability of Stop, if difference was at most 50%.
integrate(
  f = dbetadiff,
  parY = parY,
  parX = parX,
  lower = -1,
  upper = 0.5
)
# Explanation: given density of the difference between two beta distributions
# is P( difference < 0.5 | dbetadiff ), and
# if the rule is STOP when difference < 0.5, then
# P( STOP | dbetadiff ) is high, or approximately 99 %
# therefore if the difference was at most 50%, then
# we will stop most of the time.
