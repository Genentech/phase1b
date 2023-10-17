# dbetadiff ----

# We calculate and plot density (pdf) of the Beta distribution distribution
parX <- c(1, 52)
parY <- c(5.5, 20.5)

# The difference between Control and Treatment
z <- seq(from = -1, to = 1, length = 100)
plot(z, dbetadiff(z, parY = parY, parX = parX),
  type = "l"
)

# dbetadiff ----
# calculate probability of Go, a positive difference of 15%
# for Go:
test <- integrate(
  f = dbetadiff,
  parY = parY,
  parX = parX,
  lower = 0.15,
  upper = 1
)
str(test)
test$value

# dbetadiff for Stop:
integrate(
  f = dbetadiff,
  parY = parY,
  parX = parX,
  lower = -1,
  upper = 0.5
)

# qbetadiff ----
test <- qbetadiff(
  p = 0.2,
  parY = parY,
  parX = parX
)

# pbetadiff ----
pbetadiff(
  q = test,
  parY = parY,
  parX = parX
)
