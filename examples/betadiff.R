# dbetadiff ----

# We calculate and plot of density (pdf) of the Beta distribution of the difference
# using the following parameters:
parX <- c(1, 52)
parY <- c(5.5, 20.5)

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
