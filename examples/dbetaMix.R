# Calculating the density of a mixture.
dbetaMix(x = 0.3, par = rbind(c(0.2, 0.4)), weights = 1)

# With 2 mixture components
dbetaMix(
  x = 0.3, par = rbind(c(0.2, 0.4), c(1, 1)),
  weights = c(0.6, 0.4)
)

# Can also specify x as a vector
dbetaMix(
  x = seq(0, 1, .01),
  par = rbind(c(0.2, 0.4), c(1, 1)),
  weights = c(0.6, 0.4)
)

dbetaMix(
  x = seq(0, 1, .01),
  par = rbind(c(1, 1)),
  weights = c(0.6, 0.4)
)
