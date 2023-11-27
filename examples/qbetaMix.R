# Only 1 mixture component, i.e., weights = 1
qbetaMix(
  p = 0.60,
  par = rbind(c(0.2, 0.4)),
  weights = 1
)

# With 2 mixture components
qbetaMix(
  p = 0.6, par = rbind(c(0.2, 0.4), c(1, 1)),
  weights = c(0.6, 0.4)
)

# Can also specify q as a vector
qbetaMix(
  p = seq(0, 1, .01),
  par = rbind(c(0.2, 0.4), c(1, 1)),
  weights = c(0.6, 0.4)
)
