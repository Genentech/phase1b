# dbetabinom --
dbetabinom(x = 2, m = 29, a = 0.2, b = 0.4, log = FALSE)

# Can also specify x as a vector.
dbetabinom(x = 1:28, m = 29, a = 0.2, b = 0.4, log = FALSE)

# dbetabinomMix --
# returns the same result as first example
dbetabinomMix(
  x = 2,
  m = 29,
  par = rbind(c(0.2, 0.4)),
  weights = 1
)

dbetabinomMix(
  x = 1:28,
  m = 29,
  par = rbind(
    c(0.2, 1),
    c(0.4, 1)
  ),
  weights = rbind(
    c(1, 1),
    c(0.4, 1)
  ),
  log = FALSE
)

# dbetaMix --
dbetaMix(
  x = 1:20,
  par = rbind(c(1, 2), c(2, 5)),
  weights = c(1, 2)
)

# pbetaMix --
pbetaMix(
  q = 0.3,
  par = rbind(c(0.2, 0.4)),
  weights = 1
)

pbetaMix(
  q = 0.3,
  par = rbind(c(0.2, 0.4), c(1, 1)),
  weights = c(0.6, 0.4)
)

# qbetaMix --
qbetaMix(
  p = 0.6,
  par = rbind(c(0.2, 0.4)),
  weights = 1
)

qbetaMix(
  p = 0.6,
  par = rbind(c(0.2, 0.4), c(1, 1)),
  weights = c(0.6, 0.4)
)
