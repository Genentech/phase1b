dbetabinomMix(x = 2, m = 29, par = rbind(c(0.2, 0.4)), weights = 1)

dbetabinomMix(
  x = 2, m = 29, par = rbind(c(0.2, 0.4), c(1, 1)),
  weights = c(0.6, 0.4)
)

dbetabinomMix(
  x = 1:28, m = 29, par = rbind(c(0.2, 0.4), c(1, 1)),
  weights = c(0.6, 0.4)
)
