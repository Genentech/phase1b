# The original Lee and Liu (Table 1) example:
# Nmax = 40, x = 16, n = 23, beta(0.6,0.4) prior distribution,
# thetaT = 0.9. The control response rate is 60%:
predprob(
  x = 16, n = 23, Nmax = 40, p = 0.6, thetaT = 0.9,
  parE = c(0.6, 0.4)
)

# Lowering/Increasing the probability threshold thetaT of course increases
# /decreases the predictive probability of success, respectively:
predprob(
  x = 16, n = 23, Nmax = 40, p = 0.6, thetaT = 0.8,
  parE = c(0.6, 0.4)
)

predprob(
  x = 16, n = 23, Nmax = 40, p = 0.6, thetaT = 0.95,
  parE = c(0.6, 0.4)
)

# Mixed beta prior
predprob(
  x = 20, n = 23, Nmax = 40, p = 0.6, thetaT = 0.9,
  parE = rbind(c(1, 1), c(25, 15)),
  weights = c(3, 1)
)
