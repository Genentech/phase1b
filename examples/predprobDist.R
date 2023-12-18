predprobDist(
  x = 16, n = 23, Nmax = 40, delta = 0, thetaT = 0.9,
  parE = c(0.6, 0.4), parS = c(7, 11)
)
# Specifying delta.
predprobDist(
  x = 16, n = 23, Nmax = 40, delta = 0.1, thetaT = 0.9,
  parE = c(1, 1), parS = c(6, 11)
)

# Lowering theta increases predictive probability.
predprobDist(
  x = 16, n = 23, Nmax = 40, delta = 0.1, thetaT = 0.5,
  parE = c(1, 1), parS = c(6, 11)
)

# Beta-mixture prior for experimental arm.
predprobDist(
  x = 16,
  n = 23,
  Nmax = 40,
  delta = 0.1,
  thetaT = 0.9,
  parE = rbind(c(1, 1), c(2, 6)),
  weights = c(2, 1),
  parS = c(6, 11)
)

# More extensive use of historical trials for control arm.
predprobDist(
  x = 16,
  n = 23,
  xS = 5,
  nS = 10,
  Nmax = 40,
  NmaxControl = 20,
  delta = 0.1,
  thetaT = 0.9,
  parE = rbind(c(1, 1), c(3, 5)),
  weights = c(2, 1),
  parS = rbind(c(1, 1), c(2, 6)),
  weightsS = c(2, 1)
)
