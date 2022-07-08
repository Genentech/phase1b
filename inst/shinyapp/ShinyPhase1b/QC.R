## To QC the app
## Created on Nov 15, 2016

# 1) postprobDist:
# Final design/ analysis:
postprobDist(x = 16, n = 23, parE = c(0.6, 0.4), parS = c(0.6, 0.4)) # OK
# Interim analysis
postprobDist(x = 16, n = 20, parE = c(0.6, 0.4), parS = c(0.6, 0.4))

postprobDist(x = 10, n = 15, parE = c(0.6, 0.4), parS = c(0.6, 0.4))

postprobDist(x = 10, n = 15, parE = c(0.6, 0.4), parS = c(0.6, 0.4))
# oc evaluation
set.seed(123)
res1 <- ocPostprobDist(
  nn = c(15, 20, 23), p = 0.8, deltaE = 0.1, deltaF = -0.05, tL = 0.7, tU = 0.6,
  parE = c(0.6, 0.4), parS = c(0.6, 0.4), ns = 200
)

res1$oc
# 2)predprobDist
predprobDist(
  x = 16, n = 23, Nmax = 40, delta = 0.03, thetaT = 0.9,
  parE = c(1, 1), parS = c(6, 11)
)

set.seed(20)
res5 <- ocPredprobDist(c(10, 20, 30), 0.4,
  delta = 0.1, deltaFu = 0.03, tT = 0.7, tFu = 0.65, phiFu = 0.8,
  phiU = 0.75, parE = c(1, 1), parS = c(5, 25), ns = 1e2
)
res5$oc
