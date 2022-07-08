# similar input parameters as the oc.predprobDist function
# default simulation number is 10000. for illustration, the ns is set as 100 to
# save program running time. Please note that 10 simulation run (ns=10) may not
# be sufficient for a consistent OC evaluation.
# 1)
set.seed(20)
res <- ocRctPredprobDist(
  nn = c(10, 20, 30), pE = 0.4, pS = 0.3, delta = 0.1,
  relativeDelta = FALSE, tT = 0.6,
  phiL = 0.2, phiU = 0.8,
  parE = c(a = 1, b = 1), parS = c(a = 1, b = 1),
  randRatio = 1,
  ns = 10, nr = FALSE, d = NULL
)

res$oc

# separate specification of efficacy and futility analyses can be performed. E.g.
# here have futility decisions only at the final analysis:
set.seed(20)
res1 <- ocRctPredprobDist(
  nn = c(10, 20, 30), pE = 0.4, pS = 0.3, delta = 0.1, relativeDelta = FALSE,
  tT = 0.6,
  phiL = 0.2, phiU = 0.8,
  parE = c(a = 1, b = 1), parS = c(a = 1, b = 1),
  randRatio = 1,
  ns = 10, nr = FALSE, d = NULL, nnF = c(30)
)

res1$oc

# 2)
# to examine operating characters of a phase Ib design with a
# seperate rule for futility
# at final analysis
# denote treatment response rate as RRS
# trial success is defined as: P(RRS>control+delta)>tT
# trial failure is defined as: P(RRS<control+deltaFu)>tFu
# (gray zone could occur in the final analysis)

set.seed(20)
res2 <- ocRctPredprobDist(
  nn = c(10, 20, 30), pE = 0.4, pS = 0.3, delta = 0.1, deltaFu = 0.05,
  relativeDelta = FALSE,
  tT = 0.6, tFu = 0.6,
  phiFu = 0.8, phiU = 0.8,
  parE = c(a = 1, b = 1), parS = c(a = 1, b = 1),
  randRatio = 1,
  ns = 10, nr = FALSE, d = NULL
)

res2$oc
