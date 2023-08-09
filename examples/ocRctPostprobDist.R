# note, deltaF has to be a value between 0 and 1 including the bounds, or NULL.

res <- ocRctPostprobDist(
  nn = c(10, 20, 30), pE = 0.4, pS = 0.3, deltaE = 0.15, deltaF = 0.05,
  relativeDelta = FALSE,
  tL = 0.8, tU = 0.8,
  parE = c(a = 1, b = 1),
  parS = c(a = 1, b = 1),
  randRatio = 1,
  ns = 100, nr = FALSE, d = NULL
)

res$oc


# separate specification of efficacy and futility analyses can be performed. E.g.
# here have futility decisions only at the final analysis:

res2 <- ocRctPostprobDist(
  nn = c(10, 20, 30), pE = 0.4, pS = 0.3, deltaE = 0.15, deltaF = 0.05,
  relativeDelta = FALSE,
  tL = 0.8, tU = 0.8,
  parE = c(a = 1, b = 1),
  parS = c(a = 1, b = 1),
  randRatio = 1,
  ns = 100, nr = FALSE, d = NULL, nnF = c(30)
)
res2$oc
# compared to res3, we see that there is no early futility stopping anymore,
# and the overall probability for stopping for futility (which is the type II error here)
# is much lower.

# Also, when deltaF is set to NULL, the futility decision is made when the probability to
# be deltaE better than the control is lower than 1 - tL. i.e. Pr(pE-pS>deltaE)<1-tL
res3 <- ocRctPostprobDist(
  nn = c(10, 20, 30), pE = 0.4, pS = 0.3, deltaE = 0.15, deltaF = NULL,
  relativeDelta = FALSE,
  tL = 0.2, tU = 0.8,
  parE = c(a = 1, b = 1),
  parS = c(a = 1, b = 1),
  randRatio = 1,
  ns = 100, nr = FALSE, d = NULL
)
res3$oc

# As when deltaF is set to null, it sometimes makes sense for a futility decision
# to be made even when the treatment is better than the control, e.g. when that difference
# is small. This can be accomplished by specifying a negative value for deltaF.
res4 <- ocRctPostprobDist(
  nn = c(10, 20, 30), pE = 0.4, pS = 0.3,
  deltaE = 0.15, deltaF = -0.05,
  relativeDelta = FALSE,
  tL = 0.2, tU = 0.8,
  parE = c(a = 1, b = 1),
  parS = c(a = 1, b = 1),
  randRatio = 1,
  ns = 100, nr = FALSE, d = NULL
)
res4$oc
