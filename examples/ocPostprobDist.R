# operating characteristics for posterior probability method with beta prior on SOC

# design details
# multiple looks @ 10, 20, 30 patietns
# True response rate of the treatment group=0.4

# stop for efficacy (deltaE): P(pE > pS + deltaE) >tU
# stop for futility (deltaF): P(pE < pS - deltaF) >tL
# where pE is the response rate of treatment group, pS is the response rate of
# control group. Both of them are random variables.


res1 <- ocPostprobDist(
  nn = c(10, 20, 30), p = 0.4, deltaE = 0.1, deltaF = -0.1, tL = 0.6, tU = 0.6,
  parE = c(1, 1), parS = c(5, 25), ns = 100
)

res1$oc


# generate random look locations around 10,20,30 patients
# this call will generate d (distance for random looks around the look locations)
# based on "floor(min(nn - c(0,nn[-length(nn)]))/2)" as d is missing:
res2 <- ocPostprobDist(
  nn = c(10, 20, 30), p = 0.4, deltaE = 0.1, deltaF = -0.1, tL = 0.6, tU = 0.6,
  parE = c(1, 1), parS = c(5, 25), ns = 100, nr = TRUE
)

res2$oc

# specify the distance for random looks around the look locations in nn (d=5 for illustration)
res3 <- ocPostprobDist(
  nn = c(10, 20, 30), p = 0.4, deltaE = 0.1, deltaF = -0.1, tL = 0.6, tU = 0.6,
  parE = c(1, 1), parS = c(5, 25), ns = 100, nr = TRUE, d = 5
)

res3$oc


# finally, we can also have separate specification of efficacy and futility analyses. E.g.
# here have futility decisions only at the final analysis:
res4 <- ocPostprobDist(
  nn = c(10, 20, 30), p = 0.4, deltaE = 0.1, deltaF = -0.1, tL = 0.6, tU = 0.6,
  parE = c(1, 1), parS = c(5, 25), ns = 100, nr = TRUE, d = 5, nnF = c(30)
)
res4$oc
# compared to res3, we see that there is no early futility stopping anymore,
# and the overall probability for stopping for futility (which is the type II error here)
# is much lower.
