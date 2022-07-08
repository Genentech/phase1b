# operating characteristics for posterior probability method

# design details (example)
# multiple looks @ 10, 20, 30 patietns
# True response rate of the treatment group=40%
# stop for futility: P(response rate < 20% )> 60%
# s top for efficacy: P(response rate > 30% )> 80%
# prior of treatment arm parE= Beta(1,1)
res1 <- ocPostprob(
  nn = c(10, 20, 30), p = 0.4, p0 = 0.2, p1 = 0.3, tL = 0.6, tU = 0.8,
  parE = c(1, 1), ns = 1000
)
res1$oc

# this call will generate d (distance for random looks around the look locations)
# based on "floor(min(nn - c(0,nn[-length(nn)]))/2)" as d is missing:
res2 <- ocPostprob(
  nn = c(10, 20, 30), p = 0.4, p0 = 0.2, p1 = 0.3, tL = 0.6, tU = 0.8,
  parE = c(1, 1), ns = 1000, nr = TRUE
)
res2$oc

# now d is specified:
res3 <- ocPostprob(
  nn = c(10, 20, 30), p = 0.4, p0 = 0.2, p1 = 0.3, tL = 0.6, tU = 0.8,
  parE = c(1, 1), ns = 1000, nr = TRUE, d = 5
)
res3$oc

# finally, we can also have separate specification of efficacy and
# futility analyses. E.g. here have futility decisions only at the final analysis:
res4 <- ocPostprob(
  nn = c(10, 20, 30), p = 0.4, p0 = 0.2, p1 = 0.3, tL = 0.6, tU = 0.8,
  parE = c(1, 1), ns = 1000, nr = TRUE, d = 5,
  nnF = c(30)
)
res4$oc
# compared to res3, we see that there is no early futility stopping anymore,
# and the overall probability for stopping for futility (which is the type II error here)
# is much lower.
