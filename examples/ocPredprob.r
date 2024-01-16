## to examine operating characteristics of a phase Ib design with 2 interim analyses
## (sample size @ 10, 20 using predictive probability) and one final analysis (sample size @ 30);

# design details
# multiple looks @ 10, 20, 30 patients
# True response rate of the treatment group = 40%
# control response rate = 25%

# denote treatment response rate as RRS
# trial success is defined as: P(RRS > control) > tT
set.seed(20)
res1 <- ocPredprob(c(10, 20, 30),
  p = 0.4, p0 = 0.25, tT = 0.6, phiL = 0.2,
  phiU = 0.8, parE = c(1, 1), ns = 100
)
res1$oc


# this call will generate d (distance for random looks around the look locations)
# based on "floor(min(nn - c(0,nn[-length(nn)]))/2)" as d is missing:

res2 <- ocPredprob(c(10, 20, 30), 0.4,
  p0 = 0.25, tT = 0.6, phiL = 0.2, phiU = 0.8,
  parE = c(1, 1), ns = 100, nr = TRUE
)
res2$oc

# now d is specified. note that d has to be a positive value;
res3 <- ocPredprob(c(10, 20, 30), 0.4,
  p0 = 0.25, tT = 0.6, phiL = 0.2, phiU = 0.8,
  parE = c(1, 1), ns = 100, nr = TRUE, d = 5
)
res3$oc


# finally, we can also have separate specification of efficacy and futility analyses. E.g.
# here have futility decisions only at the final analysis:


res4 <- ocPredprob(c(10, 20, 30), 0.4,
  p0 = 0.25, tT = 0.6, phiL = 0.2, phiU = 0.8,
  parE = c(1, 1), ns = 100, nr = TRUE, d = 5, nnF = c(30)
)
res4$oc
# compared to res3, we see that there is no early futility stopping anymore,
# and the overall probability for stopping for futility (which is the type II error here)
# is much lower.

# 2)
# to examine operating characteristics of a phase Ib design with a seperate
# rule for futility at final analysis
# denote treatment response rate as RRS
# trial success is defined as: P(RRS>p0)>tT
# trial failure is defined as: P(RRS<p1)>tFu
# (gray zone could occur in the final analysis)

# when tFu=1-tT and phiFu=1-phiL, this decision rule (res5) is the same as the res1

set.seed(20)
res5 <- ocPredprob(c(10, 20, 30),
  p = 0.4, p0 = 0.25, tT = 0.6, tFu = 0.4, phiFu = 0.8,
  phiU = 0.8, parE = c(1, 1), ns = 100
)
res5$oc
