## to examine operating characteristics of a phase Ib design with 2 interim analyses
## (sample size @ 10, 20 using predictive probability) and one final analysis
## (sample size @ 30);

# design details
# multiple looks @ 10, 20, 30 patients
# True response rate of the treatment group=0.4
# response rate improvement (delta)=10%  as an absolute value (default)
# 1)
# denote treatment response rate as RRS
# trial success is defined as: P(RRS>control+delta)>tT
# note: ns is suggested to be a larger value than 10 in a real study in order to
# achieve correct results


set.seed(20)
res1 <- ocPredprobDist(c(10, 20, 30), 0.4,
  delta = 0.1, tT = 0.6, phiL = 0.2, phiU = 0.8,
  parE = c(1, 1), parS = c(5, 25), ns = 10
)
res1$oc



# this function will generate d based on "floor(min(nn - c(0,nn[-length(nn)]))/2)"
# if d is missing
res2 <- ocPredprobDist(c(10, 20, 30), 0.4,
  delta = 0.1, tT = 0.6, phiL = 0.2, phiU = 0.8,
  parE = c(1, 1), parS = c(5, 25), ns = 10, nr = TRUE
)
res2$oc

# note that d has to be a positive value;
res3 <- ocPredprobDist(c(10, 20, 30), 0.4,
  delta = 0.1, tT = 0.6, phiL = 0.2, phiU = 0.8,
  parE = c(1, 1), parS = c(5, 25), ns = 10, nr = TRUE, d = 5
)
res3$oc


# finally, we can also have separate specification of efficacy and futility analyses. E.g.
# here have futility decisions only at the final analysis:


res4 <- ocPredprobDist(c(10, 20, 30), 0.4,
  delta = 0.1, tT = 0.6, phiL = 0.2, phiU = 0.8,
  parE = c(1, 1), parS = c(5, 25), ns = 10, nr = TRUE, d = 5, nnF = c(30)
)
res4$oc
# compared to res3, we see that there is no early futility stopping anymore,
# and the overall probability for stopping for futility (which is the type II error here)
# is much lower.

# 2)
# to examine operating characteristics of a phase Ib design with a
# seperate rule for futility at final analysis
# denote treatment response rate as RRS
# trial success is defined as: P(RRS>control+delta)>tT
# trial failure is defined as: P(RRS<control+deltaFu)>tFu
# (gray zone could occur in the final analysis)

set.seed(20)
res5 <- ocPredprobDist(c(10, 20, 30), 0.4,
  delta = 0.1, deltaFu = 0.05, tT = 0.6, tFu = 0.6, phiFu = 0.8,
  phiU = 0.8, parE = c(1, 1), parS = c(5, 25), ns = 10
)
res5$oc


# when tFu=1-tT and phiFu=1-phiL, this decisian rule (res6) is the same as the res1
set.seed(20)
res6 <- ocPredprobDist(c(10, 20, 30), 0.4,
  delta = 0.1, tT = 0.6, tFu = 0.4, phiFu = 0.8, phiU = 0.8,
  parE = c(1, 1), parS = c(5, 25), ns = 10
)
res6$oc

# If both phiL and phiFu arguments are specified, phiL will be overwrite by 1-phiFu.
# See warning message.
set.seed(20)
res7 <- ocPredprobDist(c(10, 20, 30), 0.4,
  delta = 0.1, tT = 0.6, tFu = 0.4, phiL = 0.1, phiFu = 0.8, phiU = 0.8,
  parE = c(1, 1), parS = c(5, 25), ns = 10
)
res7$oc
