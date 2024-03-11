# Here we illustrate an example for Decision 1 with the following assumptions :
# Efficacy Looks and Futility looks are identical at sample size of 10, 20 and 30.
# True response rate or truep of the treatment group = 40%
# Desired difference to Standard of Care for Efficacy and Futility = 10%
# The following are the Final Stop rules respectively :
# - Final look for Efficacy: Pr( response rate + deltaE > 25% ) > 60% or P(response rate + deltaE > p0) > tT
# - Final look for Futility: Pr( response rate - deltaF < 25% ) < 60% or P(response rate + deltaF > p0) < tT
# - Interim look for Efficacy: Pr( success at final ) > 80% or P(success at final) > phiU
# - Interim look for Futility: Pr( failure at final ) < 20% or P(success at final) < phiL
# We assume a prior of treatment arm parE = Beta(1,1), unless otherwise indicated.

set.seed(20)
res1 <- ocPredprobDist(
  xS = 0,
  nS = 0,
  nnE = c(10, 20, 30),
  truep = 0.40,
  deltaE = 0.10,
  deltaF = 0.10,
  relativeDelta = FALSE,
  tT = 0.6,
  phiU = 0.80,
  phiL = 0.20,
  parE = c(1, 1),
  parS = c(5, 25),
  weights = 1,
  weightsS = 1,
  sim = 50,
  wiggle = FALSE,
  decision1 = TRUE
)
res1$oc


# Here we illustrate an example for Decision 1 with the following assumptions :
# Efficacy Looks are at sample size of 10, 20 and 30.Futility is only at sample size of 10
# where wiggle is allowed.
# True response rate or truep of the treatment group = 40%
# Desired difference to Standard of Care for Efficacy and Futility = 10%
# The following are the Final Stop rules respectively :
# - Final look for Efficacy: Pr( response rate + deltaE > 25% ) > 60% or P(response rate + deltaE > p0) > tT
# - Final look for Futility: Pr( response rate - deltaF < 25% ) < 60% or P(response rate + deltaF > p0) < tT
# - Interim look for Efficacy: Pr( success at final ) > 80% or P(success at final) > phiU
# - Interim look for Futility: Pr( failure at final ) < 20% or P(success at final) < phiL
# We assume a prior of treatment arm parE = Beta(1,1), unless otherwise indicated.

set.seed(20)
res1 <- ocPredprobDist( # we're not suppose to have grey zone here
  xS = 0,
  nS = 0,
  nnE = c(10, 20, 30),
  truep = 0.40,
  deltaE = 0.10,
  deltaF = 0.10,
  relativeDelta = FALSE,
  tT = 0.6,
  phiU = 0.80,
  phiL = 0.20,
  parE = c(1, 1),
  parS = c(5, 25),
  weights = 1,
  weightsS = 1,
  sim = 50,
  nnF = 20,
  wiggle = TRUE,
  decision1 = TRUE
)
res1$oc


# Here we illustrate an example for Decision 2 with the following assumptions :
# Efficacy Looks and Futility looks are identical at sample size of 10, 20 and 30
# where wiggle is allowed.
# True response rate or truep of the treatment group = 40%
# Desired difference to Standard of Care for Efficacy and Futility = 10%
# The following are the Final Stop rules respectively :
# - Final look for Efficacy: Pr( response rate + deltaE > 25% ) > 60% or P(response rate + deltaE > p0) > tT
# - Final look for Futility: Pr( response rate - deltaF < 25% ) < 60% or P(response rate + deltaF > p0) < tT
# - Interim look for Efficacy: Pr( success at final ) > 80% or P(success at final) > phiU
# - Interim look for Futility: Pr( failure at final ) < 20% or P(success at final) < phiL
# We assume a prior of treatment arm parE = Beta(1,1), unless otherwise indicated.

set.seed(20)
res1 <- ocPredprobDist( # we're not suppose to have grey zone here
  xS = 0,
  nS = 0,
  nnE = c(10, 20, 30),
  truep = 0.40,
  deltaE = 0.10,
  deltaF = 0.10,
  relativeDelta = FALSE,
  tT = 0.6,
  phiU = 0.80,
  phiFu = 0.7,
  parE = c(1, 1),
  parS = c(5, 25),
  weights = 1,
  weightsS = 1,
  sim = 50,
  nnF = c(10, 20, 30),
  wiggle = TRUE,
  decision1 = FALSE
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


# when tFu=1-tT and phiFu=1-phiL, this decision rule (res6) is the same as the res1
set.seed(20)
res6 <- ocPredprobDist(c(10, 20, 30), 0.4,
  delta = 0.1, tT = 0.6, tFu = 0.4, phiFu = 0.8, phiU = 0.8,
  parE = c(1, 1), parS = c(5, 25), ns = 10
)
res6$oc

# If both phiL and phiFu arguments are specified, phiL will be overwrite by 1-phiFu.
# See warning message (which we suppress here to let checks pass).
set.seed(20)
res7 <- suppressWarnings(ocPredprobDist(c(10, 20, 30), 0.4,
  delta = 0.1, tT = 0.6, tFu = 0.4, phiL = 0.1, phiFu = 0.8, phiU = 0.8,
  parE = c(1, 1), parS = c(5, 25), ns = 10
))
res7$oc
