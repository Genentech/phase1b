# Here we illustrate an example for Decision 1 with the following assumptions :
# Efficacy Looks and Futility looks are identical at sample size of 10, 20 and 30.
# True response rate or truep of the treatment group = 40%
# Desired difference to Standard of Care for Efficacy and Futility = 10%
# The following are the Final Stop rules respectively :
# - Final look for Efficacy: Pr( RR + deltaE > 25% ) > 60%
# - Final look for Futility: Pr( RR + deltaF < 25% ) < 60%
# - Interim look for Efficacy: Pr( success at final ) > 80%
# - Interim look for Futility: Pr( failure at final ) < 20%
# We assume a prior of treatment arm parE = Beta(1,1), unless otherwise indicated.

set.seed(20)
result <- ocPredprobDist(
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
  sim = 15,
  wiggle = FALSE,
  decision1 = TRUE
)
result$oc


# Here we illustrate an example for Decision 1 with the following assumptions :
# Efficacy Looks are at sample size of 10, 20 and 30 where sample looks are allowed to wiggle.
# Futility is only at sample size of 20 and 30. We won't do a futility at 10 patients.
# True response rate or truep of the treatment group = 40%
# Desired difference to Standard of Care for Efficacy and Futility is 10% and -10% respectively.
# Grey zone occurs due to different posterior probability distribution in the Efficacy and Futility rules.
# Delta calculation is absolute case. The following are the Final Stop rules respectively :
# - Final look for Efficacy: Pr( RR + deltaE > 25% ) > 60%
# - Final look for Futility: Pr( RR + deltaF < 25% ) < 60%
# - Interim look for Efficacy: Pr( success at final ) > 80%
# - Interim look for Futility: Pr( failure at final ) < 20%
# We assume a prior of treatment arm parE = Beta(1,1), unless otherwise indicated.
#
set.seed(20)
result <- ocPredprobDist(
  nnE = c(10, 20, 30),
  truep = 0.40,
  deltaE = 0.10,
  deltaF = -0.10,
  relativeDelta = FALSE,
  tT = 0.6,
  phiU = 0.80,
  phiL = 0.20,
  parE = c(1, 1),
  parS = c(5, 25),
  weights = 1,
  weightsS = 1,
  sim = 15,
  nnF = c(20, 30),
  wiggle = TRUE,
  decision1 = TRUE
)
result$oc

# Here we illustrate an example for Decision 2 with the following assumptions :
# Efficacy Looks and Futility looks are identical at sample size of 10, 20 and 30
# where sample looks are allowed to wiggle.
# True response rate or truep of the treatment group = 40%
# Desired difference to Standard of Care for Efficacy and Futility = 50%
# Delta calculation is absolute case. The following are the Final Stop rules respectively :
# - Final look for Efficacy: Pr( RR + deltaE > 25% ) > 60% or P(RR + deltaE > p0) > tT
# - Final look for Futility: Pr( RR + deltaF < 25% ) < 60% or P(RR + deltaF > p0) < tT
# - Interim look for Efficacy: Pr( success at final ) > 80% or P(success at final) > phiU
# - Interim look for Futility: Pr( failure at final ) < 20% or
#   P(success at final) < phiL
# We assume a prior of treatment arm parE = Beta(1,1), unless otherwise indicated.

set.seed(20)
result <- ocPredprobDist(
  nnE = c(10, 20, 30),
  truep = 0.40,
  deltaE = 0.5,
  deltaF = 0.5,
  relativeDelta = FALSE,
  tT = 0.6,
  phiU = 0.80,
  phiFu = 0.7,
  parE = c(1, 1),
  parS = c(5, 25),
  weights = 1,
  weightsS = 1,
  sim = 15,
  nnF = c(10, 20, 30),
  wiggle = TRUE,
  decision1 = FALSE
)
result$oc

# Here we illustrate an example for Decision 2 with the following assumptions :
# Efficacy Looks and Futility looks are identical at sample size of 10, 20 and 30
# where wiggle is allowed.
# True response rate or truep of the treatment group = 40%
# Desired difference to Standard of Care for Efficacy and Futility = 50%
# Delta calculation is relative case. The following are the Final Stop rules respectively :
# - Final look for Efficacy: P( P_S + (1-P_S)*deltaE > 25% ) > 60%
# - Final look for Futility: P( P_S + (1-P_S)*deltaEF < 25% ) < 60%
# - Interim look for Efficacy: P( success at final ) > 80%
# - Interim look for Futility: P( failure at final ) < 20%
# We assume a prior of treatment arm parE = Beta(1,1), unless otherwise indicated.

set.seed(20)
result <- ocPredprobDist(
  nnE = c(10, 20, 30),
  truep = 0.40,
  deltaE = 0.5,
  deltaF = 0.5,
  relativeDelta = TRUE,
  tT = 0.6,
  phiU = 0.80,
  phiFu = 0.7,
  parE = c(1, 1),
  parS = c(5, 25),
  weights = 1,
  weightsS = 1,
  sim = 15,
  nnF = c(10, 20, 30),
  wiggle = TRUE,
  decision1 = FALSE
)
result$oc
