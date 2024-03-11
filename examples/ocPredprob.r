# Here we illustrate an example for Decision 1 with the following assumptions :
# True response rate or truep of the treatment group = 40%
# The following are the Final Stop rules respectively :
# - Final look for Efficacy: Pr( response rate > 25% ) > 60% or P(response rate > p0) > tT
# - Final look for Futility: Pr( response rate < 25% ) < 60% or P(response rate > p0) < tT
# - Interim look for Efficacy: Pr( success at final ) > 80% or P(success at final) > phiU
# - Interim look for Futility: Pr( failure at final ) < 20% or P(success at final) < phiL
# We assume a prior of treatment arm parE = Beta(1,1), unless otherwise indicated.

# Decision 1 with no wiggle.
set.seed(20)
result <- ocPredprob(
  nnE = c(10, 20),
  truep = 0.4,
  p0 = 0.25,
  tT = 0.6,
  phiL = 0.2,
  phiU = 0.8,
  parE = c(1, 1),
  sim = 100,
  wiggle = FALSE,
  decision1 = TRUE
)
result$oc

# Decision 1 with wiggle.
result <- ocPredprob(
  nnE = c(10, 20),
  truep = 0.4,
  p0 = 0.25,
  tT = 0.6,
  phiL = 0.2,
  phiU = 0.8,
  parE = c(1, 1),
  sim = 50,
  wiggle = TRUE,
  nnF = c(10, 20),
  decision1 = TRUE
)
result$oc

# Decision 1 with separate Futility and Efficacy looks at interim and final without wiggle.
result <- ocPredprob(
  nnE = c(10, 25, 30),
  truep = 0.4,
  p0 = 0.25,
  p1 = 0.2,
  tT = 0.6,
  phiL = 0.2,
  phiU = 0.8,
  parE = c(1, 1),
  sim = 50,
  wiggle = FALSE,
  nnF = c(10, 15, 20),
  decision1 = TRUE
)
result$oc

# Decision 1 with separate Futility and Efficacy looks at interim and final with wiggle.
result <- ocPredprob(
  nnE = c(10, 25, 30),
  truep = 0.4,
  p0 = 0.25,
  p1 = 0.2,
  tT = 0.6,
  phiL = 0.2,
  phiU = 0.8,
  parE = c(1, 1),
  sim = 50,
  wiggle = TRUE,
  nnF = c(10, 15, 20),
  decision1 = TRUE
)
result$oc

# Here we illustrate an example for Decision 2 with the following assumptions :
# True response rate or truep of the treatment group = 60%
# The following are the Final Stop rules respectively :
# - Final look for Efficacy: Pr( response rate > 25% ) > 60% or P(response rate > p0) > tT
# - Final look for Futility: Pr( response rate < 25% ) < 60% or P(response rate < p1) > tF
# - Interim look for Efficacy: Pr( success at final ) > 80% or P(success at final) > phiU
# - Interim look for Futility: Pr( failure at final ) > 80% or P(failure at final) > phiFu
# We assume a prior of treatment arm parE = Beta(1,1), unless otherwise indicated.

# Decision 2 without wiggle.
result <- ocPredprob(
  nnE = c(10, 20),
  truep = 0.6,
  p0 = 0.25,
  p1 = 0.25,
  tT = 0.6,
  tF = 0.6,
  phiU = 0.8,
  phiFu = 0.8,
  parE = c(1, 1),
  sim = 50,
  wiggle = FALSE,
  nnF = c(10, 20),
  decision1 = FALSE
)
result$oc

# Decision 2 with wiggle and with Futility only at final with non-uniform beta prior parE.
result <- ocPredprob(
  nnE = c(10, 25, 30),
  truep = 0.6,
  p0 = 0.25,
  p1 = 0.25,
  tT = 0.6,
  tF = 0.6,
  phiL = 0.8,
  phiU = 0.8,
  parE = c(11, 19),
  sim = 50,
  wiggle = TRUE,
  nnF = 30,
  decision1 = FALSE
)
result$oc
