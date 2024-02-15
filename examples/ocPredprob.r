# For various Look locations at Interim and Final, we have the following assumptions :
# True response rate or truep of the treatment group = 40%
# The following are the Final Stop rules respectively :
# - Final look for Efficacy: Pr( response rate > 25% ) > 60%
# - Final look for Futility: Pr( response rate < 25% ) < 60%
# - Interim look for Efficacy: Pr( success at final ) > 80%
# - Interim look for Futility: Pr( failure at final ) < 20%, or > 80 % for Decision 2
# We assume a prior of treatment arm parE = Beta(1,1), unless otherwise indicated.
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

# For when random distance is allowed, i.e. `wiggle = TRUE`.
ocPredprob(
  nnE = c(10, 20),
  truep = 0.6,
  p0 = 0.2, # no p1 as this is decision 1
  tT = 0.6, # no tF as it's for decision 2
  phiL = 0.2,
  phiU = 0.8,
  parE = c(1, 1),
  sim = 50,
  wiggle = TRUE,
  nnF = c(10, 20),
  decision1 = TRUE
)

# For Decision 2, where the Futility stop rules in interim and final are
# P(failure at final) > PhiFu
# P(response rate > p1) > tF
ocPredprob(
  nnE = c(10, 20),
  truep = 0.6,
  p0 = 0.2,
  p1 = 0.2,
  tT = 0.6,
  tF = 0.4,
  phiU = 0.2,
  phiFu = 0.8,
  parE = c(1, 1),
  sim = 50,
  wiggle = FALSE,
  nnF = c(10, 20),
  decision1 = FALSE
)

# Separate Futility and Efficacy looks at interim and final.
ocPredprob(
  nnE = c(10, 25, 30),
  truep = 0.6,
  p0 = 0.2,
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

# Separate Futility and Efficacy looks at interim and final with wiggle.
ocPredprob(
  nnE = c(10, 25, 30),
  truep = 0.6,
  p0 = 0.2,
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

# Futility only at final with non-uniform beta prior parE.
ocPredprob(
  nnE = c(10, 25, 30),
  truep = 0.6,
  p0 = 0.2,
  p1 = 0.2,
  tT = 0.6,
  phiL = 0.2,
  phiU = 0.8,
  parE = c(11, 19),
  sim = 50,
  wiggle = TRUE,
  nnF = 30,
  decision1 = TRUE
)
