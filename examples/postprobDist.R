# An example similar to Lee and Liu (2008).
postprobDist(
  x = 16,
  n = 23,
  parE = c(0.6, 0.4),
  parS = c(0.6, 0.4),
  delta = 0.1,
  relativeDelta = FALSE
)

# For a sequence of success outcomes for Experimental arm.
postprobDist(
  x = c(16, 17),
  n = 23,
  parE = c(0.6, 0.4),
  parS = c(0.6, 0.4),
  delta = 0.1,
  relativeDelta = FALSE
)

# When we use a relative difference and look at several possible number of responses.
postprobDist(
  x = 1:23,
  n = 23,
  parE = c(0.2, 0.8),
  parS = c(0.6, 0.4),
  delta = 0.1,
  relativeDelta = TRUE
)

# When we use beta mixtures for both the Experimental and SOC arms.
postprobDist(
  x = 16,
  n = 23,
  parE =
    rbind(
      c(0.6, 0.4),
      c(10, 20)
    ),
  parS =
    rbind(
      c(0.6, 0.4),
      c(10, 10)
    ),
  weightsS = c(1, 3),
  delta = 0.1
)

# Experimental arm only (strictly single arm trial), uniform prior in Experimental arm.
# Non-uniform Prior used for SOC arm as no precedent data.
postprobDist(
  x = 16,
  n = 23,
  xS = 0,
  nS = 0,
  delta = 0,
  relativeDelta = FALSE,
  parS = c(2, 3),
  weightsS = 1
)
# Experimental arm and SOC, uniform prior in both E and S arms, default setting used.
postprobDist(
  x = 16,
  n = 20,
  xS = 10,
  nS = 20,
  delta = 0,
  relativeDelta = FALSE,
  weightsS = 1
)

# Experimental and SOC arm, with beta mix prior for both arms.
# For each of the SOC arm is of 3 priors, therefore 3 sets of beta parameters, and 3 weights.
postprobDist(
  x = 16,
  n = 20,
  xS = 10,
  nS = 20,
  delta = 0.1,
  relativeDelta = TRUE,
  parE = rbind(c(1, 1), c(3, 4), c(8, 9)),
  weights = c(5, 3, 2),
  parS = rbind(c(4, 5), c(2, 3), c(4, 4)),
  weightsS = c(2, 5, 3)
)
