# example similar to Lee and Liu:
postprobDist(x = 16, n = 23, parE = c(0.6, 0.4), parS = c(0.6, 0.4), delta = 0.1, relativeDelta = FALSE)

# when relativeDelta is used.
postprobDist(x = 16, n = 23, parE = c(0.6, 0.4), parS = c(0.6, 0.4), delta = 0.1, relativeDelta = TRUE)

# these two should give the same result:
postprobDist(
  x = 27, n = 34,
  xS = 0, nS = 0,
  delta = 0.15,
  parE = c(1, 1),
  parS = c(50007530, 49924090)
)

postprob(x = 27, n = 34, p = 0.65, parE = c(1, 1))
# ok, almost

# try out mixtures:
# play around with the beta parameters and weights to
# get a feeling.
# Note that very extreme beta parameters do no longer increase
# the return value, because then that mixture component is too
# unlikely a posteriori
postprobDist(
  x = 16, n = 23,
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
# try these examples

# 1. Experimental arm only (strictly single arm trial), uniform prior in both E and S arms.
postprobDist(
  x = 16,
  n = 23,
  xS = 0,
  nS = 0,
  delta = 0,
  relativeDelta = FALSE,
  parE = c(1, 1),
  parS = c(1, 1),
  weightsS = c(1)
)
# 2. Experimental arm and SOC, uniform prior in both E and S arms.
postprobDist(
  x = 16,
  n = 20,
  xS = 10,
  nS = 20,
  delta = 0,
  relativeDelta = FALSE,
  parE = c(1, 1),
  parS = c(1, 1),
  weightsS = c(1)
)
# 3. Experimental and SOC arm, with beta mix prior for S arms with 50:50 weighting, uniform for E.
postprobDist(
  x = 16,
  n = 20,
  xS = 10,
  nS = 20,
  delta = 0,
  relativeDelta = FALSE,
  parE = c(1, 1),
  weights = c(1),
  parS = rbind(c(4, 5), c(1, 3)),
  weightsS = c(1, 2)
)

# 3b. Experimental and SOC arm, with beta mix prior for S arm, uniform for E.
# The SOC arm is of 3 priors, therefore 3 sets of beta parameters, and 3 weights.
# We can have weights exceeding 1 because it will be internally normalised to sum to 1.
postprobDist(
  x = 16,
  n = 20,
  xS = 10,
  nS = 20,
  delta = 0,
  relativeDelta = FALSE,
  parE = c(1, 1),
  parS = rbind(c(4, 5), c(2, 3), c(4, 4)),
  weightsS = c(2, 5, 3)
)

# 4. Experimental and SOC arm, with beta mix prior for both arms.
# For each of the SOC arm is of 3 priors, therefore 3 sets of beta parameters, and 3 weights.
postprobDist(
  x = 16,
  n = 20,
  xS = 10,
  nS = 20,
  delta = 0,
  relativeDelta = FALSE,
  parE = rbind(c(1, 1), c(3, 4), c(8, 9)),
  weights = c(5, 3, 2),
  parS = rbind(c(4, 5), c(2, 3), c(4, 4)),
  weightsS = c(2, 5, 3)
)
