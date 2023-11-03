# example similar to Lee and Liu:
postprobDist(x = 16, n = 23, parE = c(0.6, 0.4), parS = c(0.6, 0.4))

## these two should give the same result:
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
  weightsS = c(1, 3)
)
# try these examples

# 1. Experimental arm only, uniform prior in both E and S arms
# 2. Experimental arm and SOC
# 3. Experimental arm only, with beta mix prior for S arms, uniform for E
