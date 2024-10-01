# Example taken from Lee and Liu (2008) :
# We observed 16 successes out of 23 patients
# We set a threshold of 0.60
# Assume a beta(0.6,0.4) prior for P_E
# Posterior will be a beta(16.6, 7.4), Pr(P_E > p | data) = 0.836

postprob(x = 16, n = 23, p = 0.60, par = c(0.6, 0.4))

# We could instead specify a mixture prior
# 2 component beta mixture prior :
# i.e., P_E ~ 0.6*beta(0.6,0.4) + 0.4*beta(1,1) and Pr(P_E > p | data) = 0.823
postprob(
  x = 16, n = 23, p = 0.60,
  par =
    rbind(
      c(0.6, 0.4),
      c(1, 1)
    ),
  weights = c(0.6, 0.4)
)

postprob(
  x = 0:23, n = 23, p = 0.60,
  par =
    rbind(
      c(0.6, 0.4),
      c(1, 1)
    ),
  weights = c(0.6, 0.4)
)
