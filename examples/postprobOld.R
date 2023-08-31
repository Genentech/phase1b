# Example taken from Lee & Liu (2006)
# We observed 16 successes out of 23 patients # should we write this in the documentation
# We set a threshold of 0.60
# Assume a beta(0.6,0.4) prior for P_E
# Posterior will be a beta(16.6,22.8), Pr(P_E > p | data) = 0.8358808


# Example taken from Lee and Liu (2006)
postprobOld(x = 16, n = 23, p = 0.60, a = 0.6, b = 0.4)
# Interpretation : The probability 16 of 23 successes is greater than 60 % threshold is approximately 84 %
