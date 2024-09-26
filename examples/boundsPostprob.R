# 40 pts trial with interim looks after each 10 pts.,
# Efficacy decision if more than 80% probability to be above 20% ORR,
# Futility decision if more than 60% probability to be below 15% ORR,
# with uniform prior (i.e. beta(1, 1)) on the ORR:
boundsPostprob(
  looks = c(10, 20, 30, 40),
  p0 = 0.15,
  p1 = 0.20,
  tL = 0.60,
  tU = 0.80,
  parE = c(1, 1)
)

# 40 pts trial with interim looks at 7 and 20 pts.
# Efficacy decision if more than 80% probability to be above 20% ORR,
# Futility decision if more than 60% probability to be below 15% ORR,
# with mixed prior and weights:
boundsPostprob(
  looks = c(7, 20, 40),
  p0 = 0.15,
  p1 = 0.20,
  tL = 0.60,
  tU = 0.80,
  parE = rbind(c(1, 19), c(2, 10)),
  weights = c(0.2, 0.8)
)
