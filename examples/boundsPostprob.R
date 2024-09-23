# 40 pts trial with interim looks after each 10 pts.,
# Efficacy decision if more than 80% probability to be above 20% ORR,
# Futility decision if more than 60% probability to be below 20% ORR,
# with uniform prior (i.e. beta(1, 1)) on the ORR:
boundsPostprob(
  nvec = c(10, 20, 30, 40),
  p0 = 0.15,
  p1 = 0.20,
  tL = 0.60,
  tU = 0.80,
  a = 1,
  b = 1
)
