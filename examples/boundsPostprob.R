# 40 pts trial with interim looks after each 10 pts.,
# Efficacy decision if more than 60% probability to be above 20% ORR,
# Futility decision if less than 60% probability to be below 20% ORR,
# with uniform prior (i.e. beta(1, 1)) on the ORR:
boundsPostprob(
  nvec = c(10, 20, 30, 40),
  p0 = 0.20,
  p1 = 0.20,
  tL = 0.20,
  tU = 0.20,
  a = 1,
  b = 1
)
