## 40 pts trial with interim looks after each 10 pts.,
## efficacy decision if more than 90% probability to be above 20% ORR,
## futility decision if less than 10% probability to be above 20% ORR,
## with uniform prior (i.e. beta(1, 1)) on the ORR:
boundsPostprob(
  nvec = c(10, 20, 30, 40), p0 = 0.20,
  tL = 0.10, tU = 0.90, a = 1, b = 1
)
## From this we see e.g. that at the third IA at 30 pts, we would stop for futility
## if 5 or less patients responded, and for efficacy if 9 or more pts responded.
