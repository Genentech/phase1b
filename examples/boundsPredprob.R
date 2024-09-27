# 40 pts trial with interim looks after each 10 patients.
# Final efficacy decision if more than 80% probability to be above 20% ORR,
# Final futility decision otherwise.
# Interim efficacy decision if more than 90% predictive probability reach this or
# Efficacy look Pr(Pr(P > p0 | x, Y, a, b) >= tT | x) >= phiU,
# Interim futility decision if less than 10% predictive probability or
# Futility look Pr(Pr(P > p0 | x, Y, a, b) >= tT | x) =< phiL
# Uniform prior (i.e. beta(1, 1)) on the ORR:
boundsPredprob(
  looks = c(10, 20, 30, 40),
  p0 = 0.20,
  tT = 0.80,
  phiL = 0.10,
  phiU = 0.90
)
# From this we see e.g. that at the first IA at 10 patients, we would stop for futility
# if no patient responded, and for efficacy if 4 or more patients responded.
