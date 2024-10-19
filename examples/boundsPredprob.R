# 40 pts trial with interim looks after each 10 patients.
# Final efficacy decision if more than 80% probability to be above 20% ORR,
# Final futility decision otherwise.
# Interim efficacy decision if more than 90% predictive probability reach this or
# Efficacy look Pr(Pr(P > p0 | x, Y) >= tT | x) >= phiU,
# Interim futility decision if less than 10% predictive probability or
# Futility look Pr(Pr(P > p0 | x, Y) >= tT | x) =< phiL
# Uniform prior (i.e. beta(1, 1)) on the ORR:
boundsPredprob(
  looks = c(10, 20, 30, 40),
  p0 = 0.20,
  tT = 0.80,
  phiL = 0.60,
  phiU = 0.90
)

# 25 pts trial with interim looks at 7 and 15 pts.
# Efficacy decision if more than 80% probability to be above 20% ORR,
# Final futility decision otherwise.
# Interim efficacy decision if more than 90% predictive probability reach this or
# Efficacy look Pr(Pr(P > p0 | x, Y) >= tT | x) >= phiU,
# Interim futility decision if less than 60% predictive probability or
# Futility look Pr(Pr(P > p0 | x, Y) >= tT | x) =< phi
# with mixed prior and weights:
boundsPredprob(
  looks = c(7, 15, 25),
  p0 = 0.20,
  tT = 0.80,
  phiL = 0.60,
  phiU = 0.90,
  parE = cbind(c(1, 1), c(3, 10)),
  weights = c(0.2, 0.8)
)
