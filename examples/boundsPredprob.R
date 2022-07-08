## 40 pts trial with interim looks after each 10 pts.,
## final efficacy decision if more than 80% probability to be above 20% ORR,
## final futility decision otherwise.
## Interim efficacy decision if more than 90% predictive probability reach this,
## interim futility decision if less than 10% predictive probability.
## Uniform prior (i.e. beta(1, 1)) on the ORR:
boundsPredprob(
  nvec = c(10, 20, 30, 40), p = 0.20, tT = 0.80,
  phiL = 0.10, phiU = 0.90, a = 1, b = 1
)
## From this we see e.g. that at the first IA at 10 pts, we would stop for futility
## if no patient responded, and for efficacy if 4 or more pts responded.
