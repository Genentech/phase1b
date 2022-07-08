## The original Lee and Liu (Table 1) example:
## Nmax=40, x=16, n=23, beta(0.6,0.4) prior distribution,
## thetaT=0.9. The control response rate is 60%:
predprob(
  x = 16, n = 23, Nmax = 40, p = 0.6, thetaT = 0.9,
  parE = c(0.6, 0.4)
)
## So the predictive probability is 56.6%. 12 responses
## in the remaining 17 patients are required for success.

## Lowering/Increasing the probability threshold thetaT of course increases
## /decreases the predictive probability of success, respectively:
predprob(
  x = 16, n = 23, Nmax = 40, p = 0.6, thetaT = 0.8,
  parE = c(0.6, 0.4)
)
## 70.8%
predprob(
  x = 16, n = 23, Nmax = 40, p = 0.6, thetaT = 0.95,
  parE = c(0.6, 0.4)
)
## 40.7%

## Instead of a fixed beta prior on the response rate, dynamic borrowing
## from a previous data set is possible by using a beta-mixture prior.
## For example, assume we have a previous trial where we saw 25 responses
## in 40 patients. We would like to use information worth 10 patients in our
## trial, but be robust against deviations from that response rate (maybe
## because it was conducted in slightly different disease or patients).
## Then we can use a beta mixture prior, with the informative component getting
## weight of 1/4:
predprob(
  x = 20, n = 23, Nmax = 40, p = 0.6, thetaT = 0.9,
  parE = rbind(c(1, 1), c(25, 15)),
  weights = c(3, 1)
)
## Since the response rate in the historical dataset is lower (62.5% < 69.6%)
## than in the trial, but similar, we notice that the predictive probability
## of success is now lower.
