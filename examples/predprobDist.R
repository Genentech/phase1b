## The original Lee and Liu (Table 1) example:
## Nmax=40, x=16, n=23, beta(0.6,0.4) prior distribution,
## thetaT=0.9. The control response rate is 60%:
predprob(
  x = 16, n = 23, Nmax = 40, p = 0.6, thetaT = 0.9,
  parE = c(0.6, 0.4)
)
## So the predictive probability is 56.6%. 12 responses
## in the remaining 17 patients are required for success.

## Now to the modified example, where the control response
## rate p has a beta(7, 11) distribution.
## For example if the 60% threshold stems from a trial
## with 10 patients, of which 6 were observed with response.
predprobDist(
  x = 16, n = 23, Nmax = 40, delta = 0, thetaT = 0.9,
  parE = c(0.6, 0.4), parS = c(7, 11)
)
## Now only 11 responses are needed in the remaining 17 patients
## in order to have a successful trial.
## Also, the predictive probability for a Go decision is now
## 70.8% instead of merely 56.6%.

## If the response threshold of 60% stems from an SOC estimate (e.g. 50% = 5/10)
## and a margin of e.g. 10%, then the margin can be specified with
## the delta argument:
predprobDist(
  x = 16, n = 23, Nmax = 40, delta = 0.1, thetaT = 0.9,
  parE = c(1, 1), parS = c(6, 11)
)
## This again changes the result: Only 10 future responses are required,
## the predictive probability of success is now 80%.

## Next we will use a beta mixture prior, with a weight of 1/3 for an
## informative beta(50, 10) prior, on the experimental response rate:
predprobDist(
  x = 16, n = 23, Nmax = 40, delta = 0.1, thetaT = 0.9,
  parE = rbind(c(1, 1), c(50, 10)),
  weights = c(2, 1), parS = c(6, 11)
)
## Now still 10 future responses are required for success, but the predictive
## success probability is higher (87.2%).

## We can also have additional controls in our trial. For example
## assume that in the interim analysis we had 10 control patients observed
## with 5 responses out of 20 in total at the end.
## We also had historical control data (say 60 patients of
## which 20 responded), but in order to be robust against changes in the response
# rate over time, we will again use a beta mixture, putting only 1/3 of weight
## on the historical controls:
predprobDist(
  x = 16, n = 23, xS = 5, nS = 10,
  Nmax = 40, NmaxControl = 20,
  delta = 0.1, thetaT = 0.9,
  parE = rbind(c(1, 1), c(50, 10)),
  weights = c(2, 1),
  parS = rbind(c(1, 1), c(20, 40)),
  weightsS = c(2, 1)
)
## Now we only have 59.9% predictive probability of success. The bgttheta matrix
## lists the cases where we would have success, for all possible combinations
## of future responses in the control and experimental arms. We see that the
## success threshold for experimental responses depends on the number of
## control responses.
