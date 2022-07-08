## The original Lee and Liu (Table 1) example:
## Nmax=40, x=16, n=23, beta(0.6,0.4) prior distribution,
## thetaT=0.9. The control response rate is 60%. Assume
## that at the end of the trial we would stop for futility
## if the posterior probability is below 50%. Then the predictive
## probability of failing is:
predprobFail(
  x = 16, n = 23, Nmax = 40, p = 0.6, thetaF = 0.5,
  parE = c(0.6, 0.4)
)
## So only 5%, and from the table we can see that we would fail
## with 7 or less responses in the coming 17 patients
