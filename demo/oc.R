## load the package
library(phase1b)

## show how to get to the help pages:
## Help -> type in phase1b in the search
## go to postprob

## 15 patients in total, 12 responses. What's the probability
## that we are above 50% response rate? uniform prior (Beta(1, 1)) used
postprob(x = 12, n = 15, p = 0.50, parE = c(1, 1))

## show tab completion/interactive help in RStudio
help(package = "phase1b")

## if only 8 responses
postprob(x = 8, n = 15, p = 0.50, parE = c(1, 1))

## and if only 4
postprob(x = 4, n = 15, p = 0.50, parE = c(1, 1))


## now let's assume we will have maximum 40 patients in the trial
## we will declare success if we are above 80% postprob.
## what's the probability that the trial will be successful?
## => predictive probability needs to be calculated.
predprob(x = 12, n = 15, Nmax = 40, p = 0.50, theta = 0.80, parE = c(1, 1))

## if we have a prior distribution on the SOC (not just point 0.5):
as.numeric(predprobDist(
  x = 12, n = 15, Nmax = 40, theta = 0.80, parE = c(1, 1),
  parS = c(11, 11)
)) ## 10/10 responded in SOC + 1/1 from uniform prior
## result is slightly lower than before

## operating characteristics for the predictive probability design:
z <- ocPredprobDist(
  nn = c(15, 25, 40), ## Nmax implicitly given here
  parE = c(1, 1), parS = c(11, 11),
  tT = 0.80, ## == theta from before
  p = 0.65, ## this is the assumed true response rate
  phiL = 0.10, ## when would we stop for futility?
  phiU = 0.90, ## when would we stop for efficacy?
  ns = 100
) ## number of simulated trials
## inspect z:
names(z)

## OCs are here:
z$oc

## for the simulated trials, the details:
z$Decision
z$SampleSize

## can e.g. summarize sample sizes:
barplot(table(z$SampleSize) / 100)

## now again posterior probability design.
## let's look at the implied decision bounds

## list example boundaries for 3 look locations
## along with resulting posterior probability and CI limit
boundsPostprob(
  nvec = c(15, 25, 40),
  p0 = 0.50, p1 = 0.50,
  thetaL = 0.20, thetaU = 0.80,
  a = 1, b = 1
)
## xL: bound for futility decision.
## pL: associated response rate estimate
## postL: posterior probability for success
## UciL: upper confidence interval limit for response rate
## similarly for the "U" (activity decision) results

## graphically: plot decision regions (just the xL's and xU's)
## for all look locations starting at 15
plotBounds(
  boundsPostprob(
    nvec = c(15:40), p0 = 0.50, p1 = 0.50,
    thetaL = 0.20, thetaU = 0.80, a = 1, b = 1
  ),
  yt = "p"
)


## Reproduce plots from Presentation

## Boundary Plots:
## Posterior Probability example

z1 <- boundsPostprob(10:40, p0 = 0.50, p1 = 0.50, thetaL = 0.20, thetaU = 0.80, a = 1, b = 1)
z2 <- boundsPostprob(10:40, p0 = 0.50, p1 = 0.50, thetaL = 0.10, thetaU = 0.90, a = 1, b = 1)
z3 <- boundsPostprob(10:40, p0 = 0.50, p1 = 0.50, thetaL = 0.05, thetaU = 0.95, a = 1, b = 1)
z4 <- boundsPostprob(10:40, p0 = 0.50, p1 = 0.50, thetaL = 0.01, thetaU = 0.99, a = 1, b = 1)
z5 <- boundsPostprob(10:40, p0 = 0.50, p1 = 0.50, thetaL = 0.001, thetaU = 0.999, a = 1, b = 1)

par(mar = c(5, 4, 1, 1) + .1)
plotBounds(z1, yt = "p", area = FALSE, cols = rep("red", 4), lwds = c(5, 5), gy = 10)
plotBounds(z2, yt = "p", add = TRUE, cols = rep("purple", 4), lwds = c(5, 5))
plotBounds(z3, yt = "p", add = TRUE, cols = rep("black", 4), lwds = c(5, 5))
plotBounds(z4, yt = "p", add = TRUE, cols = rep("darkgray", 4), lwds = c(5, 5))
plotBounds(z5, yt = "p", add = TRUE, cols = rep("lightgray", 4), lwds = c(5, 5))
legend("topleft",
  lwd = 5, col = c("lightgray", "darkgray", "black", "purple", "red"),
  c(
    expression(theta[U] == 0.999), expression(theta[U] == 0.99),
    expression(theta[U] == 0.95),
    expression(theta[U] == 0.90), expression(theta[U] == 0.80)
  ), cex = 0.75
)
legend("bottomleft",
  lwd = 5, col = rev(c("lightgray", "darkgray", "black", "purple", "red")),
  c(
    expression(theta[L] == 0.20), expression(theta[L] == 0.10),
    expression(theta[L] == 0.05),
    expression(theta[L] == 0.01), expression(theta[L] == 0.001)
  ), cex = 0.75
)


## OC plots:
## plots of Pr(Efficacy) and Pr(Futility) and Pr(No Decision) vs p

nsim <- 1000
pvec <- seq(0, 1, by = 0.05)
## (note: slides were produced with nsim = 10000 and by=0.01)

np <- length(pvec)
peffbp <- rep(NA, np)
pfutbp <- peffbp
pgrabp <- peffbp
for (i in 1:np) {
  z <- ocPostprob(c(10, 25, 40),
    p = pvec[i], p0 = 0.50, p1 = 0.50,
    tL = 1 - 0.20, tU = 0.80, parE = c(1, 1), ns = nsim
  )$oc
  peffbp[i] <- z[dimnames(z)[[2]] == "PrEfficacy"]
  pfutbp[i] <- z[dimnames(z)[[2]] == "PrFutility"]
  pgrabp[i] <- z[dimnames(z)[[2]] == "PrGrayZone"]
}

## the following plot can be made more appealing
## by increasing number of points:
par(mar = c(5, 4, 1, 1) + .1)
plot(c(0, 1), c(0, 1), type = "n", xlab = "True Rate", ylab = "Pr(Decision)")
lines(pvec, peffbp, lwd = 5, col = "green")
lines(pvec, pfutbp, lwd = 5, col = "red")
lines(pvec, pgrabp, lwd = 5, col = "darkgray")
legend(.7, .6,
  lwd = 5, col = c("green", "red", "darkgray"), cex = .75,
  c("Activity Decision", "Futility Decision", "Gray Zone")
)

## alternatively, spline interpolation makes it also more appealing:
par(mar = c(5, 4, 1, 1) + .1)
plot(c(0, 1), c(0, 1), type = "n", xlab = "True Rate", ylab = "Pr(Decision)")
lines(spline(pvec, peffbp), lwd = 5, col = "green")
points(pvec, peffbp, pch = 19, col = "green")
lines(spline(pvec, pfutbp), lwd = 5, col = "red")
points(pvec, pfutbp, pch = 19, col = "red")
lines(spline(pvec, pgrabp), lwd = 5, col = "darkgray")
points(pvec, pgrabp, pch = 19, col = "darkgray")
legend(.7, .6,
  lwd = 5, col = c("green", "red", "darkgray"), cex = .75,
  c("Activity Decision", "Futility Decision", "Gray Zone")
)
