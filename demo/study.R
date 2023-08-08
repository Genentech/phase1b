## (1) predictive probability functions
## ------------------------------------

# Lee and Liu example: Nmax=40, x=16, n=23, vague beta(0.6,0.4)
predProbEx <- predprob(x = 16, n = 23, Nmax = 40, p = 0.60, thetaT = 0.90, parE = c(a = 0.6, b = 0.4))
## Table 1, Panel B is reproduced (p. 97 in the paper)
predProbEx

# again the Lee and Liu example, but now with prior on the control response
# rate:
predProbDistEx <- predprobDist(
  x = 16, n = 23, Nmax = 40,
  parE = c(0.6, 0.4), parS = c(60, 40), thetaT = 0.90
)
as.numeric(predProbDistEx)
## this must be lower

## look at the operating characteristics:
set.seed(912)
z <- ocPredprobDist(
  nn = 1:40,
  p = 0.3,
  delta = 0.1,
  tT = 0.9,
  phiL = 0.01,
  phiU = 0.95,
  parE = c(1, 1),
  parS = c(1, 6),
  ns = 10
)
z$oc

## now with some data on the control arm:
predProbDistRctEx <- predprobDist(
  x = 16, n = 23, Nmax = 40,
  ## xS=1, nS=3, NmaxControl=10,
  xS = 10, nS = 20, NmaxControl = 50,
  parE = c(0.6, 0.4), parS = c(60, 40), thetaT = 0.90
)

## use as.numeric to suppress printing of the attribute
as.numeric(predProbDistRctEx)
## depending on how many responses (xS) we have already seen in the control,
## this number varies

## check that the probabilities of future response combinations
## sum up to 1:
sum(attr(predProbDistRctEx, "tables")$pyz)

## look again at operating characteristics
set.seed(9122)
zRct <- ocRctPredprobDist(
  nn = 1:40,
  pE = 0.3,
  pS = 0.15,
  delta = 0.1,
  tT = 0.9,
  phiL = 0.01,
  phiU = 0.95,
  parE = c(1, 1),
  parS = c(1, 6), ## this means 1/7 response in SOC,
  ## compatible with true rate here (no drift)
  randRatio = 1, ## 1:1 randomization
  ns = 10
)
zRct$oc
z$oc

## (2) posterior probability functions:
## ------------------------------------

## example from Lee and Liu
fix <- postprob(x = 16, n = 23, p = 0.60, parE = c(a = 0.6, b = 0.4))
fix

## show what the effect of a prior distribution on the threshold p is:
## instead of fixed 0.6 we can assume 100 patients out of which 60 had response
postprobDist(x = 16, n = 23, parE = c(0.6, 0.4), parS = c(60, 40))


## show how this probability converges for increasing control size:
ppex <- function(nS) {
  postprobDist(x = 16, n = 23, parE = c(0.6, 0.4), parS = c(0.6, 0.4) * nS)
}
nSgrid <- c(10, 20, 50, 100, 1000, 10000)
plot(nSgrid, sapply(nSgrid, ppex), type = "l", log = "x")
abline(h = fix, col = "red")

## look at the operating characteristics:
set.seed(912)
z1 <- ocPostprobDist(
  nn = 1:40,
  p = 0.3,
  deltaE = 0.1,
  deltaF = 0.1,
  tL = 0.9,
  tU = 0.9,
  parE = c(1, 1),
  parS = c(1, 6),
  ns = 10
)
z1

## look at characteristics if we randomize
set.seed(431)
z2 <- ocRctPostprobDist(
  nn = 1:40,
  pE = 0.3,
  pS = 0.15,
  deltaE = 0.1,
  deltaF = 0.1,
  tL = 0.9,
  tU = 0.9,
  parE = c(1, 1),
  parS = c(1, 6), ## this means 1/7 response in SOC,
  ## compatible with true rate here (no drift)
  randRatio = 1, ## 1:1 randomization
  ns = 10
)
z2

z1$oc
z2$oc
hist(z2$SampleSize)

## now check what happens if we use point mass for comparison
p0 <- 0.15 - 0.10
p1 <- 0.15 + 0.10
set.seed(141)
z3 <- ocPostprob(
  nn = 1:40,
  p = 0.3,
  p0 = p0,
  p1 = p1,
  tL = 0.9,
  tU = 0.9,
  parE = c(1, 1),
  ns = 100
)
z3$oc

## situation in our study
b1 <- boundsPredprob(
  nvec = c(10, 20, 30, 40), Nmax = 40, p = 0.20, tT = 0.90,
  phiL = 0.01, phiU = 0.90, a = 1, b = 1
)
b2 <- boundsPostprob(
  nvec = c(10, 20, 30, 40), p0 = 0.20, p1 = 0.35,
  tL = 0.01, tU = 0.90, a = 1, b = 1
)
plotBounds(b1, yt = "p", area = FALSE)
plotBounds(b2, yt = "p", add = TRUE)
b1
b2

# 3 scenarios for interim looks
y <- list(c(10, 20, 30, 40), c(20, 30, 40), c(20, 40))

# p=0.20: predictive probability
for (i in 1:3) {
  z <- ocPredprob(
    nn = y[[i]], p = 0.20, p0 = 0.20, tT = 0.90, phiL = 0.01, phiU = 0.90,
    ns = 10
  )
  print(z$params)
  print(z$oc)
  plotOc(z)
}

# p=0.20: posterior probability
for (i in 1:3) {
  z <- ocPostprob(
    nn = y[[i]], p = 0.20, p0 = 0.20, p1 = 0.35, tL = 0.01, tU = 0.90,
    ns = 10
  )
  print(z$params)
  print(z$oc)
  plotOc(z)
}

# p=0.35: predictive probability
for (i in 1:3) {
  z <- ocPredprob(
    nn = y[[i]], p = 0.35, p0 = 0.20, tT = 0.90, phiL = 0.01, phiU = 0.90,
    ns = 10
  )
  print(z$params)
  print(z$oc)
  plotOc(z)
}

# p=0.35: posterior probability
for (i in 1:3) {
  z <- ocPostprob(
    nn = y[[i]], p = 0.35, p0 = 0.20, p1 = 0.35, tL = 0.01, tU = 0.90,
    ns = 10
  )
  print(z$params)
  print(z$oc)
  plotOc(z)
}

## Compare with Simon Two-Stage Design
# http://www.cscc.unc.edu/cscc/aivanova/SimonsTwoStageDesign.aspx
# type 1 error 0.13, power=0.8, p0=0.20, p1=0.35
# --> n=40, n1=16, r1=3, r2=10, EN0=25.6, Pr(stop early)=0.5981
b1 <- boundsPredprob(
  nvec = c(16, 40), Nmax = 40, p = 0.20, tT = 0.90,
  phiL = 0.01, phiU = 0.90, a = 1, b = 1
)
b2 <- boundsPostprob(
  nvec = c(16, 40), p0 = 0.20, p1 = 0.35,
  tL = 0.01, tU = 0.90, a = 1, b = 1
)
plotBounds(b1, yt = "p", area = FALSE)
plotBounds(b2, yt = "p", add = TRUE)
points(40, 11 / 40, pch = 16)
lines(c(16, 40), c(3 / 16, 10 / 40), lwd = 3)
b1
b2

# change parameters to match simon two-stage
b1 <- boundsPredprob(
  nvec = c(16, 40), Nmax = 40, p = 0.20, tT = 0.85,
  phiL = 0.25, phiU = 0.999999, a = 1, b = 1
)
b2 <- boundsPostprob(
  nvec = c(16, 40), p0 = 0.20, p1 = 0.35, tT = 0.85,
  phiL = 0.15, phiU = 0.85, a = 1, b = 1
)
plotBounds(b1, yt = "p", area = FALSE)
plotBounds(b2, yt = "p", add = TRUE)
points(40, 11 / 40, pch = 16)
lines(c(16, 40), c(3 / 16, 10 / 40), lwd = 3, lty = 2)
b1
b2

# c(16,40) with original parameters
# p=0.20: predictive probability
z <- ocPredprob(nn = c(16, 40), p = 0.20, p0 = 0.20, tT = 0.90, tL = 0.01, tU = 0.90, ns = 10)
print(z$params)
print(z$oc)
plotOc(z)

# p=0.20: posterior probability
z <- ocPostprob(nn = c(16, 40), p = 0.20, p0 = 0.20, p1 = 0.35, tL = 0.01, tU = 0.90, ns = 10)
print(z$params)
print(z$oc)
plotOc(z)
2
# p=0.35: predictive probability
z <- ocPredprob(nn = c(16, 40), p = 0.35, p0 = 0.20, tT = 0.90, tL = 0.01, tU = 0.90, ns = 10)
print(z$params)
print(z$oc)
plotOc(z)

# p=0.35: posterior probability
z <- ocPostprob(nn = c(16, 40), p = 0.35, p0 = 0.20, p1 = 0.35, tL = 0.01, tU = 0.90, ns = 10)
print(z$params)
print(z$oc)
plotOc(z)

# c(16,40) with parameters adjusted to match simon two stage
# p=0.20: predictive probability
z <- ocPredprob(nn = c(16, 40), p = 0.20, p0 = 0.20, tT = 0.85, tL = 0.25, tU = 0.999999, ns = 10)
print(z$params)
print(z$oc)
plotOc(z)

# p=0.20: posterior probability
z <- ocPostprob(nn = c(16, 40), p = 0.20, p0 = 0.20, p1 = 0.35, tL = 0.15, tU = 0.85, ns = 10)
print(z$params)
print(z$oc)
plotOc(z)

# p=0.35: predictive probability
z <- ocPredprob(nn = c(16, 40), p = 0.35, p0 = 0.20, tT = 0.85, tL = 0.25, tU = 0.999999, ns = 10)
print(z$params)
print(z$oc)
plotOc(z)

# p=0.35: posterior probability
z <- ocPostprob(nn = c(16, 40), p = 0.35, p0 = 0.20, p1 = 0.35, tL = 0.15, tU = 0.85, ns = 10)
print(z$params)
print(z$oc)
plotOc(z)
