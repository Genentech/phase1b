x <- 16
n <- 23
xS <- 5
nS <- 10
Nmax <- 40
NmaxControl <- 20
delta <- 0
thetaT <- 0.6
parE <- c(1, 1)
parS <- c(1, 1)
weights <- c(2, 1)
weightsS <- c(2, 1)
relativeDelta <- FALSE
mE <- Nmax - n
parE <- t(parE)
weights <- rep(1, nrow(parE))
parS <- t(parS)
weightsS <- rep(1, nrow(parS))

h_get_predproblist_control(x = x,
                           n = Nmax,
                           delta = delta,
                           relativeDelta = relativeDelta,
                           parE = parE,
                           weights = weights,
                           parS = parS,
                           weightsS = weightsS,
                           thetaT = thetaT,
                           density = 0.77,
                           mE = 17)
