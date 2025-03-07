# operating characteristics for posterior probability method with beta prior on SOC
# For three looks of 10, 20 and 30 we have the following assumptions :
# True response rate or truep of the treatment group = 40%
# The following are the Go and Stop rules respectively :
# Prior of treatment arm parE= Beta(1,1).
# Without random distance allowed for Futility and Efficacy Looks.
res1 <- ocPostprobDist(
  nnE = c(10, 20, 30),
  truep = 0.4,
  deltaE = 0.1,
  deltaF = -0.1,
  tL = 0.6,
  tU = 0.6,
  parE = c(1, 1),
  parS = c(5, 25),
  sim = 50,
  wiggle = FALSE
)

res1$oc

# Allow random distance for Efficacy and Futility looks.
res2 <- ocPostprobDist(
  nnE = c(10, 20, 30),
  truep = 0.4,
  deltaE = 0.1,
  deltaF = -0.1,
  tL = 0.6,
  tU = 0.6,
  parE = c(1, 1),
  parS = c(5, 25),
  sim = 50,
  wiggle = TRUE
)

res2$oc

# Allow Futility analysis only at the end.
res3 <- ocPostprobDist(
  nnE = c(10, 20, 30),
  truep = 0.4,
  deltaE = 0.1,
  deltaF = -0.1,
  tL = 0.6,
  tU = 0.6,
  parE = c(1, 1),
  parS = c(5, 25),
  sim = 50,
  wiggle = TRUE,
  nnF = 30
)

res3$oc
