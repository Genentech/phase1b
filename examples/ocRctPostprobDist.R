# When `relativeDelta = TRUE`
set.seed(2000)
ocRctPostprobDist(
  nnE = c(10, 20, 30),
  pE = 0.4,
  pS = 0.1,
  deltaE = 0.1,
  deltaF = 0.1,
  relativeDelta = TRUE,
  tL = 0.4,
  tU = 0.6,
  parE = c(a = 1, b = 1),
  parS = c(a = 1, b = 1),
  randRatio = 1,
  sim = 50,
  wiggle = FALSE,
  nnF = c(10, 20, 30)
)

# When `relativeDelta = FALSE`
set.seed(2000)
ocRctPostprobDist(
  nnE = c(10, 20, 30),
  pE = 0.4,
  pS = 0.1,
  deltaE = 0.1,
  deltaF = 0.1,
  relativeDelta = TRUE,
  tL = 0.4,
  tU = 0.6,
  parE = c(a = 1, b = 1),
  parS = c(a = 1, b = 1),
  randRatio = 1,
  sim = 50,
  wiggle = FALSE,
  nnF = c(10, 20, 30)
)

# When `wigge = TRUE` is negative
set.seed(2000)
ocRctPostprobDist(
  nnE = c(10, 20, 30),
  pE = 0.4,
  pS = 0.1,
  deltaE = 0.1,
  deltaF = 0.1,
  relativeDelta = TRUE,
  tL = 0.4,
  tU = 0.6,
  parE = c(a = 1, b = 1),
  parS = c(a = 1, b = 1),
  randRatio = 1,
  sim = 50,
  wiggle = TRUE,
  nnF = c(10, 20, 30)
)
