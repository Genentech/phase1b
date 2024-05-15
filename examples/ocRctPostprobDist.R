# When `relativeDelta = TRUE`:
set.seed(2000)
ocRctPostprobDist(
  nnE = c(10, 20, 30),
  pE = 0.4,
  pS = 0.3,
  deltaE = 0.15,
  deltaF = 0.05,
  relativeDelta = TRUE,
  tL = 0.2,
  tU = 0.8,
  parE = c(a = 1, b = 1),
  parS = c(a = 1, b = 1),
  randRatio = 1,
  sim = 50,
  wiggle = FALSE,
  nnF = c(10, 20, 30)
)

# When `relativeDelta = FALSE`:
set.seed(2000)
ocRctPostprobDist(
  nnE = c(10, 20, 30),
  pE = 0.4,
  pS = 0.3,
  deltaE = 0.15,
  deltaF = 0.05,
  relativeDelta = FALSE,
  tL = 0.2,
  tU = 0.8,
  parE = c(a = 1, b = 1),
  parS = c(a = 1, b = 1),
  randRatio = 1,
  sim = 50,
  wiggle = FALSE,
  nnF = c(10, 20, 30)
)

# Only one Futility look:
set.seed(2000)
ocRctPostprobDist(
  nnE = c(10, 20, 30),
  pE = 0.4,
  pS = 0.3,
  deltaE = 0.15,
  deltaF = 0.05,
  relativeDelta = FALSE,
  tL = 0.2,
  tU = 0.8,
  parE = c(a = 1, b = 1),
  parS = c(a = 1, b = 1),
  randRatio = 1,
  sim = 50,
  wiggle = FALSE,
  nnF = 30
)

# When `deltaF = 0`:
set.seed(2000)
ocRctPostprobDist(
  nnE = c(10, 20, 30),
  pE = 0.4,
  pS = 0.3,
  deltaE = 0.15,
  deltaF = 0,
  relativeDelta = FALSE,
  tL = 0.2,
  tU = 0.8,
  parE = c(a = 1, b = 1),
  parS = c(a = 1, b = 1),
  randRatio = 1,
  sim = 50,
  wiggle = FALSE,
  nnF = c(10, 20, 30)
)
