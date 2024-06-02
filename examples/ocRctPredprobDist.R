# When `relativeDelta = TRUE`:
set.seed(2000)
ocRctPredprobDist(
  nnE = c(10, 20, 30),
  pE = 0.3,
  pS = 0.3,
  deltaE = 0.2,
  deltaF = 0.1,
  phiU = 0.8,
  phiFu = 0.2,
  relativeDelta = TRUE,
  tT = 0.6,
  tF = 0.4,
  parE = c(a = 1, b = 1),
  parS = c(a = 1, b = 1),
  weights = 1,
  weightsS = 1,
  randRatio = 1,
  sim = 50,
  wiggle = FALSE,
  nnF = c(10, 20, 30),
  decision1 = TRUE
)

# When `relativeDelta = FALSE`:
set.seed(2000)
ocRctPredprobDist(
  nnE = c(10, 20, 30),
  pE = 0.3,
  pS = 0.3,
  deltaE = 0.2,
  deltaF = 0.1,
  phiU = 0.8,
  phiFu = 0.2,
  relativeDelta = FALSE,
  tT = 0.6,
  tF = 0.4,
  parE = c(a = 1, b = 1),
  parS = c(a = 1, b = 1),
  weights = 1,
  weightsS = 1,
  randRatio = 1,
  sim = 50,
  wiggle = FALSE,
  nnF = c(10, 20, 30),
  decision1 = TRUE
)

# Only one Futility look:
set.seed(2000)
ocRctPredprobDist(
  nnE = c(10, 20, 30),
  pE = 0.3,
  pS = 0.3,
  deltaE = 0.2,
  deltaF = 0.1,
  phiU = 0.8,
  phiFu = 0.2,
  relativeDelta = FALSE,
  tT = 0.6,
  tF = 0.4,
  parE = c(a = 1, b = 1),
  parS = c(a = 1, b = 1),
  weights = 1,
  weightsS = 1,
  randRatio = 1,
  sim = 50,
  wiggle = FALSE,
  nnF = 20,
  decision1 = TRUE
)

# When `deltaF = 0`:
set.seed(2000)
ocRctPredprobDist(
  nnE = c(10, 20, 30),
  pE = 0.3,
  pS = 0.3,
  deltaE = 0.2,
  deltaF = 0,
  phiU = 0.8,
  phiFu = 0.2,
  relativeDelta = FALSE,
  tT = 0.6,
  tF = 0.4,
  parE = c(a = 1, b = 1),
  parS = c(a = 1, b = 1),
  weights = 1,
  weightsS = 1,
  randRatio = 1,
  sim = 50,
  wiggle = FALSE,
  nnF = c(10, 20, 30),
  decision1 = TRUE
)
