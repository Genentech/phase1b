# postprobDist ----

test_that("postprobDist gives the correct number result.", {
  result <- postprobDist(x = 16, n = 23, parE = c(0.6, 0.4), parS = c(0.6, 0.4), delta = 0.1)
  expect_equal(result, 0.4431067, tolerance = 1e-5)
})

test_that("postprobDist gives incrementally higher values with larger x", {
  is_lower <- postprobDist(x = 16, n = 23, delta = 0.1, parE = c(0.6, 0.4), parS = c(0.6, 0.4))
  is_higher <- postprobDist(x = 20, n = 23, delta = 0.1, parE = c(0.6, 0.4), parS = c(0.6, 0.4))
  expect_true(is_lower < is_higher)
})

test_that("postprobDist gives higher values with larger x and returns identical result when x is a vector", {
  expected_lower <- postprobDist(x = 16, n = 23, delta = 0.1, parE = c(0.6, 0.4), parS = c(0.6, 0.4))
  expected_higher <- postprobDist(x = 20, n = 23, delta = 0.1, parE = c(0.6, 0.4), parS = c(0.6, 0.4))
  result <- postprobDist(x = c(16, 20), n = 23, delta = 0.1, parE = c(0.6, 0.4), parS = c(0.6, 0.4))
  expect_identical(result, c(expected_lower, expected_higher))
})

test_that("postprobDist gives the correct result for a beta-mixture", {
  result <- postprobDist(
    x = 10,
    n = 23,
    delta = 0.1,
    parE = rbind(
      c(0.6, 0.4),
      c(1, 1)
    ),
    parS = rbind(
      c(0.6, 0.4),
      c(1, 1)
    )
  )
  expect_equal(result, 0.3143941, tolerance = 1e-5)
})

test_that("postprobDist gives incrementally higher values with larger x for a beta-mixture", {
  is_lower <- postprobDist(
    x = 10,
    n = 23,
    delta = 0.1,
    parE = rbind(
      c(0.6, 0.4),
      c(1, 1)
    ),
    parS = rbind(
      c(0.6, 0.4),
      c(1, 1)
    )
  )
  is_higher <- postprobDist(
    x = 16,
    n = 23,
    delta = 0.1,
    parE = rbind(
      c(0.6, 0.4),
      c(1, 1)
    ),
    parS = rbind(
      c(0.6, 0.4),
      c(1, 1)
    )
  )
  expect_true(is_lower < is_higher)
})

test_that("postprobDist gives the correct result for a weighted beta-mixture", {
  result <- postprobDist(
    x = 10,
    n = 23,
    delta = 0.1,
    parE = rbind(
      c(0.6, 0.4),
      c(1, 1)
    ),
    parS = rbind(
      c(0.6, 0.4),
      c(1, 1)
    ),
    weights = c(0.5, 0.5),
    weightsS = c(0.3, 0.7)
  )
  expect_equal(result, 0.3248885, tolerance = 1e-4)
})

test_that("postprobDist works with vector x", {
  result <- postprobDist(
    x = c(0, 10, 22, 23),
    n = 23,
    delta = 0.1,
    parE = rbind(
      c(0.6, 0.4),
      c(1, 1)
    ),
    parS = rbind(
      c(0.6, 0.4),
      c(1, 1)
    ),
    weights = c(0.5, 0.5),
    weightsS = c(0.3, 0.7)
  )
  expected <- c(0.0022653966293937, 0.324888481243124, 0.771937234865335, 0.817017633697455)
  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("postprobDist gives an error when n is not a number.", {
  expect_error(
    results <- postprobDist(
      x = c(16, 17),
      n = c(23, 20),
      parE = c(0.6, 0.4),
      parS = c(0.6, 0.4),
      delta = 0.1,
      relativeDelta = FALSE
    ),
    "Assertion on 'n' failed: Must have length 1",
    fixed = TRUE
  )
})

test_that("postprobDist gives an error when xS and nS are not numbers", {
  expect_error(
    results <- postprobDist(
      x = c(10, 16),
      n = 23,
      xS = c(9, 10),
      nS = c(20, 22),
      delta = 0.1,
      parE = c(0.6, 0.4),
      parS = c(0.6, 0.4),
      weights = c(0.5),
      weightsS = c(0.3),
    ), "Must have length 1"
  )
})

# h_integrand_relDelta ----
test_that("h_integrand_relDelta gives the correct numerical result for a beta-mixture.", {
  x <- 16
  n <- 23
  xS <- 10
  nS <- 20
  parE <- t(c(1, 3))
  parS <- t(c(1, 1))
  weights <- 1
  weightsS <- 1
  p_s <- 0.1
  delta <- 0.1
  relativeDelta <- TRUE
  activeBetamixPost <- h_getBetamixPost(x = x, n = n, par = parE, weights = weights)
  controlBetamixPost <- h_getBetamixPost(x = xS, n = nS, par = parS, weights = weightsS)
  results <- h_integrand_relDelta(
    p_s = p_s,
    delta = delta,
    activeBetamixPost = activeBetamixPost,
    controlBetamixPost = controlBetamixPost
  )
  expect_equal(results, 0.0001352829, tolerance = 1e-4)
})

test_that("h_integrand_relDelta gives the correct numerical result for a weighted beta-mixture.", {
  x <- 16
  n <- 23
  xS <- 10
  nS <- 20
  parE <- rbind(c(1, 3), c(2, 3))
  parS <- rbind(c(1, 1), c(3, 4))
  weights <- c(5, 10)
  weightsS <- c(3, 4)
  p_s <- 0.1
  delta <- 0.1
  relativeDelta <- TRUE
  activeBetamixPost <- h_getBetamixPost(x = x, n = n, par = parE, weights = weights)
  controlBetamixPost <- h_getBetamixPost(x = xS, n = nS, par = parS, weights = weightsS)
  results <- h_integrand_relDelta(
    p_s = p_s,
    delta = delta,
    activeBetamixPost = activeBetamixPost,
    controlBetamixPost = controlBetamixPost
  )
  expect_equal(results, 6.498862e-05, tolerance = 1e-4)
})

# h_integrand ----
test_that("h_integrand gives the correct numerical result for a beta-mixture", {
  x <- 16
  n <- 23
  xS <- 10
  nS <- 20
  parE <- t(c(1, 3))
  parS <- t(c(1, 1))
  weights <- 1
  weightsS <- 1
  p_s <- 0.1
  delta <- 0.1
  relativeDelta <- TRUE
  activeBetamixPost <- h_getBetamixPost(x = x, n = n, par = parE, weights = weights)
  controlBetamixPost <- h_getBetamixPost(x = xS, n = nS, par = parS, weights = weightsS)
  results <- h_integrand(
    p_s = p_s,
    delta = delta,
    activeBetamixPost = activeBetamixPost,
    controlBetamixPost = controlBetamixPost
  )
  expect_equal(results, 0.0001352828, tolerance = 1e-4)
})

test_that("h_integrand works as expected for a weighted beta-mixture.", {
  x <- 16
  n <- 23
  xS <- 10
  nS <- 20
  parE <- rbind(c(1, 3), c(2, 3))
  parS <- rbind(c(1, 1), c(3, 4))
  weights <- c(5, 10)
  weightsS <- c(3, 4)
  p_s <- 0.1
  delta <- 0.1
  relativeDelta <- FALSE
  activeBetamixPost <- h_getBetamixPost(x = x, n = n, par = parE, weights = weights)
  controlBetamixPost <- h_getBetamixPost(x = xS, n = nS, par = parS, weights = weightsS)
  results <- h_integrand(
    p_s = p_s,
    delta = delta,
    activeBetamixPost = activeBetamixPost,
    controlBetamixPost = controlBetamixPost
  )
  expect_equal(results, 6.498861e-05, tolerance = 1e-4)
})
