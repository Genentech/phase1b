# postprobDist ----

test_that("postprobDist gives the correct number result", {
  result <- postprobDist(x = 16, n = 23, parE = c(0.6, 0.4), parS = c(0.6, 0.4), delta = 0.1)
  expect_equal(result, 0.4431067, tolerance = 1e-5)
})

test_that("postprobDist gives incrementally higher values with larger x", {
  is_lower <- postprobDist(x = 16, n = 23, delta = 0.1, parE = c(0.6, 0.4), parS = c(0.6, 0.4))
  is_higher <- postprobDist(x = 20, n = 23, delta = 0.1, parE = c(0.6, 0.4), parS = c(0.6, 0.4))
  expect_true(is_lower < is_higher)
})

test_that("postprobDist gives incrementally higher values with increase x support", {
  expected_lower <- postprobDist(x = 16, n = 23, delta = 0.1, parE = c(0.6, 0.4), parS = c(0.6, 0.4))
  expected_higher <- postprobDist(x = 20, n = 23, delta = 0.1, parE = c(0.6, 0.4), parS = c(0.6, 0.4))
  result <- postprobDist(x = c(16, 20), n = 23, delta = 0.1, parE = c(0.6, 0.4), parS = c(0.6, 0.4))
  expect_identical(result, c(expected_lower, expected_higher))
})

test_that("postprobDist gives the correct result for a beta-mixture example", {
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

test_that("postprobDist gives incrementally higher values with increased x", {
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

test_that("postprobDist gives the correct result with a weighted beta-mixture", {
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

test_that("postprobDist gives the correct number result", {
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
    weightsS = c(0.3, 0.7),
  )
  expect_equal(result, 0.3248885, tolerance = 1e-4)
})

test_that("postprobDist gives an error when length(par)", {
  expect_error(
    results <- postprobDist(
      x = c(16, 17),
      n = c(23, 20),
      parE = c(0.6, 0.4),
      parS = c(0.6, 0.4),
      delta = 0.1,
      relativeDelta = FALSE
    ), "must have length 1, but has length 2"
  )
})

test_that("postprobDist gives an error", {
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

test_that("postprobDist gives an error", {
  expect_error(
    results <- postprobDist(
      x = 16,
      n = 23,
      xS = c(10, 12),
      nS = c(20),
      parE = c(0.6, 0.4),
      parS = c(0.6, 0.4),
      delta = 0.1,
      relativeDelta = FALSE
    ), "Must have length 1."
  )
})

test_that("postprobDist gives an error", {
  expect_error(
    results <- postprobDist(
      x = 16,
      n = 23,
      xS = c(10, 12),
      nS = c(20, 21),
      parE = c(0.6, 0.4),
      parS = c(0.6, 0.4),
      delta = 0.1,
      relativeDelta = FALSE
    ), "Must have length 1."
  )
})

# h_integrand_relDelta--
test_that("h_integrand_relDelta gives the correct numerical result", {
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
  activeBetamixPost <- getBetamixPost(x = x, n = n, par = parE, weights = weights)
  controlBetamixPost <- getBetamixPost(x = xS, n = nS, par = parS, weights = weightsS)
  results <- h_integrand_relDelta(
    p_s = p_s, delta = delta,
    activeBetamixPost = activeBetamixPost,
    controlBetamixPost = controlBetamixPost
  )
  expect_equal(results, 0.0001352829, tolerance = 1e-4)
})

test_that("h_integrand_relDelta gives the correct numerical result", {
  x <- 16
  n <- 23
  xS <- 10
  nS <- 20
  parE <- t(c(1, 3))
  parS <- t(c(1, 1))
  weights <- c(0.5)
  weightsS <- c(1)
  p_s <- 0.1
  delta <- 0.1
  relativeDelta <- TRUE
  activeBetamixPost <- getBetamixPost(x = x, n = n, par = parE, weights = weights)
  controlBetamixPost <- getBetamixPost(x = xS, n = nS, par = parS, weights = weightsS)
  results <- h_integrand_relDelta(
    p_s = p_s, delta = delta,
    activeBetamixPost = activeBetamixPost,
    controlBetamixPost = controlBetamixPost
  )
  expect_equal(results, 0.0001352829, tolerance = 1e-4)
})

# h_integrand --
test_that("h_integrand gives the correct numerical result", {
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
  activeBetamixPost <- getBetamixPost(x = x, n = n, par = parE, weights = weights)
  controlBetamixPost <- getBetamixPost(x = xS, n = nS, par = parS, weights = weightsS)
  results <- h_integrand(
    p_s = p_s, delta = delta,
    activeBetamixPost = activeBetamixPost,
    controlBetamixPost = controlBetamixPost
  )
  expect_equal(results, 0.0001352828, tolerance = 1e-4)
})


test_that("h_integrand gives the correct numerical result", {
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
  relativeDelta <- FALSE
  activeBetamixPost <- getBetamixPost(x = x, n = n, par = parE, weights = weights)
  controlBetamixPost <- getBetamixPost(x = xS, n = nS, par = parS, weights = weightsS)
  results <- h_integrand(
    p_s = p_s, delta = delta,
    activeBetamixPost = activeBetamixPost,
    controlBetamixPost = controlBetamixPost
  )
  expect_equal(results, 0.0001352828, tolerance = 1e-4)
})
