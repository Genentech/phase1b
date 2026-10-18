test_that("predprob gives correct result", {
  result <- predprob(
    x = 16,
    n = 23,
    Nmax = 40,
    p = 0.6,
    thetaT = 0.9,
    parE = c(0.6, 0.4)
  )
  expect_equal(result$result, 0.5655589, tolerance = 1e-6)
})

test_that("Higher thetaT decreases predprob", {
  is_lower <- predprob(
    x = 16,
    n = 23,
    Nmax = 40,
    p = 0.6,
    thetaT = 0.9,
    parE = c(0.6, 0.4)
  )
  is_higher <- predprob(
    x = 16,
    n = 23,
    Nmax = 40,
    p = 0.6,
    thetaT = 0.8,
    parE = c(0.6, 0.4)
  )
  expect_true(is_lower$result < is_higher$result)
})

test_that("predprob gives an error when x is a numeric", {
  expect_error(
    predprob(
      x = 16:20,
      n = 23,
      Nmax = 40,
      p = 0.6,
      thetaT = 0.9,
      parE = c(0.6, 0.4)
    ),
    "Must have length 1"
  )
})

test_that("predprob gives an error when K columns of weights exceed rows parE", {
  expect_error(
    predprob(
      x = 16,
      n = 23,
      Nmax = 40,
      p = 0.6,
      thetaT = 0.9,
      parE = c(0.6, 0.4),
      weights = c(3, 1, 4)
    ),
    "Assertion on 'weights' failed: Must have length 1",
    fixed = TRUE
  )
})

test_that("predprob gives an error when K columns of weights is less than rows parE", {
  expect_error(
    predprob(
      x = 16,
      n = 23,
      Nmax = 40,
      p = 0.6,
      thetaT = 0.9,
      parE = rbind(c(0.6, 0.4), c(0.5, 0.5)),
      weights = 1
    ),
    "Assertion on 'weights' failed: Must have length 2",
    fixed = TRUE
  )
})

test_that("predprob gives an error when thetaT exceeds 1", {
  expect_error(
    predprob(
      x = 16,
      n = 23,
      Nmax = 40,
      p = 0.6,
      thetaT = 2,
      parE = c(0.6, 0.4)
    ),
    "failed"
  )
})

test_that("predprob gives the correct numeric result with a beta-mixture", {
  result <- predprob(
    x = 20,
    n = 23,
    Nmax = 40,
    p = 0.6,
    thetaT = 0.9,
    parE = rbind(c(1, 1), c(25, 15)),
    weights = c(0.6, 0.4)
  )
  expect_equal(result$result, 0.9831967, tolerance = 1e-6)
})

test_that("predprob gives an error when x exceeds interim n", {
  expect_error(
    predprob(
      x = 24,
      n = 23,
      Nmax = 40,
      p = 0.6,
      thetaT = 0.9,
      parE = rbind(c(1, 1), c(25, 15)),
      weights = c(3, 1)
    ),
    "failed"
  )
})

test_that("predprob gives an error when Nmax is less than n", {
  expect_error(
    predprob(
      x = 16,
      n = 23,
      Nmax = 22,
      p = 0.6,
      thetaT = 0.9,
      parE = rbind(c(1, 1), c(25, 15)),
      weights = c(0.6, 0.4)
    ),
    "failed"
  )
})
