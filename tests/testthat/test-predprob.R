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
    x = 16, n = 23, Nmax = 40, p = 0.6, thetaT = 0.8,
    parE = c(0.6, 0.4)
  )
  expect_true(is_lower$result < is_higher$result)
})

test_that("predprob gives an error when x is a numeric", {
  expect_error(predprob(
    x = 16:20,
    n = 23,
    Nmax = 40,
    p = 0.6,
    thetaT = 0.9,
    parE = c(0.6, 0.4)
  ), "is not a multiple of replacement length")
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
      parE = rbind(c(0.6, 0.4), c(0.5, 0.5)), weights = 1
    ),
    "Assertion on 'weights' failed: Must have length 2",
    fixed = TRUE
  )
})

test_that("predprob gives an error when thetaT exceeds 1", {
  expect_error(predprob(
    x = 16,
    n = 23,
    Nmax = 40,
    p = 0.6,
    thetaT = 2,
    parE = c(0.6, 0.4)
  ), "failed")
})

test_that("predprob gives the correct numeric result with a beta-mixture", {
  result <- predprob(
    x = 20,
    n = 23,
    Nmax = 40,
    p = 0.6,
    thetaT = 0.9,
    parE = rbind(c(1, 1), c(25, 15)),
    weights = c(3, 1)
  )
  expect_equal(result$result, 0.9874431, tolerance = 1e-6)
})

test_that("predprob gives an error when x exceeds interim n", {
  expect_error(predprob(
    x = 24,
    n = 23,
    Nmax = 40,
    p = 0.6,
    thetaT = 0.9,
    parE = rbind(c(1, 1), c(25, 15)),
    weights = c(3, 1)
  ), "failed")
})

test_that("predprob gives an error when Nmax is less than n", {
  expect_error(predprob(
    x = 16,
    n = 23,
    Nmax = 22,
    p = 0.6,
    thetaT = 0.9,
    parE = rbind(c(1, 1), c(25, 15)),
    weights = c(3, 1)
  ), "failed")
})

test_that("predprob gives correct list", {
  result <- predprob(
    x = 20,
    n = 23,
    Nmax = 40,
    p = 0.6,
    thetaT = 0.9,
    parE = rbind(c(1, 1), c(25, 15)),
    weights = c(3, 1)
  )
  expected <- list(
    result = 0.987443066689065,
    table = data.frame(
      counts = 0:17,
      cumul_counts = c(
        23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
        33, 34, 35, 36, 37, 38, 39, 40
      ),
      density = c(
        0, 0, 0, 0,
        1e-04, 3e-04, 0.0011, 0.0031, 0.008, 0.0177, 0.0348, 0.0613,
        0.0973, 0.1394, 0.1783, 0.1964, 0.1708, 0.0913
      ),
      posterior = c(
        0.156,
        0.2352, 0.3303, 0.4359, 0.5449, 0.65, 0.7447, 0.8244, 0.8871,
        0.9327, 0.9634, 0.9821, 0.9923, 0.9972, 0.9991, 0.9998, 1,
        1
      ),
      success = c(
        FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
        TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE
      )
    )
  )
  expect_identical(result, expected, tolerance = 1e-4)
})
