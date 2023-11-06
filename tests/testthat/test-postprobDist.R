# postprobBeta ----
test_that("postprobDist gives the correct number result", {
  result <- postprobDist(x = 16, n = 23, parE = c(0.6, 0.4), parS = c(0.6, 0.4))
  expect_equal(result, 0.5123873, tolerance = 1e-5)
})

test_that("postprobDist gives incrementally higher values with increase x support", {
  is_lower <- postprobDist(x = 16, n = 23, parE = c(0.6, 0.4), parS = c(0.6, 0.4))
  is_higher <- postprobDist(x = 20, n = 23, parE = c(0.6, 0.4), parS = c(0.6, 0.4))
  expect_true(is_lower < is_higher)
})

test_that("postprob gives the correct number result", {
  # 2 component beta mixture prior with weight = weightS = c(1,1)
  result <- postprobDist(
    x = 10,
    n = 23,
    parE = rbind(
      c(0.6, 0.4),
      c(1, 1)
    ),
    parS = rbind(
      c(0.6, 0.4),
      c(1, 1)
    )
  )
  expect_equal(result, 0.3948115, tolerance = 1e-5)
})

test_that("postprob gives incrementally higher values with increased x", {
  is_lower <- postprobDist(
    x = 10,
    n = 23,
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

test_that("postprobDist gives the correct number result", {
  # 2 component beta mixture prior with weights = weightsS = various
  result <- postprobDist(
    x = 10,
    n = 23,
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
  expect_equal(result, 0.3856478, tolerance = 1e-4)
})

# Extreme beta numbers are lowly weighted
test_that("postprobDist gives the correct number result", {
  # 2 component beta mixture prior with weights = weightsS = various
  result <- postprobDist(
    x = 10,
    n = 23,
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
  expect_equal(result, 0.3856478, tolerance = 1e-4)
})
