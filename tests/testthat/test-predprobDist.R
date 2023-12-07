# h_predprobdist_single_arm ----
test_that("predprobdist gives correct predictive probability", {
  result <- h_predprobdist_single_arm(
    x = 16,
    Nmax = 40,
    delta = 0.1,
    relativeDelta = FALSE,
    parE = c(0.6, 0.4),
    weights = 1,
    parS = c(7, 11),
    weightsS = 1,
    thetaT = 0.5,
    density = seq(0, 1, length.out = 18),
    mE = 17
  )
  expect_equal(result, expected, tolerance = 1e-4)
})

test_that("predictive probability is lower when control group has higher response rate at interim", {
  is_lower <- h_predprobdist_single_arm(
    x = 16,
    n = 23,
    xS = 20,
    nS = 20,
    delta = 0.1,
    relativeDelta = FALSE,
    parE = c(1, 1),
    weights = 1,
    parS = c(1, 1),
    weightsS = 1,
    thetaT = 0.9,
    density,
    mE
  )
  is_higher <- h_predprobdist_single_arm(
    x = 16,
    n = 23,
    delta = 0.1,
    relativeDelta = FALSE,
    parE = c(1, 1),
    weights = 1,
    parS = c(1, 1),
    weightsS = 1,
    thetaT = 0.9,
    density,
    mE
  )
  expect_true(is_higher$result > is_lower$result, )
})

test_that("predprobdist gives correct list", {
  result <- h_predprobdist_single_arm(
    x = 16,
    n = 23,
    delta = 0.1,
    relativeDelta = FALSE,
    parE = c(1, 1),
    weights = 1,
    parS = c(1, 1),
    weightsS = 1,
    thetaT = 0.9,
    density,
    mE
  )
  expected <- list()
  expect_identical(result, expected)
})
