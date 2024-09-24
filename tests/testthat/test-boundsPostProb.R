# boundsPostProb ----
test_that("boundsPostProb gives correct result and list", {
  result <- boundsPostprob(
    looks = c(10, 20, 30, 40),
    p0 = 0.15,
    p1 = 0.2,
    tL = 0.70,
    tU = 0.60,
    parE = c(1, 1)
  )
  expected <- data.frame(
    looks = c(10, 20, 30, 40),
    xL = c(0, 1, 3, 4),
    pL = c(0, 0.05, 0.1, 0.1),
    postL = c(0.8327, 0.8450, 0.7039, 0.7567),
    pL_upper_ci = c(0.2589, 0.2161, 0.2386, 0.2144),
    xU = c(2, 5, 7, 9),
    pU = c(0.2000, 0.2500, 0.2333, 0.2250),
    postU = c(0.6174, 0.7693, 0.7300, 0.7040),
    pU_lower_ci = c(0.0368, 0.1041, 0.1150, 0.1227)
  )
  expect_equal(result$xL, expected$xL)
  expect_equal(result$pL, expected$pL)
  expect_equal(result$postL, expected$postL)
  expect_equal(result$pL_upper_ci, expected$pL_upper_ci)
  expect_equal(result$xU, expected$xU)
  expect_equal(result$pU, expected$pU)
  expect_equal(result$postU, expected$postU)
  expect_equal(result$pU_lower_ci, expected$pU_lower_ci)
})

test_that("boundsPostProb for beta mixture gives correct result and list", {
  result <- boundsPostprob(
    looks = c(7, 20, 40),
    p0 = 0.15,
    p1 = 0.20,
    tL = 0.60,
    tU = 0.80,
    parE = rbind(c(1, 19), c(2, 10)),
    weights = c(0.2, 0.8)
  )
  expected <- data.frame(
    looks = c(7, 20, 40),
    xL = c(0, 2, 5),
    pL = c(0.000, 0.100, 0.125),
    postL = c(0.7456, 0.6601, 0.6173),
    pL_upper_ci = c(0.3482, 0.2826, 0.2450),
    xU = c(2, 6, 10),
    pU = c(0.2857, 0.3000, 0.2500),
    postU = c(0.8890, 0.9054, 0.8012),
    pU_lower_ci = c(.0534, 0.1396, 0.1424)
  )
  expect_equal(result$xL, expected$xL)
  expect_equal(result$pL, expected$pL)
  expect_equal(result$postL, expected$postL)
  expect_equal(result$pL_upper_ci, expected$pL_upper_ci)
  expect_equal(result$xU, expected$xU)
  expect_equal(result$pU, expected$pU)
  expect_equal(result$postU, expected$postU)
  expect_equal(result$pU_lower_ci, expected$pU_lower_ci)
})
