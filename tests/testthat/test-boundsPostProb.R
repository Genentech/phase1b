# boundsPostProb ----
test_that("boundsPostProb gives correct result and list", {
  result <- boundsPostprob(
    nvec = c(10, 20, 30, 40),
    p0 = 0.2,
    p1 = 0.2,
    tL = 0.60,
    tU = 0.60,
    a = 1,
    b = 1
  )
  expected <- data.frame(
    nvec = c(10, 20, 30, 40),
    xL = c(1, 3, 5, 6),
    pL = c(0.1, 0.15, 0.1667, 0.15),
    postL = c(0.6779, 0.6296, 0.6069, 0.739),
    pL_upper_ci = c(0.3942, 0.3437, 0.319, 0.2747),
    xU = c(2, 5, 7, 9),
    pU = c(0.2, 0.25, 0.2333, 0.225),
    postU = c(0.6174, 0.7693, 0.73, 0.704),
    pU_lower_ci = c(0.0368, 0.1041, 0.115, 0.1227)
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
