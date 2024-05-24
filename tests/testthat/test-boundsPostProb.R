# boundsPostProb ----
test_that("h_decision_one_RctPredProbDist gives correct result and list when relativeDelta = TRUE", {
  result <- boundsPostprob(
    nvec = c(10, 20, 30, 40),
    p0 = 0.2,
    p1 = 0.2,
    tL = 0.10,
    tU = 0.90,
    a = 1,
    b = 1
  )
  expected <- data.frame(
    list(
      nvec = c(10, 20, 30, 40),
      xL = c(0, 1, 2, 4),
      pL = c(0, 0.05, 0.0667, 0.1),
      postL = c(0.0859, 0.0576, 0.0374, 0.0664),
      UciL = c(0.2589, 0.2161, 0.1953, 0.2144),
      xU = c(4, 7, 9, 12),
      pU = c(0.4, 0.35, 0.3, 0.3),
      postU = c(0.9496, 0.9569, 0.9254, 0.9479),
      LciU = c(0.15, 0.1773, 0.1663, 0.1831)
    )
  )
  expect_equal(result$xL, c(0, 1, 2, 4))
  expect_equal(result$pL, c(0.0000, 0.0500, 0.0667, 0.1000))
  expect_equal(result$UciL, c(0.2589, 0.2161, 0.1953, 0.2144))
})
