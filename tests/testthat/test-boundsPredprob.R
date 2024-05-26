# boundsPostProb ----
test_that("boundsPredprob gives correct result and list", {
  result <- boundsPredprob(
    nvec = c(10, 20, 30, 40),
    p0 = 0.2,
    tT = 0.80,
    phiL = 0.10,
    phiU = 0.90,
    a = 1,
    b = 1
  )
  expected <- data.frame(
    list(
      nvec = c(10, 20, 30, 40),
      xL = c(0, 2, 5, 9),
      pL = c(0, 0.1, 0.1667, 0.225),
      postL = c(0.0859, 0.1787, 0.3931, 0.704),
      UciL = c(0.2589, 0.2826, 0.319, 0.3598),
      xU = c(4, 7, 9, 10),
      pU = c(0.4, 0.35, 0.3, 0.25),
      postU = c(0.9496, 0.9569, 0.9254, 0.8177),
      LciU = c(0.15, 0.1773, 0.1663, 0.1424)
    )
  )
  expect_equal(result$xL, c(0, 2, 5, 9))
  expect_equal(result$pL, c(0, 0.1, 0.1667, 0.225))
  expect_equal(result$postL, c(0.0859, 0.1787, 0.3931, 0.704))
  expect_equal(result$UciL, c(0.2589, 0.2826, 0.319, 0.3598))
  expect_equal(result$xU, c(4, 7, 9, 10))
  expect_equal(result$pU, c(0.4, 0.35, 0.3, 0.25))
  expect_equal(result$postU, c(0.9496, 0.9569, 0.9254, 0.8177))
  expect_equal(result$LciU, c(0.15, 0.1773, 0.1663, 0.1424))
})
