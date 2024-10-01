# boundsPostProb ----
test_that("boundsPredprob gives correct result", {
  result <- boundsPredprob(
    looks = c(10, 20, 30, 40),
    p0 = 0.2,
    tT = 0.80,
    phiL = 0.10,
    phiU = 0.90,
    parE = c(1, 1)
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

test_that("boundsPredprob of beta mixture gives correct result", {
  result <- boundsPredprob(
    looks = c(7, 15, 20),
    p0 = 0.2,
    tT = 0.80,
    phiL = 0.10,
    phiU = 0.90,
    parE = rbind(c(1, 1), c(3, 10)),
    weights = c(0.2, 0.8)
  )
  expected <- data.frame(
    list(
      looks = c(7, 15, 20),
      xL = c(1, 3, 6),
      pL = c(0.1429, 0.2000, 0.3000),
      predL = c(0.0446, 0.0121, 0.0000),
      postL = c(0.2407, 0.3818, 0.7734),
      UciL = c(0.5207, 0.4398, 0.5078),
      xU = c(5, 7, 7),
      pU = c(0.7143, 0.4667, 0.3500),
      predU = c(0.9843, 1.0000, 1.0000),
      postU = c(0.9883, 0.9727, 0.8919),
      LciU = c(0.3413, 0.2437, 0.1773)
    )
  )
  expect_equal(result$xL, c(1, 3, 6))
  expect_equal(result$pL, c(0.1429, 0.2000, 0.3000))
  expect_equal(result$predL, c(0.0446, 0.0121, 0.0000))
  expect_equal(result$postL, c(0.2407, 0.3818, 0.7734))
  expect_equal(result$UciL, c(0.5207, 0.4398, 0.5078))
  expect_equal(result$xU, c(5, 7, 7))
  expect_equal(result$pU, c(0.7143, 0.4667, 0.3500))
  expect_equal(result$predU, c(0.9843, 1.0000, 1.0000))
  expect_equal(result$postU, c(0.9883, 0.9727, 0.8919))
  expect_equal(result$LciU, c(0.3413, 0.2437, 0.1773))
})
