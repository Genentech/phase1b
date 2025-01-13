# boundsPredprob ----
test_that("boundsPredprob gives correct result and when default weight is not assigned", {
  result_weights <- boundsPredprob(
    looks = c(10, 20, 30, 40),
    p0 = 0.2,
    tT = 0.80,
    phiL = 0.10,
    phiU = 0.90,
    parE = c(1, 1),
    weights = 1
  )
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
      looks = c(10, 20, 30, 40),
      xL = c(0, 2, 5, 9),
      pL = c(0, 0.1, 0.1667, 0.225),
      predL = c(0.0268, 0.0269, 0.0446, 0.0000),
      postL = c(0.0859, 0.1787, 0.3931, 0.704),
      UciL = c(0.2589, 0.2826, 0.319, 0.3598),
      xU = c(4, 7, 9, 10),
      pU = c(0.4, 0.35, 0.3, 0.25),
      predU = c(0.9287, 0.9600, 0.9604, 1.0000),
      postU = c(0.9496, 0.9569, 0.9254, 0.8177),
      LciU = c(0.15, 0.1773, 0.1663, 0.1424)
    )
  )
  expect_equal(result$xL, c(0, 2, 5, 9))
  expect_equal(result$pL, c(0, 0.1, 0.1667, 0.225))
  expect_equal(result$postL, c(0.0859, 0.1787, 0.3931, 0.704))
  expect_equal(result$pL_upper_ci, c(0.2589, 0.2826, 0.319, 0.3598))
  expect_equal(result$xU, c(4, 7, 9, 10))
  expect_equal(result$pU, c(0.4, 0.35, 0.3, 0.25))
  expect_equal(result$postU, c(0.9496, 0.9569, 0.9254, 0.8177))
  expect_equal(result$pU_lower_ci, c(0.15, 0.1773, 0.1663, 0.1424))
})

test_that("boundsPredprob with Beta Mixture Priors give correct results", {
  result <- boundsPredprob(
    looks = c(10, 20),
    p0 = 0.20,
    tT = 0.80,
    phiL = 0.10,
    phiU = 0.90,
    parE = cbind(c(1, 1), c(3, 10)),
    weights = c(0.2, 0.8)
  )
  expected_lower_bound_results <- data.frame(
    list(
      interim_predL = # predL of interim data
        predprob(
          x = result$xL[1],
          n = 10,
          p = 0.20,
          Nmax = 20,
          thetaT = 0.80,
          parE = cbind(c(1, 1), c(3, 10)),
          weights = c(0.2, 0.8)
        )$result,
      interim_post = # postL of interim data
        postprob(
          x = 2,
          n = 10,
          p = 0.2,
          parE = cbind(c(1, 1), c(3, 10)),
          weights = c(0.2, 0.8),
          log.p = FALSE
        ),
      final_predL = # predU of interim data
        predprob(
          x = result$xL[2],
          n = 20,
          p = 0.20,
          Nmax = 20,
          thetaT = 0.80,
          parE = cbind(c(1, 1), c(3, 10)),
          weights = c(0.2, 0.8)
        )$result,
      final_post = # postU of final data
        postprob(
          x = 6,
          n = 20,
          p = 0.2,
          parE = cbind(c(1, 1), c(3, 10)),
          weights = c(0.2, 0.8),
          log.p = FALSE
        )
    )
  )
  expected_upper_bound_results <- data.frame(
    list(
      interim_predU = # predL of interim data
        predprob(
          x = result$xU[1],
          n = 10,
          p = 0.20,
          Nmax = 20,
          thetaT = 0.80,
          parE = cbind(c(1, 1), c(3, 10)),
          weights = c(0.2, 0.8)
        )$result,
      interim_post = # postL of interim data
        postprob(
          x = result$xU[1],
          n = 10,
          p = 0.2,
          parE = cbind(c(1, 1), c(3, 10)),
          weights = c(0.2, 0.8),
          log.p = FALSE
        ),
      final_predU = # predU of interim data
        predprob(
          x = result$xU[2],
          n = 20,
          p = 0.20,
          Nmax = 20,
          thetaT = 0.80,
          parE = cbind(c(1, 1), c(3, 10)),
          weights = c(0.2, 0.8)
        )$result,
      final_post = # postU of final data
        postprob(
          x = result$xU[2],
          n = 20,
          p = 0.2,
          parE = cbind(c(1, 1), c(3, 10)),
          weights = c(0.2, 0.8),
          log.p = FALSE
        )
    )
  )
  # lower bound predictive and posterior probabilities
  expect_equal(result$xL[1], 2)
  expect_equal(result$predL[1], expected_lower_bound_results$interim_predL, tolerance = 1e-3)
  expect_equal(result$postL[1], expected_lower_bound_results$interim_post, tolerance = 1e-4)
  expect_equal(result$predL[2], expected_lower_bound_results$final_predL, tolerance = 1e-3)
  expect_equal(result$postL[2], expected_lower_bound_results$final_post, tolerance = 1e-4)
  # lower bound predictive and posterior probabilities
  expect_equal(result$xU[1], 6)
  expect_equal(result$predU[1], expected_upper_bound_results$interim_predU, tolerance = 1e-3)
  expect_equal(result$postU[1], expected_upper_bound_results$interim_post, tolerance = 1e-4)
  expect_equal(result$predU[2], expected_upper_bound_results$final_predU, tolerance = 1e-3)
  expect_equal(result$postU[2], expected_upper_bound_results$final_post, tolerance = 1e-4)
})
