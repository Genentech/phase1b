# h_get_decisionDist ----
test_that("h_get_decisionDist gives correct result when relativeDelta = TRUE", {
  set.seed(1989)
  example_dist <- h_get_distance(nn = c(10, 20, 30))
  looks <- h_get_looks(dist = example_dist, nnE = c(10, 20, 30), nnF = c(10, 20, 30))
  looks_nnrE <- looks$nnrE
  looks_nnrF <- looks$nnrF

  # input values from above arguments
  result <- h_get_decisionDist(
    nnr = c(10, 20, 30),
    nnrE = looks_nnrE,
    nnrF = looks_nnrF,
    truep = 0.4,
    parE = c(1, 1),
    tL = 0.8,
    tU = 0.6,
    deltaF = 0.1,
    deltaE = 0.2,
    relativeDelta = TRUE
  )
  expect_list(result, any.missing = FALSE)
  expect_equal(result$decision, NA)
  expect_equal(result$all_sizes, 30)
})

test_that("h_get_decisionDist gives correct result relativeDelta = FALSE", {
  set.seed(1989)
  example_dist <- h_get_distance(nn = c(10, 20, 30))
  looks <- h_get_looks(dist = example_dist, nnE = c(10, 20, 30), nnF = c(10, 20, 30))
  looks_nnrE <- looks$nnrE
  looks_nnrF <- looks$nnrF

  # input values from above arguments
  result <- h_get_decisionDist(
    nnr = c(10, 20, 30),
    nnrE = looks_nnrE,
    nnrF = looks_nnrF,
    truep = 0.4,
    parE = c(1, 1),
    tL = 0.8,
    tU = 0.6,
    deltaF = 0.1,
    deltaE = 0.2,
    relativeDelta = FALSE
  )
  expect_list(result, any.missing = FALSE)
  expect_equal(result$all_sizes, 30)
  expect_equal(result$decision, NA)
})


test_that("h_get_decisionDist gives correct result when nnE <U+2260> nnF", {
  set.seed(1989)
  example_dist <- h_get_distance(nn = c(10, 20, 30))
  looks <- h_get_looks(dist = example_dist, nnE = c(10, 20, 30), nnF = c(10, 20))
  looks_nnrE <- looks$nnrE
  looks_nnrF <- looks$nnrF

  # input values from above arguments
  result <- h_get_decisionDist(
    nnr = c(10, 20, 30),
    nnrE = looks_nnrE,
    nnrF = looks_nnrF,
    truep = 0.4,
    parE = c(1, 1),
    tL = 0.8,
    tU = 0.6,
    deltaF = 0.1,
    deltaE = 0.2,
    relativeDelta = FALSE
  )
  expect_list(result, any.missing = FALSE)
  expect_equal(result$decision, NA)
  expect_equal(result$all_sizes, 30)
})

# ocPostprobDist ----
test_that("the sum of Eff, Fut, Gray zone probabiliy is 1", {
  set.seed(1989)
  expect_warning(res1 <- ocPostprobDist(
    nnE = c(10, 20, 30),
    truep = 0.4,
    deltaE = 0.1,
    deltaF = -0.1,
    tL = 0.6,
    tU = 0.6,
    parE = c(1, 1),
    parS = c(5, 25),
    sim = 1000,
    wiggle = FALSE
  ), "Advise to use sim >= 50000 to achieve convergence")
  results <- sum(res1$oc[5:7])
  expect_equal(results, 1)
})

test_that("the PrFutility increases with increase futility looks", {
  set.seed(1989)
  expect_warning(res_one_fut <- ocPostprobDist(
    nnE = c(10, 20, 30),
    truep = 0.4,
    deltaE = 0.1,
    deltaF = -0.1,
    tL = 0.6,
    tU = 0.6,
    parE = c(1, 1),
    parS = c(5, 25),
    sim = 100,
    wiggle = TRUE,
    nnF = 30
  ), "Advise to use sim >= 50000 to achieve convergence")
  res_one_fut$oc$PrFutility
  expect_warning(res_three_fut <- ocPostprobDist(
    nnE = c(10, 20, 30),
    truep = 0.4,
    deltaE = 0.1,
    deltaF = -0.1,
    tL = 0.6,
    tU = 0.6,
    parE = c(1, 1),
    parS = c(5, 25),
    sim = 100,
    wiggle = TRUE,
    nnF = c(10, 20, 30)
  ), "Advise to use sim >= 50000 to achieve convergence")
  res_three_fut$oc$PrFutility
  expect_true(res_three_fut$oc$PrFutility > res_one_fut$oc$PrFutility)
})

test_that("the PrEfficacy increases with increase Efficacy looks", {
  set.seed(1989)
  expect_warning(res_eff <- ocPostprobDist(
    nnE = 30,
    truep = 0.4,
    deltaE = 0.1,
    deltaF = -0.1,
    tL = 0.6,
    tU = 0.6,
    parE = c(1, 1),
    parS = c(5, 25),
    sim = 100,
    wiggle = TRUE,
    nnF = 10
  ), "Advise to use sim >= 50000 to achieve convergence")

  res_eff$oc$PrEfficacy
  expect_warning(res_more_eff <- ocPostprobDist(
    nnE = c(10, 20, 30),
    truep = 0.4,
    deltaE = 0.1,
    deltaF = -0.1,
    tL = 0.6,
    tU = 0.6,
    parE = c(1, 1),
    parS = c(5, 25),
    sim = 100,
    wiggle = TRUE,
    nnF = 10
  ), "Advise to use sim >= 50000 to achieve convergence")
  res_more_eff$oc$PrEfficacy
  expect_true(res_more_eff$oc$PrEfficacy > res_eff$oc$PrEfficacy)
})

# ocPostprob  ---
test_that("ocPostprob gives results that are within range to stats::pbinom", {
  set.seed(1989)
  # Go criteria is P_E(truep >= 0.45) > 0.70
  # Stop criteria is P_E(truep <= 0.45) > 0.90
  res1 <- ocPostprob(
    nnE = 40, truep = 0.5, p0 = 0.45, p1 = 0.45, tL = 0.9, tU = 0.7,
    parE = c(1, 1), sim = 50000
  )
  # Pre-calculation indicate that :
  # Go criteria: 20 out of 40, means >= 50% response rate
  expect_equal(res1$oc$PrEfficacy, 0.56226)
  p.go <- 1 - pbinom(q = 20 - 1, size = 40, prob = 0.5)
  expect_true(abs(p.go - res1$oc$PrEfficacy) < 1e-3)
  # Stop criteria: 13 out of 40, means <= 32.5% response rate.
  expect_equal(res1$oc$PrFutility, 0.01998)
  p.stop <- pbinom(q = 13, size = 40, prob = 0.5)
  expect_true(abs(p.stop - res1$oc$PrFutility) < 1e-2)
})
