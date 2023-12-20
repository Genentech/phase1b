# h_get_distance ----
test_that("h_get_distance gives an error with one element numeric", {
  expect_equal(h_get_distance(10), integer(0))
})

test_that("h_get_distance gives results within range", {
  set.seed(1989)
  nn <- c(10, 20, 30)
  results <- h_get_distance(nn)
  expect_numeric(results, lower = -min(nn) / 2, upper = 30, len = 2)
})

test_that("h_get_distance gives an error with non sorted argument", {
  set.seed(1989)
  expect_error(h_get_distance(c(30, 20, 10)))
})

# h_get_looks ----
test_that("h_get_looks gives correct results if input is identical", {
  dist <- c(0, 5)
  results <- h_get_looks(dist = dist, nnE = c(10, 20, 30), nnF = c(10, 20, 30))
  expect_equal(results$nnrE, results$nnrF)
})

test_that("h_get_looks gives correct results if input is identical", {
  dist <- c(0, 5)
  results <- h_get_looks(dist = dist, nnE = c(10, 20, 30), nnF = c(10, 20, 30))
  expect_equal(results$nnrE, results$nnrF)
})

# h_get_decision ----
test_that("get_decision will give GO decision in favourable conditions", {
  tmp <- h_get_decision(
    nnr = c(10, 20, 30),
    truep = 0.5,
    # Go criteria is P_E(p > p1) > tU, where P_E(truep > 0.30) > 0.2
    # Stop criteria is P_E(p < p0) > tL, where P_E(truep > 0.20) > 0.3
    p0 = 0.2,
    p1 = 0.5,
    tL = 0.2,
    tU = 0.3,
    nnE = c(10, 20, 30),
    nnF = c(10, 20, 30)
  )
  expect_equal(tmp$decision, TRUE)
})

# h_get_oc ----
test_that("the probability results of get_oc are less than 1", {
  oc <- h_get_oc(
    all_sizes = sample(c(11, 14, 20), size = 10000, replace = TRUE),
    decision = sample(c(NA, TRUE, FALSE), size = 10000, replace = TRUE),
    nnrE = c(11, 14, 20),
    nnrF = c(11, 14, 20)
  )
  expect_true(oc$PrStopEarly && oc$PrFutility && oc$PrEarlyEff && oc$PrEfficacy < 1)
})

test_that("the ExpectedN is within range based on vector of looks", {
  all_sizes <- sample(c(11, 14, 20), size = 10000, replace = TRUE)
  oc <- h_get_oc(
    all_sizes = all_sizes,
    decision = sample(c(NA, TRUE, FALSE), size = 10000, replace = TRUE),
    nnrE = c(11, 14, 20),
    nnrF = c(11, 14, 20)
  )
  expect_number(oc$ExpectedN, lower = min(all_sizes), upper = max(all_sizes))
})

# ocPostprob ----
test_that("the sum of Eff, Fut, Gray zone probabiliy is 1", {
  set.seed(1989)
  expect_warning(res1 <- ocPostprob(
    nnE = 40, truep = 0.5, p0 = 0.45, p1 = 0.45, tL = 0.9, tU = 0.7,
    parE = c(1, 1), sim = 10000
  ), "Advise to use sim >= 50000 to achieve convergence")
  results <- sum(res1$oc[5:7])
  expect_equal(results, 1)
})

test_that("the PrFutility increases with increase futility looks", {
  set.seed(1989)
  expect_warning(res_fut <- ocPostprob(
    nnE = c(10, 20, 30), truep = 0.40, p0 = 0.20, p1 = 0.30, tL = 0.60, tU = 0.80, parE = c(1, 1),
    sim = 10000, wiggle = FALSE, randomdist = NULL, nnF = c(10, 20, 30)
  ), "Advise to use sim >= 50000 to achieve convergence")

  res_fut$oc$PrFutility
  expect_warning(res_one_fut <- ocPostprob(
    nnE = c(10, 20, 30), truep = 0.40, p0 = 0.20, p1 = 0.30, tL = 0.60, tU = 0.80, parE = c(1, 1),
    sim = 10000, wiggle = FALSE, randomdist = NULL, nnF = 10
  ), "Advise to use sim >= 50000 to achieve convergence")
  res_one_fut$oc$PrFutility
  expect_true(res_fut$oc$PrFutility > res_one_fut$oc$PrFutility)
})

test_that("the PrEfficacy increases with increase Efficacy looks", {
  set.seed(1989)
  expect_warning(res_eff <- ocPostprob(
    nnE = c(30), truep = 0.40, p0 = 0.20, p1 = 0.30, tL = 0.60, tU = 0.80, parE = c(1, 1),
    sim = 10000, wiggle = FALSE, randomdist = NULL, nnF = 30
  ), "Advise to use sim >= 50000 to achieve convergence")

  res_eff$oc$PrEfficacy
  expect_warning(res_more_eff <- ocPostprob(
    nnE = c(10, 20, 30), truep = 0.40, p0 = 0.20, p1 = 0.30, tL = 0.60, tU = 0.80, parE = c(1, 1),
    sim = 10000, wiggle = FALSE, randomdist = NULL, nnF = c(10, 20, 30)
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
