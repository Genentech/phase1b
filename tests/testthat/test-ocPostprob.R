# get_distance (helper function) ----
test_that("get_distance gives an error with one element numeric", {
  expect_equal(get_distance(10), integer(0))
})

test_that("get_distance gives results within range", {
  set.seed(1989)
  nn <- c(10, 20, 30)
  results <- get_distance(nn)
  expect_numeric(results, lower = -min(nn) / 2, upper = 30, len = 2) # TO DO fix error
})

test_that("get_distance will give error with non sorted argument", {
  set.seed(1989)
  expect_error(get_distance(c(30, 20, 10)))
})

# get_looks (helper function) ----
test_that("get_looks gives correct results if input is identical", {
  dist <- c(0, 5)
  results <- get_looks(dist = dist, nnE = c(10, 20, 30), nnF = c(10, 20, 30))
  expect_equal(results$nnrE, results$nnrF)
})

test_that("get_looks gives correct results if input is identical", {
  dist <- c(0, 5)
  results <- get_looks(dist = dist, nnE = c(10, 20, 30), nnF = c(10, 20, 30))
  expect_equal(results$nnrE, results$nnrF)
})

# get_decision (helper function) --
# Stop criteria for Efficacy :
# P_E(p > p1) > tU, where P_E(truep > 0.30) > 0.8
# Stop criteria for Futility :
# P_E(p < p0) > tL, where P_E(truep > 0.20) > 0.5
# It is a Go decision usually when the threshold to Go is Low
test_that("get_decision will give GO decision in favourable conditions", {
  tmp <- get_decision(
    nnr = c(10, 20, 30),
    truep = 0.5,
    p0 = 0.2,
    p1 = 0.5,
    tL = 0.2,
    tU = 0.3,
    nnE = c(10, 20, 30),
    nnF = c(10, 20, 30)
  )
  expect_equal(tmp$decision, TRUE)
})

# get_oc ----
test_that("the probability results of get_oc are less than 1", {
  oc <- get_oc(
    all_sizes = sample(c(11, 14, 20), 10000, replace = TRUE),
    decision = sample(c(NA, TRUE, FALSE), 10000, replace = TRUE),
    nnrE = c(11, 14, 20),
    nnrF = c(11, 14, 20)
  )
  expect_true(oc$PrStopEarly && oc$PrFutility && oc$PrEarlyEff && oc$PrEfficacy < 1)
})

test_that("the ExpectedN is within range based on vector of looks", {
  oc <- get_oc(
    all_sizes = sample(c(11, 14, 20), 10000, replace = TRUE),
    decision = sample(c(NA, TRUE, FALSE), 10000, replace = TRUE),
    nnrE = c(11, 14, 20),
    nnrF = c(11, 14, 20)
  )
  expect_number(oc$ExpectedN, lower = min(all_sizes), upper = max(all_sizes))
})

# ocPostprob ----
test_that("the sum of Eff, Fut, Gray zone probabiliy is 1", {
  set.seed(1989)
  res1 <- ocPostprob(
    nnE = 40, truep = 0.5, p0 = 0.45, p1 = 0.45, tL = 0.9, tU = 0.7,
    parE = c(1, 1), sim = 50000
  )
  results <- sum(res1$oc[5:7])
  expect_equal(results, 1)
})

test_that("the PrFutility increases with increase futility looks", {
  set.seed(1989)
  res_fut <- ocPostprob(
    nnE = c(10, 20, 30), truep = 0.40, p0 = 0.20, p1 = 0.30, tL = 0.60, tU = 0.80, parE = c(1, 1),
    sim = 1000, wiggle = FALSE, randomdist = NULL, nnF = c(10, 20, 30)
  )

  res_fut$oc$PrFutility # 0.048
  res_no_fut <- ocPostprob(
    nnE = c(10, 20, 30), truep = 0.40, p0 = 0.20, p1 = 0.30, tL = 0.60, tU = 0.80, parE = c(1, 1),
    sim = 1000, wiggle = FALSE, randomdist = NULL, nnF = c(10)
  )
  res_no_fut$oc$PrFutility # 0.031
  expect_true(res_fut$oc$PrFutility > res_no_fut$oc$PrFutility)
})

test_that("the PrFfficacy increases with increase Efficacy looks", {
  set.seed(1989)
  res_eff <- ocPostprob(
    nnE = c(30), truep = 0.40, p0 = 0.20, p1 = 0.30, tL = 0.60, tU = 0.80, parE = c(1, 1),
    sim = 1000, wiggle = FALSE, randomdist = NULL, nnF = c(30)
  )

  res_eff$oc$PrEfficacy # 0.691
  res_more_eff <- ocPostprob(
    nnE = c(10, 20, 30), truep = 0.40, p0 = 0.20, p1 = 0.30, tL = 0.60, tU = 0.80, parE = c(1, 1),
    sim = 1000, wiggle = FALSE, randomdist = NULL, nnF = c(10, 20, 30)
  )
  res_more_eff$oc$PrEfficacy # 0.728
  expect_true(res_more_eff$oc$PrEfficacy > res_more_eff$oc$PrEfficacy)
})

# expect equal with tolerance ---
####   P(ORR $\ge$ Min PP) must be high for going, we assume e.g. 70% for go
####   P(ORR $\ge$ Min PP) must be low for stopping, we assume e.g. 10% for go
# Pre-calculation indicate that :
##### go criteria: 20 out of 40, means >= 50% response rate
##### stop criteria: 13 out of 40, means <= 32.5% response rate
test_that("ocPostprob gives results that are within range to stats::pbinom", {
  set.seed(1989)
  res1 <- ocPostprob(
    nnE = 40, truep = 0.5, p0 = 0.45, p1 = 0.45, tL = 0.9, tU = 0.7,
    parE = c(1, 1), sim = 50000
  )
  res1$oc$PrEfficacy # 0.5623
  p.go <- 1 - pbinom(q = 20 - 1, size = 40, prob = 0.5)
  p.go # 0.5626853
  expect_true((p.go - res1$oc$PrEfficacy) < 1e-3)
})

test_that("ocPostprob gives results that are within range to stats::pbinom", {
  set.seed(1989)
  res1 <- ocPostprob(
    nnE = 40, truep = 0.5, p0 = 0.45, p1 = 0.45, tL = 0.9, tU = 0.7,
    parE = c(1, 1), sim = 50000
  )
  res1$oc$PrFutility # 0.01998
  p.stop <- pbinom(q = 13, size = 40, prob = 0.5)
  p.stop # 0.01923865
  expect_true((p.stop - res1$oc$PrFutility) < 1e-2)
})
