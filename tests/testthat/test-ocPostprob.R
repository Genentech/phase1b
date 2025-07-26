# h_dist0 ----
test_that("h_dist0 works as expected", {
  expect_identical(h_dist0(c(10, 20, 40)), 4)
  expect_identical(h_dist0(c(10, 19, 40)), 4)
  expect_identical(h_dist0(c(10, 18, 40)), 3)
  expect_identical(h_dist0(c(1, 2, 3)), 0)
  expect_identical(h_dist0(c(1, 3, 5)), 0)
  expect_identical(h_dist0(c(3, 5, 7)), 0)
  expect_identical(h_dist0(c(4, 7, 10)), 1)
})

# h_get_distance ----
test_that("h_get_distance gives an error with one element numeric", {
  set.seed(1989)
  expect_error(h_get_distance(10), "has length 1")
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

test_that("h_get_looks together with h_get_distance always gives unique looks", {
  nn <- c(10, 20, 30)
  set.seed(123)
  for (i in seq_len(1000)) {
    dist <- h_get_distance(nn = nn)
    nnr <- h_get_looks(dist = dist, nnE = nn, nnF = nn)
    nnr <- as.integer(nnr$nnrE)
    expect_integer(nnr, unique = TRUE)
  }
})

# h_get_decision ----
test_that("h_get_decision will give GO decision in favourable conditions", {
  set.seed(1989)
  orig_nnE <- c(10, 20, 30)
  orig_nnF <- c(10, 20, 30)
  orig_nnr <- unique(orig_nnE, orig_nnF)
  result <- h_get_decision(
    nnr = c(10, 20, 30),
    truep = 0.5,
    # Go criteria is P_E(p > p1) > tU, where P_E(truep > 0.30) > 0.2
    # Stop criteria is P_E(p < p0) > tL, where P_E(truep > 0.20) > 0.3
    p0 = 0.2,
    p1 = 0.5,
    tL = 0.2,
    tU = 0.3,
    parE = c(1,1),
    nnE = c(10, 20, 30),
    nnF = c(10, 20, 30),
    orig_nnr = orig_nnr
  )
  expect_equal(result$decision, TRUE)
  expect_equal(result$all_sizes, 20)
})

# h_get_oc ----
test_that("the probability results of h_get_oc are less than 1", {
  set.seed(1989)
  oc <- h_get_oc(
    all_sizes = sample(c(11, 14, 20), size = 10000, replace = TRUE),
    Nmax = 20,
    decision = sample(c(NA, TRUE, FALSE), size = 10000, replace = TRUE)
  )
  expect_true(oc$PrStopEarly < 1)
  expect_true(oc$PrFutility < 1)
  expect_true(oc$PrEarlyEff < 1)
  expect_true(oc$PrEfficacy < 1)
})

test_that("the ExpectedN is within range based on vector of looks", {
  set.seed(1989)
  all_sizes <- sample(c(11, 14, 20), size = 10000, replace = TRUE)
  oc <- h_get_oc(
    all_sizes = all_sizes,
    Nmax = 20,
    decision = sample(c(NA, TRUE, FALSE), size = 10000, replace = TRUE)
  )
  expect_number(oc$ExpectedN, lower = min(all_sizes), upper = max(all_sizes))
})

# ocPostprob ----
test_that("the sum of Eff, Fut, Gray zone probabiliy is 1", {
  set.seed(1989)
  expect_warning(result <- ocPostprob(
    nnE = 40, truep = 0.5, p0 = 0.45, p1 = 0.45, tL = 0.9, tU = 0.7,
    parE = c(1, 1), sim = 10000
  ), "Advise to use sim >= 50000 to achieve convergence")
  result_sum <- sum(result$oc[5:7])
  expect_equal(result_sum, 1)
})

test_that("the PrFutility increases with increase futility looks", {
  set.seed(1989)
  expect_warning(result_fut <- ocPostprob(
    nnE = c(10, 20, 30), truep = 0.40, p0 = 0.20, p1 = 0.30, tL = 0.60, tU = 0.80, parE = c(1, 1),
    sim = 10000, wiggle = FALSE, nnF = c(10, 20, 30)
  ), "Advise to use sim >= 50000 to achieve convergence")

  result_fut$oc$PrFutility
  expect_warning(result_one_fut <- ocPostprob(
    nnE = c(10, 20, 30), truep = 0.40, p0 = 0.20, p1 = 0.30, tL = 0.60, tU = 0.80, parE = c(1, 1),
    sim = 10000, wiggle = FALSE, nnF = 10
  ), "Advise to use sim >= 50000 to achieve convergence")
  result_one_fut$oc$PrFutility
  expect_true(result_fut$oc$PrFutility > result_one_fut$oc$PrFutility)
})

test_that("the PrEfficacy increases with increase Efficacy looks", {
  set.seed(1989)
  expect_warning(result_eff <- ocPostprob(
    nnE = 30, truep = 0.40, p0 = 0.20, p1 = 0.30, tL = 0.60, tU = 0.80, parE = c(1, 1),
    sim = 10000, wiggle = FALSE, nnF = 30
  ), "Advise to use sim >= 50000 to achieve convergence")

  result_eff$oc$PrEfficacy
  expect_warning(result_more_eff <- ocPostprob(
    nnE = c(10, 20, 30), truep = 0.40, p0 = 0.20, p1 = 0.30, tL = 0.60, tU = 0.80, parE = c(1, 1),
    sim = 10000, wiggle = FALSE, nnF = c(10, 20, 30)
  ), "Advise to use sim >= 50000 to achieve convergence")
  result_more_eff$oc$PrEfficacy
  expect_true(result_more_eff$oc$PrEfficacy > result_eff$oc$PrEfficacy)
})

# ocPostprob  ---
test_that("ocPostprob gives results that are within range to stats::pbinom", {
  set.seed(1989)
  # Go criteria is P_E(truep >= 0.45) > 0.70
  # Stop criteria is P_E(truep <= 0.45) > 0.90
  result <- ocPostprob(
    nnE = 40, truep = 0.5, p0 = 0.45, p1 = 0.45, tL = 0.9, tU = 0.7,
    parE = c(1, 1), sim = 50000
  )
  # Pre-calculation indicate that :
  # Go criteria: 20 out of 40, means >= 50% response rate
  expect_equal(result$oc$PrEfficacy, 0.56226)
  p.go <- 1 - pbinom(q = 20 - 1, size = 40, prob = 0.5)
  expect_true(abs(p.go - result$oc$PrEfficacy) < 1e-3)
  # Stop criteria: 13 out of 40, means <= 32.5% response rate.
  expect_equal(result$oc$PrFutility, 0.01998)
  p.stop <- pbinom(q = 13, size = 40, prob = 0.5)
  expect_true(abs(p.stop - result$oc$PrFutility) < 1e-2)
})

test_that("two function calls that differ in parE does not give the same result.", {
  set.seed(1989)
  expect_warning(result_uniform_hard_coded <- ocPostprob(
    nnE = 30,
    truep = 0.4,
    p0 = 0.1,
    p1 = 0.3,
    tL = 0.8,
    tU = 0.3,
    parE = c(1, 1), # weak prior gives more PrGrayZone
    sim = 100,
    wiggle = TRUE,
    nnF = 30), "Advise to use sim >= 50000 to achieve convergence")
  # result_uniform_hard_coded$oc
  expect_warning(result_no_hard_code <- ocPostprob(
    nnE = 30,
    truep = 0.4,
    p0 = 0.1,
    p1 = 0.3,
    tL = 0.8,
    tU = 0.3,
    parE = c(4, 10), # stronger prior gives higher PrEfficacy
    sim = 100,
    wiggle = TRUE,
    nnF = 30), "Advise to use sim >= 50000 to achieve convergence")
  # result_no_hard_code$oc
  expect_true(result_no_hard_code$oc["PrEfficacy"] > result_uniform_hard_coded$oc["PrEfficacy"])
  expect_true(result_no_hard_code$oc["PrGrayZone"] < result_uniform_hard_coded$oc["PrGrayZone"])
}
)
