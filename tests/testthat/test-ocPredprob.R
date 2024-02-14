# h_get_decision_one_predprob ----

test_that("h_get_decision_one_predprob gives correct result and list", {
  set.seed(1989)
  result <- h_get_decision_one_predprob(
    nnr = c(10, 20, 30),
    truep = 0.4,
    p0 = 0.25,
    parE = c(1, 1),
    nnE = c(10, 20, 30),
    nnF = c(10, 20, 30),
    tT = 0.6,
    phiU = 0.8,
    phiL = 0.3
  )
  expect_flag(result$decision, TRUE)
  expect_equal(result$all_sizes, 20)
  expect_list(result)
})

# h_get_decision_two_predprob ----
test_that("h_get_decision_two_predprob gives correct result and list", {
  set.seed(1989)
  result <- h_get_decision_two_predprob(
    nnr = c(10, 20, 30),
    truep = 0.4,
    p0 = 0.25,
    p1 = 0.30,
    parE = c(1, 1),
    nnE = c(10, 20, 30),
    nnF = c(10, 20, 30),
    tT = 0.6,
    tF = 0.6,
    phiFu = 0.8,
    phiU = 0.3
  )
  expect_flag(result$decision, TRUE)
  expect_equal(result$all_sizes, 10)
  expect_list(result)
})

# h_get_oc_predprob ----
test_that("h_get_oc_predprob gives correct results", {
  set.seed(1989)
  oc <- h_get_oc_predprob(
    all_sizes = sample(c(11, 14, 20), size = 1000, replace = TRUE),
    nnr = c(10, 20, 30),
    decision = sample(c(NA, TRUE, FALSE), size = 1000, replace = TRUE)
  )
  expect_equal(oc$PrStopEarly, 1, tolerance = 1e-4)
  expect_true(oc$PrFutility < 1)
  expect_true(oc$PrEarlyEff < 1)
  expect_true(oc$PrEfficacy < 1)
  expect_data_frame(oc, any.missing = FALSE)
  expect_number(oc$ExpectedN, lower = 11, upper = 20)
})

# oc_Predprob
