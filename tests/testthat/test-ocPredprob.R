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

# oc_Predprob ----
test_that("the sum of Eff, Fut, Gray zone probability is 1", {
  set.seed(1989)
  expect_warning(results <- ocPredprob(
    nnE = c(10, 20),
    truep = 0.4,
    p0 = 0.25,
    tT = 0.6,
    phiL = 0.2,
    phiU = 0.8,
    parE = c(1, 1),
    sim = 50,
    wiggle = FALSE,
    decision1 = TRUE
  ), "Advise to use sim >= 50000 to achieve convergence")
  results <- sum(results$oc[5:7])
  expect_equal(results, 1)
})

test_that("the PrFutility increases with increase futility looks", {
  set.seed(1989)
  expect_warning(result_one_fut <- ocPredprob(
    nnE = c(10, 20),
    truep = 0.4,
    p0 = 0.25,
    tT = 0.6,
    phiL = 0.2,
    phiU = 0.8,
    parE = c(1, 1),
    sim = 50,
    nnF = 10,
    wiggle = FALSE,
    decision1 = TRUE
  ), "Advise to use sim >= 50000 to achieve convergence")
  expect_warning(result_three_fut <- ocPredprob(
    nnE = c(10, 20),
    truep = 0.4,
    p0 = 0.25,
    tT = 0.6,
    phiL = 0.2,
    phiU = 0.8,
    parE = c(1, 1),
    sim = 50,
    nnF = c(10, 20),
    wiggle = FALSE,
    decision1 = TRUE
  ), "Advise to use sim >= 50000 to achieve convergence")
  expect_true(result_three_fut$oc$PrFutility > result_one_fut$oc$PrFutility)
})

test_that("the PrEfficacy increases with increase Efficacy looks", {
  set.seed(1989)
  expect_warning(result_eff <- ocPredprob(
    nnE = c(10, 20),
    truep = 0.6,
    p0 = 0.2,
    p1 = 0.2,
    tT = 0.6,
    tF = 0.4,
    phiU = 0.2,
    phiFu = 0.8,
    parE = c(1, 1),
    sim = 500,
    wiggle = FALSE,
    nnF = c(10, 20),
    decision1 = FALSE
  ), "Advise to use sim >= 50000 to achieve convergence")
  expect_warning(result_more_eff <- ocPredprob(
    nnE = c(10, 20, 40),
    truep = 0.6,
    p0 = 0.2,
    p1 = 0.2,
    tT = 0.6,
    tF = 0.4,
    phiU = 0.2,
    phiFu = 0.8,
    parE = c(1, 1),
    sim = 500,
    wiggle = FALSE,
    nnF = c(10, 20),
    decision1 = FALSE
  ), "Advise to use sim >= 50000 to achieve convergence")
  expect_true(result_more_eff$oc$PrEfficacy > result_eff$oc$PrEfficacy)
})
