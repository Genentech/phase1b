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