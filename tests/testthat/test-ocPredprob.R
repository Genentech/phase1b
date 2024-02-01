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
  expect_equal(result$all_sizes, 30)
  expect_list(result)
})
