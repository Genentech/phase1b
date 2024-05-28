# h_decision_one_RctPredProbDist----
test_that("h_decision_one_RctPredProbDist gives correct result and list when relativeDelta = TRUE", {
  set.seed(1989)
  example_dist <- h_get_distance(nn = c(10, 20, 30))
  looks <- h_get_looks(dist = example_dist, nnE = c(10, 20, 30), nnF = c(10, 20, 30))
  looks_nnrE <- looks$nnrE
  looks_nnrF <- looks$nnrF
  result <- h_decision_one_RctpredprobDist(
    nnr = c(10, 20, 30),
    nnE = looks$nnrE,
    nnF = looks$nnrF,
    pE = 0.6,
    pS = 0.3,
    parE = c(1, 1),
    parS = c(1, 1),
    tT = 0.7,
    phiU = 0.7,
    phiL = 0.3,
    deltaE = 0.1,
    deltaF = 0.1,
    weights = 1,
    weightsS = 1,
    relativeDelta = TRUE,
    randRatio = 1
  )
  expect_flag(result$decision, TRUE)
  expect_equal(result$all_sizes, 30)
  expect_equal(result$nActive, 15)
  expect_equal(result$nControl, 15)
  expect_list(result)
})

test_that("h_decision_one_RctPredProbDist gives correct result and list when relativeDelta = FALSE", {
  set.seed(1989)
  example_dist <- h_get_distance(nn = c(10, 20, 30))
  looks <- h_get_looks(dist = example_dist, nnE = c(10, 20, 30), nnF = c(10, 20, 30))
  looks_nnrE <- looks$nnrE
  looks_nnrF <- looks$nnrF
  result <- h_decision_one_RctpredprobDist(
    nnr = c(10, 20, 30),
    nnE = looks$nnrE,
    nnF = looks$nnrF,
    pE = 0.6,
    pS = 0.3,
    parE = c(1, 1),
    parS = c(1, 1),
    tT = 0.7,
    phiU = 0.7,
    phiL = 0.3,
    deltaE = 0.1,
    deltaF = 0.1,
    weights = 1,
    weightsS = 1,
    relativeDelta = FALSE,
    randRatio = 1
  )
  expect_flag(result$decision, TRUE)
  expect_equal(result$all_sizes, 30)
  expect_equal(result$nActive, 15)
  expect_equal(result$nControl, 15)
  expect_list(result)
})

# h_get_decision_two_RctPredProbDist----
test_that("h_decision_two_RctPredProbDist gives correct result and list when relativeDelta = TRUE", {
  set.seed(1989)
  example_dist <- h_get_distance(nn = c(10, 20, 30))
  looks <- h_get_looks(dist = example_dist, nnE = c(10, 20, 30), nnF = c(10, 20, 30))
  looks_nnrE <- looks$nnrE
  looks_nnrF <- looks$nnrF
  result <- h_decision_two_RctpredprobDist(
    nnr = c(10, 20, 30),
    nnE = looks$nnrE,
    nnF = looks$nnrF,
    pE = 0.6,
    pS = 0.3,
    parE = c(1, 1),
    parS = c(1, 1),
    tT = 0.7,
    tF = 0.3,
    phiU = 0.3,
    phiFu = 0.7,
    deltaE = 0.1,
    deltaF = 0.1,
    weights = 1,
    weightsS = 1,
    relativeDelta = TRUE,
    randRatio = 1
  )
  expect_flag(result$decision, TRUE)
  expect_equal(result$all_sizes, 30)
  expect_equal(result$nActive, 15)
  expect_equal(result$nControl, 15)
  expect_list(result)
})

test_that("h_decision_two_RctPredProbDist gives correct result and list when relativeDelta = FALSE", {
  set.seed(1989)
  example_dist <- h_get_distance(nn = c(10, 20, 30))
  looks <- h_get_looks(dist = example_dist, nnE = c(10, 20, 30), nnF = c(10, 20, 30))
  looks_nnrE <- looks$nnrE
  looks_nnrF <- looks$nnrF
  result <- h_decision_two_RctpredprobDist(
    nnr = c(10, 20, 30),
    nnE = looks$nnrE,
    nnF = looks$nnrF,
    pE = 0.6,
    pS = 0.3,
    parE = c(1, 1),
    parS = c(1, 1),
    tT = 0.7,
    tF = 0.3,
    phiU = 0.3,
    phiFu = 0.7,
    deltaE = 0.1,
    deltaF = 0.1,
    weights = 1,
    weightsS = 1,
    relativeDelta = FALSE,
    randRatio = 1
  )
  expect_flag(result$decision, TRUE)
  expect_equal(result$all_sizes, 30)
  expect_equal(result$nActive, 15)
  expect_equal(result$nControl, 15)
  expect_list(result)
})
