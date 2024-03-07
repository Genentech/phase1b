# h_get_decision_one_predprobDist ----

test_that("h_decision_one_predprobDist gives correct result and list", {
  set.seed(1989)
  example_dist <- h_get_distance(nn = c(10, 20, 30))
  looks <- h_get_looks(dist = example_dist, nnE = c(10, 20, 30), nnF = c(10, 20, 30))
  looks_nnrE <- looks$nnrE
  looks_nnrF <- looks$nnrF
  result <- h_decision_one_predprobDist(
    nnE = looks$nnrE,
    nnF = looks$nnrF,
    nnr = c(10, 20, 30),
    truep = 0.4,
    xS = 5,
    nS = 10,
    parE = c(1, 1),
    parS = c(1, 1),
    tT = 0.6,
    phiU = 0.8,
    phiL = 0.6,
    deltaE = 0.1,
    deltaF = 0.1,
    weights = 1,
    weightsS = 1,
    relativeDelta = FALSE
  )
  expect_flag(result$decision, FALSE)
  expect_equal(result$all_sizes, 30)
  expect_list(result)
})

# h_get_decision12_predprobDist ----

test_that("h_get_decision_two_predprobDist gives correct result and list", {
  set.seed(1989)
  example_dist <- h_get_distance(nn = c(10, 20, 30))
  looks <- h_get_looks(dist = example_dist, nnE = c(10, 20, 30), nnF = c(10, 20, 30))
  looks_nnrE <- looks$nnrE
  looks_nnrF <- looks$nnrF
  result <- h_get_decision_two_predprobDist(
    nnE = looks$nnrE,
    nnF = looks$nnrF,
    nnr = c(10, 20, 30),
    truep = 0.4,
    xS = 5,
    nS = 10,
    parE = c(1, 1),
    parS = c(1, 1),
    tT = 0.6,
    phiFu = 0.8,
    phiL = 0.6,
    deltaE = 0.1,
    deltaF = 0.1,
    weights = 1,
    weightsS = 1,
    relativeDelta = FALSE
  )
  expect_flag(result$decision, FALSE)
  expect_equal(result$all_sizes, 30)
  expect_list(result)
})
