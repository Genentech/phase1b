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
test_that("h_decision_two_RctpredprobDist gives correct result and list when relativeDelta = TRUE", {
  set.seed(1989)
  example_dist <- h_get_distance(nn = c(10, 20, 30))
  looks <- h_get_looks(dist = example_dist, nnE = c(10, 20, 30), nnF = c(10, 20, 30))
  looks_nnrE <- looks$nnrE
  looks_nnrF <- looks$nnrF
  orig_nnE <- c(10, 20, 30)
  orig_nnF <- c(10, 20, 30)
  orig_nnr <- unique(sort(c(orig_nnE, orig_nnF)))
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
    randRatio = 1,
    orig_nnr = orig_nnr
  )
  expect_flag(result$decision, TRUE)
  expect_equal(result$all_sizes, 30)
  expect_equal(result$nActive, 15)
  expect_equal(result$nControl, 15)
  expect_list(result)
})

test_that("h_decision_two_RctpredprobDist gives correct result and list when relativeDelta = FALSE", {
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
    randRatio = 1,
    orig_nnr = orig_nnr
  )
  expect_flag(result$decision, TRUE)
  expect_equal(result$all_sizes, 30)
  expect_equal(result$nActive, 15)
  expect_equal(result$nControl, 15)
  expect_list(result)
})

# ocRctPredprobDist ----
test_that("the sum of Eff, Fut, Gray zone probability is 1", {
  set.seed(2000)
  expect_warning(results <- ocRctPredprobDist(
    nnE = c(10, 20, 30),
    pE = 0.4,
    pS = 0.3,
    deltaE = 0.2,
    deltaF = 0.1,
    phiU = 0.8,
    phiFu = 0.2,
    relativeDelta = FALSE,
    tT = 0.6,
    tF = 0.4,
    parE = c(a = 1, b = 1),
    parS = c(a = 1, b = 1),
    weights = 1,
    weightsS = 1,
    randRatio = 1,
    sim = 50,
    wiggle = FALSE,
    nnF = c(10, 20, 30),
    decision1 = TRUE
  ), "Advise to use sim >= 50000 to achieve convergence")
  results <- sum(results$oc[5:7])
  expect_equal(results, 1)
})

test_that("ocRctPredprobDist gives higher PrFutility with decreased pE", {
  set.seed(1989)
  expect_warning(res_low_truep <- ocRctPredprobDist(
    nnE = c(10, 20, 30),
    pE = 0.4,
    pS = 0.3,
    deltaE = 0.2,
    deltaF = 0.1,
    phiU = 0.8,
    phiFu = 0.2,
    relativeDelta = FALSE,
    tT = 0.6,
    tF = 0.4,
    parE = c(a = 1, b = 1),
    parS = c(a = 1, b = 1),
    weights = 1,
    weightsS = 1,
    randRatio = 1,
    sim = 50,
    wiggle = FALSE,
    nnF = c(10, 20, 30),
    decision1 = TRUE
  ), "Advise to use sim >= 50000 to achieve convergence")
  expect_warning(res <- ocRctPredprobDist(
    nnE = c(10, 20, 30),
    pE = 0.6,
    pS = 0.3,
    deltaE = 0.2,
    deltaF = 0.1,
    phiU = 0.8,
    phiFu = 0.2,
    relativeDelta = FALSE,
    tT = 0.6,
    tF = 0.4,
    parE = c(a = 1, b = 1),
    parS = c(a = 1, b = 1),
    weights = 1,
    weightsS = 1,
    randRatio = 1,
    sim = 50,
    wiggle = FALSE,
    nnF = c(10, 20, 30),
    decision1 = TRUE
  ), "Advise to use sim >= 50000 to achieve convergence")
  expect_true(res$oc$PrFutility < res_low_truep$oc$PrFutility)
})

test_that("ocRctPredprobDist gives higher PrEfficacy with increased pE", {
  set.seed(1989)
  expect_warning(res_eff <- ocRctPredprobDist(
    nnE = c(10, 20, 30),
    pE = 0.3,
    pS = 0.3,
    deltaE = 0.2,
    deltaF = 0.1,
    phiU = 0.8,
    phiFu = 0.2,
    relativeDelta = FALSE,
    tT = 0.6,
    tF = 0.4,
    parE = c(a = 1, b = 1),
    parS = c(a = 1, b = 1),
    weights = 1,
    weightsS = 1,
    randRatio = 1,
    sim = 50,
    wiggle = FALSE,
    nnF = c(10, 20, 30),
    decision1 = TRUE
  ), "Advise to use sim >= 50000 to achieve convergence")
  expect_warning(res_high_truep <- ocRctPredprobDist(
    nnE = c(10, 20, 30),
    pE = 0.6,
    pS = 0.3,
    deltaE = 0.2,
    deltaF = 0.1,
    phiU = 0.8,
    phiFu = 0.2,
    relativeDelta = FALSE,
    tT = 0.6,
    tF = 0.4,
    parE = c(a = 1, b = 1),
    parS = c(a = 1, b = 1),
    weights = 1,
    weightsS = 1,
    randRatio = 1,
    sim = 50,
    wiggle = FALSE,
    nnF = c(10, 20, 30),
    decision1 = TRUE
  ), "Advise to use sim >= 50000 to achieve convergence")
  expect_true(res_high_truep$oc$PrEfficacy > res_eff$oc$PrEfficacy)
})
