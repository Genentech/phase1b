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
    deltaF = -0.1,
    weights = 1,
    weightsS = 1,
    relativeDelta = FALSE
  )
  expect_flag(result$decision, FALSE)
  expect_equal(result$all_sizes, 30)
  expect_list(result)
})

# h_get_decision_two_predprobDist ----

test_that("h_decision_two_predprobDist gives correct result and list", {
  set.seed(1989)
  example_dist <- h_get_distance(nn = c(10, 20, 30))
  looks <- h_get_looks(dist = example_dist, nnE = c(10, 20, 30), nnF = c(10, 20, 30))
  looks_nnrE <- looks$nnrE
  looks_nnrF <- looks$nnrF
  result <- h_decision_two_predprobDist(
    nnE = looks$nnrE,
    nnF = looks$nnrF,
    nnr = c(10, 20, 30),
    truep = 0.4,
    xS = 5,
    nS = 10,
    parE = c(1, 1),
    parS = c(1, 1),
    tT = 0.6,
    tF = 0.6,
    phiFu = 0.6,
    phiU = 0.8,
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

# ocPredprobDist ----

test_that("ocPredprobDist gives correct result and list when relativeDelta = FALSE", {
  set.seed(1989)
  expect_warning(
    {
      result <- ocPredprobDist(
        nnE = c(10, 20, 30),
        truep = 0.40,
        deltaE = 0.5,
        deltaF = 0.5,
        relativeDelta = FALSE,
        tT = 0.6,
        phiU = 0.80,
        phiFu = 0.7,
        parE = c(1, 1),
        parS = c(5, 25),
        weights = 1,
        weightsS = 1,
        sim = 50,
        nnF = c(10, 20, 30),
        wiggle = TRUE,
        decision1 = FALSE
      )
    },
    "achieve convergence"
  )
  result_sum <- sum(result$oc[5:7])
  expect_equal(result_sum, 1)
})

test_that("ocPredprobDist gives correct result and list when relativeDelta = TRUE", {
  set.seed(20)
  expect_warning(result <- ocPredprobDist(
    nnE = c(10, 20, 30),
    truep = 0.40,
    deltaE = 0.5,
    deltaF = 0.5,
    relativeDelta = TRUE,
    tT = 0.6,
    phiU = 0.80,
    phiFu = 0.7,
    parE = c(1, 1),
    parS = c(5, 25),
    weights = 1,
    weightsS = 1,
    sim = 50,
    nnF = c(10, 20, 30),
    wiggle = TRUE,
    decision1 = FALSE
  ), "Advise to use sim >= 50000 to achieve convergence")
  result_sum <- sum(result$oc[5:7])
  expect_equal(result_sum, 1)
})

test_that("ocPredprobDist gives higher PrFutility with more futility looks", {
  set.seed(1989)
  expect_warning(result_one_fut <- ocPredprobDist(
    nnE = c(10, 20, 30),
    truep = 0.40,
    deltaE = 0.5,
    deltaF = 0.5,
    relativeDelta = TRUE,
    tT = 0.6,
    phiU = 0.80,
    phiFu = 0.7,
    parE = c(1, 1),
    parS = c(5, 25),
    weights = 1,
    weightsS = 1,
    sim = 50,
    nnF = 10,
    wiggle = TRUE,
    decision1 = FALSE
  ), "Advise to use sim >= 50000 to achieve convergence")
  expect_warning(result_three_fut <- ocPredprobDist(
    nnE = c(10, 20, 30),
    truep = 0.40,
    deltaE = 0.5,
    deltaF = 0.5,
    relativeDelta = TRUE,
    tT = 0.6,
    phiU = 0.80,
    phiFu = 0.7,
    parE = c(1, 1),
    parS = c(5, 25),
    weights = 1,
    weightsS = 1,
    sim = 50,
    nnF = c(10, 20, 30),
    wiggle = TRUE,
    decision1 = FALSE
  ), "Advise to use sim >= 50000 to achieve convergence")
  expect_true(result_three_fut$oc$PrFutility > result_one_fut$oc$PrFutility)
})

test_that("ocPredprobDist gives higher PrEfficacy with more efficacy looks", {
  set.seed(2020)
  expect_warning(result_two_eff <- ocPredprobDist(
    nnE = 30,
    truep = 0.40,
    deltaE = 0.1,
    deltaF = 0.1,
    relativeDelta = TRUE,
    tT = 0.6,
    phiU = 0.80,
    phiL = 0.20,
    parE = c(1, 1),
    parS = c(5, 25),
    weights = 1,
    weightsS = 1,
    sim = 50,
    nnF = 10,
    wiggle = TRUE,
    decision1 = TRUE
  ), "Advise to use sim >= 50000 to achieve convergence")
  expect_warning(result_three_eff <- ocPredprobDist(
    nnE = c(10, 20, 30),
    truep = 0.40,
    deltaE = 0.1,
    deltaF = 0.1,
    relativeDelta = TRUE,
    tT = 0.6,
    phiU = 0.80,
    phiL = 0.20,
    parE = c(1, 1),
    parS = c(5, 25),
    weights = 1,
    weightsS = 1,
    sim = 50,
    nnF = 10,
    wiggle = TRUE,
    decision1 = TRUE
  ), "Advise to use sim >= 50000 to achieve convergence")
  expect_true(result_three_eff$oc$PrEfficacy > result_two_eff$oc$PrEfficacy)
})
