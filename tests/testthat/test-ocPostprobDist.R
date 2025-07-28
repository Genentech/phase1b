# h_get_decisionDist ----
test_that("h_get_decisionDist gives correct result when relativeDelta = TRUE", {
  set.seed(1989)
  example_dist <- h_get_distance(nn = c(10, 20, 30))
  looks <- h_get_looks(dist = example_dist, nnE = c(10, 20, 30), nnF = c(10, 20, 30))
  looks_nnrE <- looks$nnrE
  looks_nnrF <- looks$nnrF
  orig_nnE <- c(10, 20, 30)
  orig_nnF <- c(10, 20, 30)
  orig_nnr <- unique(sort(c(orig_nnE, orig_nnF)))
  # input values from above arguments
  result <- h_get_decisionDist(
    nnr = c(10, 20, 30),
    nnrE = looks_nnrE,
    nnrF = looks_nnrF,
    truep = 0.4,
    parE = c(1, 1),
    tL = 0.8,
    tU = 0.6,
    deltaF = 0.1,
    deltaE = 0.2,
    relativeDelta = TRUE,
    orig_nnr = orig_nnr
  )
  expect_list(result, any.missing = FALSE)
  expect_equal(result$decision, NA)
  expect_equal(result$all_sizes, 30)
})

test_that("h_get_decisionDist gives correct result relativeDelta = FALSE", {
  set.seed(1989)
  example_dist <- h_get_distance(nn = c(10, 20, 30))
  looks <- h_get_looks(dist = example_dist, nnE = c(10, 20, 30), nnF = c(10, 20, 30))
  looks_nnrE <- looks$nnrE
  looks_nnrF <- looks$nnrF
  orig_nnE <- c(10, 20, 30)
  orig_nnF <- c(10, 20, 30)
  orig_nnr <- unique(sort(c(orig_nnE, orig_nnF)))
  # input values from above arguments
  result <- h_get_decisionDist(
    nnr = c(10, 20, 30),
    nnrE = looks_nnrE,
    nnrF = looks_nnrF,
    truep = 0.4,
    parE = c(1, 1),
    tL = 0.8,
    tU = 0.6,
    deltaF = 0.1,
    deltaE = 0.2,
    relativeDelta = FALSE,
    orig_nnr = orig_nnr
  )
  expect_list(result, any.missing = FALSE)
  expect_equal(result$all_sizes, 30)
  expect_equal(result$decision, NA)
})


test_that("h_get_decisionDist gives correct result when nnE <U+2260> nnF", {
  set.seed(1989)
  example_dist <- h_get_distance(nn = c(10, 20, 30))
  looks <- h_get_looks(dist = example_dist, nnE = c(10, 20, 30), nnF = c(10, 20))
  looks_nnrE <- looks$nnrE
  looks_nnrF <- looks$nnrF
  orig_nnE <- c(10, 20, 30)
  orig_nnF <- c(10, 20, 30)
  orig_nnr <- unique(sort(c(orig_nnE, orig_nnF)))
  # input values from above arguments
  result <- h_get_decisionDist(
    nnr = c(10, 20, 30),
    nnrE = looks_nnrE,
    nnrF = looks_nnrF,
    truep = 0.4,
    parE = c(1, 1),
    tL = 0.8,
    tU = 0.6,
    deltaF = 0.1,
    deltaE = 0.2,
    relativeDelta = FALSE,
    orig_nnr = orig_nnr
  )
  expect_list(result, any.missing = FALSE)
  expect_equal(result$decision, NA)
  expect_equal(result$all_sizes, 30)
})

# ocPostprobDist ----
test_that("the sum of Eff, Fut, Gray zone probabiliy is 1", {
  set.seed(1989)
  expect_warning(res1 <- ocPostprobDist(
    nnE = c(10, 20, 30),
    truep = 0.4,
    deltaE = 0.1,
    deltaF = -0.1,
    tL = 0.6,
    tU = 0.6,
    parE = c(1, 1),
    parS = c(5, 25),
    sim = 50,
    wiggle = FALSE
  ), "Advise to use sim >= 50000 to achieve convergence")
  results <- sum(res1$oc[5:7])
  expect_equal(results, 1)
})

test_that("the PrFutility increases with increase futility looks", {
  set.seed(1989)
  expect_warning(res_one_fut <- ocPostprobDist(
    nnE = c(10, 20, 30),
    truep = 0.4,
    deltaE = 0.1,
    deltaF = -0.1,
    tL = 0.6,
    tU = 0.6,
    parE = c(1, 1),
    parS = c(5, 25),
    sim = 50,
    wiggle = TRUE,
    nnF = 30
  ), "Advise to use sim >= 50000 to achieve convergence")
  res_one_fut$oc$PrFutility
  expect_warning(res_three_fut <- ocPostprobDist(
    nnE = c(10, 20, 30),
    truep = 0.4,
    deltaE = 0.1,
    deltaF = -0.1,
    tL = 0.6,
    tU = 0.6,
    parE = c(1, 1),
    parS = c(5, 25),
    sim = 50,
    wiggle = TRUE,
    nnF = c(10, 20, 30)
  ), "Advise to use sim >= 50000 to achieve convergence")
  res_three_fut$oc$PrFutility
  expect_true(res_three_fut$oc$PrFutility > res_one_fut$oc$PrFutility)
})

test_that("the PrEfficacy increases with increase Efficacy looks", {
  set.seed(1989)
  expect_warning(res_eff <- ocPostprobDist(
    nnE = 30,
    truep = 0.4,
    deltaE = 0.1,
    deltaF = 0.1,
    tL = 0.7,
    tU = 0.3,
    parE = c(1, 1),
    parS = c(5, 25),
    sim = 100,
    wiggle = TRUE,
    nnF = 10
  ), "Advise to use sim >= 50000 to achieve convergence")
  expect_warning(res_more_eff <- ocPostprobDist(
    nnE = c(10, 20, 30),
    truep = 0.4,
    deltaE = 0.1,
    deltaF = 0.1,
    tL = 0.7,
    tU = 0.3,
    parE = c(1, 1),
    parS = c(5, 25),
    sim = 50,
    wiggle = TRUE,
    nnF = 10
  ), "Advise to use sim >= 50000 to achieve convergence")
  expect_true(res_more_eff$oc$PrEfficacy > res_eff$oc$PrEfficacy)
})

test_that("two function calls that differ in parE does not give the same result.", {
  set.seed(1989)
  expect_warning(result_uniform_hard_coded <- ocPostprobDist(
    nnE = c(10, 20, 30),
    truep = 0.15,
    deltaE = 0.1,
    deltaF = 0.1,
    tL = 0.45,
    tU = 0.4,
    parE = c(1, 1), # PrGrayZone higher due to same, weak priors
    parS = c(1, 1),
    sim = 100,
    wiggle = FALSE,
    nnF = 30
  ), "Advise to use sim >= 50000 to achieve convergence")
  expect_warning(result_no_hard_code <- ocPostprobDist(
    nnE = c(10, 20, 30),
    truep = 0.15,
    deltaE = 0.1,
    deltaF = -0.1,
    tL = 0.45,
    tU = 0.4,
    parE = c(5, 10), # PrFutility lower due to stronger prior
    parS = c(1, 1),
    sim = 100,
    wiggle = FALSE,
    nnF = 30
  ), "Advise to use sim >= 50000 to achieve convergence")
  expect_true(result_no_hard_code$oc["PrFutility"] > result_uniform_hard_coded$oc["PrFutility"])
  expect_true(result_no_hard_code$oc["PrGrayZone"] < result_uniform_hard_coded$oc["PrGrayZone"])
})
