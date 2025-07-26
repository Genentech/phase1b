# h_get_decisionDist_rct ----
test_that("h_get_decisionDist_rct gives correct result and list when relativeDelta = TRUE", {
  set.seed(1989)
  nn <- c(10, 20, 30)
  example_dist <- h_get_distance(nn = nn)
  looks <- h_get_looks(dist = example_dist, nnE = c(10, 20, 30), nnF = c(10, 20, 30))
  looks_nnrE <- looks$nnrE
  looks_nnrF <- looks$nnrF
  orig_nnE <- c(10, 20, 30)
  orig_nnF <- c(10, 20, 30)
  orig_nnr <- unique(sort(c(orig_nnE, orig_nnF)))
  Nmax <- max(nn)
  result <- h_get_decisionDist_rct(
    nnr = nn,
    nnrE = looks$nnrE,
    nnrF = looks$nnrF,
    pE = 0.4,
    pS = 0.3,
    parE = c(1, 1),
    parS = c(1, 1),
    tL = 0.8,
    tU = 0.8,
    deltaE = 0.2,
    deltaF = 0.1,
    relativeDelta = TRUE,
    Nmax = Nmax,
    orig_nnr = orig_nnr
  )
  expect_flag(result$decision, TRUE)
  expect_equal(result$all_sizes, 30)
  expect_equal(result$nActive, 15)
  expect_equal(result$nControl, 15)
  expect_list(result)
})

test_that("h_get_decisionDist_rct gives correct result and list when relativeDelta = FALSE", {
  set.seed(1989)
  nn <- c(10, 20, 30)
  example_dist <- h_get_distance(nn = nn)
  looks <- h_get_looks(dist = example_dist, nnE = c(10, 20, 30), nnF = c(10, 20, 30))
  looks_nnrE <- looks$nnrE
  looks_nnrF <- looks$nnrF
  orig_nnE <- c(10, 20, 30)
  orig_nnF <- c(10, 20, 30)
  orig_nnr <- unique(sort(c(orig_nnE, orig_nnF)))
  Nmax <- max(nn)
  result <- h_get_decisionDist_rct(
    nnr = nn,
    nnrE = looks$nnrE,
    nnrF = looks$nnrF,
    pE = 0.4,
    pS = 0.3,
    parE = c(1, 1),
    parS = c(1, 1),
    tL = 0.8,
    tU = 0.8,
    deltaE = 0.2,
    deltaF = 0.1,
    relativeDelta = FALSE,
    Nmax = Nmax,
    orig_nnr = orig_nnr
  )
  expect_flag(result$decision, TRUE)
  expect_equal(result$all_sizes, 30)
  expect_equal(result$nActive, 15)
  expect_equal(result$nControl, 15)
  expect_list(result)
})

# h_get_oc_rct ----
test_that("the ExpectedN is within range based on vector of looks", {
  all_sizes <- c(10, 20, 30)
  oc <- h_get_oc_rct(
    all_sizes = all_sizes,
    Nmax = 30,
    nActive = c(4, 10, 15),
    nControl = c(6, 10, 15),
    decision = c(TRUE, TRUE, FALSE)
  )
  expect_equal(oc$PrGrayZone, 0, tolerance = 1e-7)
  expect_equal(oc$PrStopEarly, 0.6666667, tolerance = 1e-7)
  expect_equal(oc$PrFutility, 0.3333333, tolerance = 1e-6)
  expect_equal(oc$PrEarlyEff, 0.6666667, tolerance = 1e-7)
  expect_equal(oc$PrEfficacy, 0.6666667, tolerance = 1e-7)
  expect_true(oc$ExpectedN == 20)
})

# ocRctPostprobDist ----
test_that("the sum of Eff, Fut, Gray zone probabiliy is 1", {
  set.seed(2000)
  input <- list(
    nnE = c(10, 20, 30),
    pE = 0.4,
    pS = 0.3,
    deltaE = 0.2,
    deltaF = 0.1,
    relativeDelta = FALSE,
    tL = 0.8,
    tU = 0.8,
    parE = c(a = 1, b = 1),
    parS = c(a = 1, b = 1),
    nnF = c(10, 20, 30),
    sim = 5,
    Nmax = 15
  )
  expect_warning(results <- ocRctPostprobDist(
    nnE = input$nnE,
    pE = input$pE,
    pS = input$pS,
    deltaE = input$deltaE,
    deltaF = input$deltaF,
    relativeDelta = TRUE,
    tL = input$tL,
    tU = input$tU,
    parE = input$parE,
    parS = c(a = 1, b = 1),
    randRatio = 1,
    sim = 50,
    wiggle = FALSE,
    nnF = input$nnF
  ), "Advise to use sim >= 50000 to achieve convergence")
  results <- sum(results$oc[5:7])
  expect_equal(results, 1)
})

test_that("ocRctPostprobDist gives higher PrFutility with decreased pE", {
  set.seed(1989)
  input <- list(
    nnE = c(10, 20, 30),
    pE = 0.4,
    pS = 0.3,
    deltaE = 0.2,
    deltaF = 0.1,
    relativeDelta = FALSE,
    tL = 0.8,
    tU = 0.8,
    parE = c(a = 1, b = 1),
    parS = c(a = 1, b = 1),
    nnF = c(10, 20, 30),
    sim = 5,
    Nmax = 15
  )
  expect_warning(res_low_truep <- ocRctPostprobDist(
    nnE = input$nnE,
    pE = input$pE - 0.1,
    pS = input$pS,
    deltaE = input$deltaE,
    deltaF = input$deltaF,
    relativeDelta = TRUE,
    tL = input$tL,
    tU = input$tU,
    parE = input$parE,
    parS = c(a = 1, b = 1),
    randRatio = 1,
    sim = 50,
    wiggle = FALSE,
    nnF = c(10, 20, 30)
  ), "Advise to use sim >= 50000 to achieve convergence")
  expect_warning(res <- ocRctPostprobDist(
    nnE = input$nnE,
    pE = input$pE,
    pS = input$pS,
    deltaE = input$deltaE,
    deltaF = input$deltaF,
    relativeDelta = TRUE,
    tL = input$tL,
    tU = input$tU,
    parE = input$parE,
    parS = c(a = 1, b = 1),
    randRatio = 1,
    sim = 50,
    wiggle = FALSE,
    nnF = c(10, 20, 30)
  ), "Advise to use sim >= 50000 to achieve convergence")
  expect_true(res$oc$PrFutility < res_low_truep$oc$PrFutility)
})

test_that("ocRctPostprobDist gives higher PrEfficacy with increased pE", {
  set.seed(1989)
  input <- list(
    nnE = c(10, 20, 30),
    pE = 0.4,
    pS = 0.3,
    deltaE = 0.2,
    deltaF = 0.1,
    relativeDelta = FALSE,
    tL = 0.8,
    tU = 0.8,
    parE = c(a = 1, b = 1),
    parS = c(a = 1, b = 1),
    nnF = c(10, 20, 30),
    sim = 5,
    Nmax = 15
  )
  expect_warning(res_eff <- ocRctPostprobDist(
    nnE = input$nnE,
    pE = input$pE,
    pS = input$pS,
    deltaE = input$deltaE,
    deltaF = input$deltaF,
    relativeDelta = TRUE,
    tL = input$tL,
    tU = input$tU,
    parE = input$parE,
    parS = c(a = 1, b = 1),
    randRatio = 1,
    sim = 50,
    wiggle = FALSE,
    nnF = 10
  ), "Advise to use sim >= 50000 to achieve convergence")
  expect_warning(res_high_truep <- ocRctPostprobDist(
    nnE = input$nnE,
    pE = 0.7,
    pS = input$pS,
    deltaE = input$deltaE,
    deltaF = input$deltaF,
    relativeDelta = TRUE,
    tL = input$tL,
    tU = input$tU,
    parE = input$parE,
    parS = c(a = 1, b = 1),
    randRatio = 1,
    sim = 50,
    wiggle = FALSE,
    nnF = 10
  ), "Advise to use sim >= 50000 to achieve convergence")
  expect_true(res_high_truep$oc$PrEfficacy > res_eff$oc$PrEfficacy)
})

test_that("two function calls that differ in parE does not give the same result.", {
  set.seed(1989)
  input <- list(
    nnE = 30,
    pE = 0.4,
    pS = 0.2,
    deltaE = 0.1,
    deltaF = 0.1,
    relativeDelta = FALSE,
    tL = 0.8,
    tU = 0.8,
    parE = c(a = 4, b = 10),
    parS = c(a = 1, b = 1),
    nnF = 30,
    sim = 5,
    Nmax = 15
  )
  expect_warning(result_uniform_hard_coded <- ocRctPostprobDist(
    nnE = input$nnE,
    pE = input$pE,
    pS = input$pS,
    deltaE = input$deltaE,
    deltaF = input$deltaF,
    relativeDelta = TRUE,
    tL = input$tL,
    tU = input$tU,
    parE = c(1, 1),
    parS = input$parS,
    randRatio = 1,
    sim = 50,
    wiggle = FALSE,
    nnF = 10
  ), "Advise to use sim >= 50000 to achieve convergence")
  result_uniform_hard_coded$oc
  expect_warning(result_no_hard_code <- ocRctPostprobDist(
    nnE = input$nnE,
    pE = input$pE,
    pS = input$pS,
    deltaE = input$deltaE,
    deltaF = input$deltaF,
    relativeDelta = TRUE,
    tL = input$tL,
    tU = input$tU,
    parE = input$parE, # stronger prior, higher difference needed to pass Go
    parS = input$parS,
    randRatio = 1,
    sim = 50,
    wiggle = FALSE,
    nnF = 10
  ), "Advise to use sim >= 50000 to achieve convergence")
  result_no_hard_code$oc
  expect_true(result_no_hard_code$oc["PrEfficacy"] < result_uniform_hard_coded$oc["PrEfficacy"])
  expect_true(result_no_hard_code$oc["PrGrayZone"] > result_uniform_hard_coded$oc["PrGrayZone"])
}
)
