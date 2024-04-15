# h_get_decisionDist_rct ----
test_that("h_get_decisionDist_rct gives correct result and list when relativeDelta = TRUE", {
  set.seed(1989)
  nn <- c(10, 20, 30)
  example_dist <- h_get_distance(nn = nn)
  looks <- h_get_looks(dist = example_dist, nnE = c(10, 20, 30), nnF = c(10, 20, 30))
  looks_nnrE <- looks$nnrE
  looks_nnrF <- looks$nnrF
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
    Nmax = Nmax
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
    Nmax = Nmax
  )
  expect_flag(result$decision, TRUE)
  expect_equal(result$all_sizes, 30)
  expect_equal(result$nActive, 15)
  expect_equal(result$nControl, 15)
  expect_list(result)
})

# h_get_oc_rct ----
test_that("the probability results of h_get_oc_rct are less than 1 when relativeDelta = TRUE", {
  set.seed(2000)
  input <- list(
    nn = c(10, 20, 30),
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
    randRatio = 1,
    ns = 10000,
    nr = FALSE,
    d = NULL,
    nnF = c(10, 20, 30),
    truep = 0.4,
    sim = 5,
    Nmax = 15,
    NmaxControl = 15,
    decision <- rep(NA, 5)
  )
  result <- ocRctPostprobDist(
    nn = input$nn,
    pE = input$pE,
    pS = input$pS,
    deltaE = input$deltaE,
    deltaF = input$deltaF,
    relativeDelta = FALSE,
    tL = input$tL,
    tU = input$tU,
    parE = input$parE,
    parS = input$parS,
    randRatio = 1,
    ns = 100,
    nr = input$nr,
    d = input$d,
    nnF = input$nnF
  )
  oc <- h_get_oc_rct(
    all_sizes = result$SampleSize,
    Nmax = max(input$nn),
    nActive = result$SampleSizeActive,
    nControl = result$SampleSizeControl,
    decision = result$Decision
  )
  expect_true(oc$PrStopEarly < 1)
  expect_true(oc$PrFutility < 1)
  expect_true(oc$PrEarlyEff < 1)
  expect_true(oc$PrEfficacy < 1)
})

test_that("the ExpectedN of h_get_oc_rct is within range based on vector of looks when relativeDelta = TRUE", {
  set.seed(2000)
  input <- list(
    nn = c(10, 20, 30),
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
    randRatio = 1,
    ns = 10000,
    nr = FALSE,
    d = NULL,
    nnF = c(10, 20, 30),
    truep = 0.4,
    sim = 5,
    Nmax = 15,
    NmaxControl = 15,
    decision <- rep(NA, 5)
  )
  result <- ocRctPostprobDist(
    nn = input$nn,
    pE = input$pE,
    pS = input$pS,
    deltaE = input$deltaE,
    deltaF = input$deltaF,
    relativeDelta = TRUE,
    tL = input$tL,
    tU = input$tU,
    parE = input$parE,
    parS = input$parS,
    randRatio = 1,
    ns = 100,
    nr = input$nr,
    d = input$d,
    nnF = input$nnF
  )
  oc <- h_get_oc_rct(
    all_sizes = result$SampleSize,
    Nmax = max(input$nn),
    nActive = result$SampleSizeActive,
    nControl = result$SampleSizeControl,
    decision = result$Decision
  )
  expect_number(oc$ExpectedN, lower = min(oc$all_sizes), upper = max(oc$all_sizes))
})

test_that("the probability results of h_get_oc_rct are less than 1 when relativeDelta = FALSE", {
  set.seed(2000)
  input <- list(
    nn = c(10, 20, 30),
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
    randRatio = 1,
    ns = 10000,
    nr = FALSE,
    d = NULL,
    nnF = c(10, 20, 30),
    truep = 0.4,
    sim = 5,
    Nmax = 15,
    NmaxControl = 15,
    decision <- rep(NA, 5)
  )
  result <- ocRctPostprobDist(
    nn = input$nn,
    pE = input$pE,
    pS = input$pS,
    deltaE = input$deltaE,
    deltaF = input$deltaF,
    relativeDelta = FALSE,
    tL = input$tL,
    tU = input$tU,
    parE = input$parE,
    parS = input$parS,
    randRatio = 1,
    ns = 100,
    nr = input$nr,
    d = input$d,
    nnF = input$nnF
  )
  oc <- h_get_oc_rct(
    all_sizes = result$SampleSize,
    Nmax = max(input$nn),
    nActive = result$SampleSizeActive,
    nControl = result$SampleSizeControl,
    decision = result$Decision
  )
  expect_true(oc$PrStopEarly < 1)
  expect_true(oc$PrFutility < 1)
  expect_true(oc$PrEarlyEff < 1)
  expect_true(oc$PrEfficacy < 1)
})

test_that("the ExpectedN of h_get_oc_rct is within range based on vector of looks when relativeDelta = FALSE", {
  set.seed(2000)
  input <- list(
    nn = c(10, 20, 30),
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
    randRatio = 1,
    ns = 10000,
    nr = FALSE,
    d = NULL,
    nnF = c(10, 20, 30),
    truep = 0.4,
    sim = 5,
    Nmax = 15,
    NmaxControl = 15,
    decision <- rep(NA, 5)
  )
  result <- ocRctPostprobDist(
    nn = input$nn,
    pE = input$pE,
    pS = input$pS,
    deltaE = input$deltaE,
    deltaF = input$deltaF,
    relativeDelta = FALSE,
    tL = input$tL,
    tU = input$tU,
    parE = input$parE,
    parS = input$parS,
    randRatio = 1,
    ns = 100,
    nr = input$nr,
    d = input$d,
    nnF = input$nnF
  )
  oc <- h_get_oc_rct(
    all_sizes = result$SampleSize,
    Nmax = max(input$nn),
    nActive = result$SampleSizeActive,
    nControl = result$SampleSizeControl,
    decision = result$Decision
  )
  expect_number(oc$ExpectedN, lower = min(result$SampleSize), upper = max(result$SampleSize))
})
