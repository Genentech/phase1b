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
