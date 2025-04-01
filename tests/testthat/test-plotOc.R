# h_get_dataframe_oc ----
test_that("h_get_dataframe_oc gives correct results for `ocPostprob` for wiggle = TRUE", {
  set.seed(2025)
  expect_warning(res2 <- ocPostprob(
    nnE = c(10, 20, 30),
    truep = 0.40,
    p0 = 0.20,
    p1 = 0.30,
    tL = 0.60,
    tU = 0.80,
    parE = c(1, 1),
    sim = 100,
    wiggle = TRUE,
    nnF = c(10, 20, 30)
  ), "Advise to use sim >= 50000 to achieve convergence")
  result <- h_get_dataframe_oc(decision = res2$Decision, all_sizes = res2$SampleSize, all_looks = res2$Looks)
  expected <- dplyr::tibble(
    decision = structure(c(1L, 1L, 1L, 2L, 2L, 2L, NA, NA, NA),
      levels = c("TRUE", "FALSE"),
      class = "factor"
    ),
    all_looks = structure(c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L),
      levels = c("10", "20", "30"),
      class = "factor"
    ),
    prop = structure(c(0.44, 0.22, 0.11, 0.03, 0.00, 0.00, 0.00, 0.00, 0.20),
    look = c(1L,2L, 3L, 1L, NA, NA, NA, NA, 3L),
    levels = c("10", "20", "30"),
    class = "factor")
  )
  expected <- dplyr::tibble(
    decision = structure(c(1L, 1L, 1L, 2L, 2L, 2L, NA, NA, NA),
                         levels = c("TRUE", "FALSE"),
                         class = "factor"),
    all_looks = c(10, 20, 30, 10, 20, 30, 10, 20, 30),
    prop = c(0.44, 0.22, 0.11, 0.03, 0, 0, 0, 0, 0.2),
    look = structure(c(1L,2L, 3L, 1L, NA, NA, NA, NA, 3L),
                     levels = c("10", "20", "30"),
                     class = "factor"))
expect_identical(result, expected)
})

test_that("h_get_dataframe_oc gives correct results for `ocPredprob` when decision1 = FALSE", {
  set.seed(2025)
  expect_warning(res5 <- ocPredprob(
    nnE = c(10, 20),
    truep = 0.6,
    p0 = 0.25,
    p1 = 0.25,
    tT = 0.6,
    tF = 0.6,
    phiU = 0.8,
    phiFu = 0.8,
    parE = c(1, 1),
    sim = 50,
    wiggle = TRUE,
    nnF = c(10, 20),
    decision1 = FALSE
  ))
  result <- h_get_dataframe_oc(decision = res5$Decision, all_sizes = res5$SampleSize, all_looks = res5$Looks)
  expected <- dplyr::tibble(decision = structure(c(1L, 1L, 2L, 2L),
                                                 levels = c("TRUE", "FALSE"),
                                                 class = "factor"),
                            all_looks = c(10, 20, 10, 20),
                            prop = c(0.94, 0.06, 0, 0),
                            look = structure(c(1L, 2L, NA, NA),
                                             levels = c("10", "20"),
                                             class = "factor"))
expect_identical(result, expected)
})

test_that("h_get_dataframe_oc gives correct results for `ocPredprobDist` when relativeDelta = FALSE", {
  set.seed(2025)
  expect_warning(res7 <- ocPredprobDist(
    nnE = c(10, 20, 30),
    truep = 0.40,
    deltaE = 0.10,
    deltaF = 0.10,
    relativeDelta = FALSE,
    tT = 0.6,
    phiU = 0.80,
    phiL = 0.20,
    parE = c(1, 1),
    parS = c(5, 25),
    weights = 1,
    weightsS = 1,
    sim = 50,
    wiggle = TRUE,
    decision1 = TRUE
  ))
  result <- h_get_dataframe_oc(decision = res7$Decision, all_sizes = res7$SampleSize, all_looks = res7$Looks)
  expected <- dplyr::tibble(
    decision = structure(c(1L, 1L, 2L, 2L),
      levels = c("TRUE", "FALSE"),
      class = "factor"
    ),
    all_looks = structure(c(1L, 2L, 1L, 2L),
      levels = c("20", "30"),
      class = "factor"
    ),
    prop = c(0.56, 0.36, 0.02, 0.06),
    look = structure(c(1L, 2L, 1L, 2L), levels = c("20", "30"),
                     class = "factor")
  )
  expect_identical(result, expected)
})

test_that("h_get_dataframe_oc gives correct results for `ocPredprobDist`when relativeDelta = TRUE", {
  set.seed(2025)
  expect_warning(res8 <- ocPredprobDist(
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
    decision1 = TRUE
  ))
  result <- h_get_dataframe_oc(res8$Decision, all_sizes = res8$SampleSize, res8$Looks)
  expected <- dplyr::tibble(
    decision = structure(c(1L, 1L, 2L, 2L),
    levels = c("TRUE", "FALSE"),
    class = "factor"),
  all_looks = c(20, 30, 20, 30),
  prop = c(0.04, 0.02, 0.84, 0.1),
  look = structure(c(1L, 2L, 1L, 2L),
                   levels = c("20","30"),
                   class = "factor"))
expect_identical(result, expected)
})

test_that("h_get_dataframe_oc gives correct results for `ocRctPostprobDist` when relativeDelta = FALSE", {
  set.seed(2025)
  expect_warning(res9 <- ocRctPostprobDist(
    nnE = c(10, 20, 30),
    pE = 0.4,
    pS = 0.3,
    deltaE = 0.15,
    deltaF = 0.05,
    relativeDelta = FALSE,
    tL = 0.2,
    tU = 0.8,
    parE = c(a = 1, b = 1),
    parS = c(a = 1, b = 1),
    randRatio = 1,
    sim = 50,
    wiggle = TRUE,
    nnF = c(10, 20, 30)
  ))
  result <- h_get_dataframe_oc(decision = res9$Decision, all_sizes = res9$SampleSize, all_looks = res9$Looks)
  expected <- dplyr::tibble(decision = structure(
    c(1L, 1L, 1L, 2L, 2L, 2L, NA, NA, NA),
    levels = c("TRUE", "FALSE"), class = "factor"),
    all_looks = c(10, 20, 30, 10, 20, 30, 10, 20, 30),
    prop = c(0.08, 0.04, 0.02, 0.68, 0.08, 0, 0, 0, 0.1),
    look = structure(c(1L,2L, 3L, 1L, 2L, NA, NA, NA, 3L),
                     levels = c("10", "20", "30"), class = "factor"))
})

test_that("h_get_dataframe_oc gives correct results for `ocRctPredprobDist` relativeDelta = TRUE", {
  set.seed(2025)
  expect_warning(res11 <- ocRctPredprobDist(
    nnE = c(10, 20, 30),
    pE = 0.3,
    pS = 0.3,
    deltaE = 0.2,
    deltaF = 0.1,
    phiU = 0.8,
    phiFu = 0.2,
    relativeDelta = TRUE,
    tT = 0.6,
    tF = 0.4,
    parE = c(a = 1, b = 1),
    parS = c(a = 1, b = 1),
    weights = 1,
    weightsS = 1,
    randRatio = 1,
    sim = 50,
    wiggle = TRUE,
    nnF = c(10, 20, 30),
    decision1 = FALSE
  ))
  result <- h_get_dataframe_oc(res11$Decision, all_sizes = res11$SampleSize, res11$Looks)
  expected <- dplyr::tibble(
    decision = factor(c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA, NA, NA)),
    all_looks = c(10, 20, 30, 10, 20, 30, 10, 20, 30),
    prop = c(0.02, 0, 0, 0.22, 0.22, 0.48, 0, 0, 0.06),
    look = factor(c("10", NA, NA, "10", "20", "30", NA, NA, "30"))
  )
  expect_identical(result, expected)
})

# plotOc ----
test_that("plotOc gives expected results for `ocPostprob` and `ocPredprob`", {
  set.seed(2025)
  expect_warning(res2 <- ocPostprob(
    nnE = c(10, 20, 30),
    truep = 0.40,
    p0 = 0.20,
    p1 = 0.30,
    tL = 0.60,
    tU = 0.80,
    parE = c(1, 1),
    sim = 100,
    wiggle = TRUE,
    nnF = c(10, 20, 30)
  ))
  expect_warning(res5 <- ocPredprob(
    nnE = c(10, 20),
    truep = 0.6,
    p0 = 0.25,
    p1 = 0.25,
    tT = 0.6,
    tF = 0.6,
    phiU = 0.8,
    phiFu = 0.8,
    parE = c(1, 1),
    sim = 50,
    wiggle = TRUE,
    nnF = c(10, 20),
    decision1 = FALSE
  ))
  result1 <- plotOc(
    decision = res2$Decision,
    all_sizes = res2$SampleSize,
    all_looks = res2$Looks,
    wiggle_status = res2$params$wiggle
  )
  result2 <- plotOc(
    decision = res5$Decision,
    all_sizes = res5$SampleSize,
    all_looks = res5$Looks,
    wiggle_status = res5$params$wiggle
  )
  vdiffr::expect_doppelganger(
    title = "plot of simulation result for single arm posterior probability",
    fig = result1
  )
  vdiffr::expect_doppelganger(
    title = "plot of simulation result for single arm posterior predictive probability",
    fig = result2
  )
})

# ggsave("tests/testthat/_snaps/plotOc/plot-of-simulation-result-for-single-arm-posterior-probability.svg", plot = result1)
# ggsave("tests/testthat/_snaps/plotOc/plot-of-simulation-result-for-single-arm-posterior-predictive-probability.svg", plot = result2 )

test_that("plotOc gives expected results for `ocPredprobDist` with different relativeDelta status", {
  set.seed(2025)
  expect_warning(res7 <- ocPredprobDist(
    nnE = c(10, 20, 30),
    truep = 0.40,
    deltaE = 0.10,
    deltaF = 0.10,
    relativeDelta = FALSE,
    tT = 0.6,
    phiU = 0.80,
    phiL = 0.20,
    parE = c(1, 1),
    parS = c(5, 25),
    weights = 1,
    weightsS = 1,
    sim = 50,
    wiggle = TRUE,
    decision1 = TRUE
  ))
  expect_warning(res8 <- ocPredprobDist(
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
    decision1 = TRUE
  ))
  result1 <- plotOc(
    decision = res7$Decision,
    all_sizes = res7$SampleSize,
    all_looks = res7$Looks,
    wiggle_status = res7$params$wiggle
  )
  result2 <- plotOc(
    decision = res8$Decision,
    all_sizes = res8$SampleSize,
    all_looks = res8$Looks,
    wiggle_status = res8$params$wiggle
  )
  vdiffr::expect_doppelganger(
    title = "Plot of simulation result without relativeDelta for posterior predictive probability",
    fig = result1
  )
  vdiffr::expect_doppelganger(
    title = "Plot of simulation result with relativeDelta for posterior predictive probability",
    fig = result2
  )
})

# ggsave("tests/testthat/_snaps/plotOc/plot-of-simulation-result-without-relativeDelta-for-posterior-probability.svg", plot = result1)
# ggsave("tests/testthat/_snaps/plotOc/plot-of-simulation-result-without-relativeDelta-for-posterior-predictive-probability.svg",  plot = result2)
