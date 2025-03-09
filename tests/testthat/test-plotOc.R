# h_get_dataframe_oc ----
test_that("h_get_dataframe_oc gives correct results for `ocPostprob` for wiggle = TRUE", {
  set.seed(2025)
  res2 <- ocPostprob(
    nnE = c(10, 20, 30), truep = 0.40, p0 = 0.20, p1 = 0.30, tL = 0.60, tU = 0.80, parE = c(1, 1),
    sim = 100, wiggle = TRUE, nnF = c(10, 20, 30)
  )
  result <- h_get_dataframe_oc(res2$Decision, sample_size = res2$SampleSize, res2$Looks)
  expected <- dplyr::tibble(
    decision = structure(c(1L, 1L, 1L, 2L, 2L, 2L, NA, NA, NA),
      levels = c("TRUE", "FALSE"),
      class = "factor"
    ),
    look = structure(c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L),
      levels = c("10", "20", "30"),
      class = "factor"
    ),
    prop = c(0.44, 0.22, 0.11, 0.03, 0.00, 0.00, 0.00, 0.00, 0.20)
  )
  expect_identical(result, expected)
})

test_that("h_get_dataframe_oc gives correct results for `ocPredprob` when decision1 = FALSE", {
  set.seed(2025)
  res5 <- ocPredprob(
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
  )
  result <- h_get_dataframe_oc(res5$Decision, sample_size = res5$SampleSize, res5$Looks)
  expected <- dplyr::tibble(
    decision = structure(c(1L, 1L, 2L, 2L),
      levels = c("TRUE", "FALSE"),
      class = "factor"
    ),
    look = structure(c(1L, 2L, 1L, 2L),
      levels = c("10", "20"),
      class = "factor"
    ),
    prop = c(0.9, 0.1, 0, 0)
  )
  expect_identical(result, expected)
})

test_that("h_get_dataframe_oc gives correct results for `ocPredprobDist` when relativeDelta = FALSE", {
  set.seed(2025)
  res7 <- ocPredprobDist(
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
  )
  result <- h_get_dataframe_oc(res7$Decision, sample_size = res7$SampleSize, res7$Looks)
  expected <- dplyr::tibble(
    decision = structure(c(1L, 1L, 2L, 2L),
      levels = c("TRUE", "FALSE"),
      class = "factor"
    ),
    look = structure(c(1L, 2L, 1L, 2L),
      levels = c("20", "30"),
      class = "factor"
    ),
    prop = c(0.56, 0.36, 0.02, 0.06)
  )
  expect_identical(result, expected)
})

test_that("h_get_dataframe_oc gives correct results for `ocPredprobDist`when relativeDelta = TRUE", {
  set.seed(2025)
  res8 <- ocPredprobDist(
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
  )
  result <- h_get_dataframe_oc(res8$Decision, sample_size = res8$SampleSize, res8$Looks)
  expected <- dplyr::tibble(
    decision = structure(c(1L, 1L, 2L, 2L),
      levels = c("TRUE", "FALSE"),
      class = "factor"
    ),
    look = structure(c(1L, 2L, 1L, 2L),
      levels = c("20", "30"),
      class = "factor"
    ),
    prop = c(0.04, 0.02, 0.8, 0.14)
  )
  expect_identical(result, expected)
})

test_that("h_get_dataframe_oc gives correct results for `ocRctPostprobDist` when relativeDelta = FALSE", {
  set.seed(2025)
  res11 <- ocRctPostprobDist(
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
  )
  result <- h_get_dataframe_oc(res11$Decision, sample_size = res11$SampleSize, res11$Looks)
  expected <- dplyr::tibble(
    decision = structure(c(1L, 1L, 1L, 2L, 2L, 2L, NA, NA, NA),
      levels = c("TRUE", "FALSE"),
      class = "factor"
    ),
    look = structure(c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L),
      levels = c("10", "20", "30"),
      class = "factor"
    ),
    prop = c(0.04, 0.02, 0.04, 0.72, 0.02, 0.04, 0, 0, 0.12)
  )
  expect_identical(result, expected)
})

test_that("h_get_dataframe_oc gives correct results for `ocRctPredprobDist` relativeDelta = TRUE", {
  set.seed(2025)
  res11 <- ocRctPredprobDist(
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
  )
  result <- h_get_dataframe_oc(res11$Decision, sample_size = res11$SampleSize, res11$Looks)
  expected <- dplyr::tibble(
    decision = structure(c(1L, 1L, 1L, 2L, 2L, 2L, NA, NA, NA),
      levels = c("TRUE", "FALSE"),
      class = "factor"
    ),
    look = structure(c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L),
      levels = c("10", "20", "30"),
      class = "factor"
    ),
    prop = c(0.02, 0, 0, 0.22, 0.22, 0.48, 0, 0, 0.06)
  )
  expect_identical(result, expected)
})
