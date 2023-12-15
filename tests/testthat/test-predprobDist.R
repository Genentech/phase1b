# h_predprobdist_single_arm ----
test_that("h_predprobdist gives correct results", {
  result <- h_predprobdist_single_arm( # From Lee & Liu (2008) example
    x = 16,
    n = 23,
    Nmax = 40,
    delta = 0.1,
    relativeDelta = FALSE,
    parE = t(c(0.6, 0.4)),
    weights = 1,
    parS = t(c(7, 11)),
    weightsS = 1,
    thetaT = 0.9,
    mE = 17
  )
  expect_equal(result$result, 0.7081907, tolerance = 1e-4)
  expect_equal(sum(result$table$density), 1, tolerance = 1e-4)
  expect_true(all(result$posterior) <= 1)
})

test_that("h_predprobdist_single_arm gives higher predictive probability when thetaT is lower", {
  is_lower <- h_predprobdist_single_arm( # From Lee & Liu (2008) example
    x = 16,
    n = 23,
    Nmax = 40,
    delta = 0.1,
    relativeDelta = FALSE,
    parE = t(c(0.6, 0.4)),
    weights = 1,
    parS = t(c(7, 11)),
    weightsS = 1,
    thetaT = 0.9,
    mE = 17
  )
  is_higher <- h_predprobdist_single_arm( # From Lee & Liu (2008) example
    x = 16,
    n = 16,
    Nmax = 40,
    delta = 0.1,
    relativeDelta = FALSE,
    parE = t(c(0.6, 0.4)),
    weights = 1,
    parS = t(c(7, 11)),
    weightsS = 1,
    thetaT = 0.5,
    mE = 17
  )
  expect_true(is_higher$result > is_lower$result)
})

test_that("h_predprobdist gives correct list", {
  result <- h_predprobdist_single_arm( # From Lee & Liu (2008) example
    x = 16,
    n = 23,
    Nmax = 40,
    delta = 0.1,
    relativeDelta = FALSE,
    parE = t(c(0.6, 0.4)),
    weights = 1,
    parS = t(c(7, 11)),
    weightsS = 1,
    thetaT = 0.9,
    mE = 17
  )
  expected <- list(
    result = 0.708190673645917,
    table = structure(
      list(
        counts = 0:17,
        cumul_counts = c(
          16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
          26, 27, 28, 29, 30, 31, 32, 33
        ),
        density = c(
          1.88519627332867e-06,
          2.27351448005706e-05, 0.000142906624460729, 0.00062104280723588,
          0.00208840865570497, 0.005765730288637, 0.0135369319820173,
          0.0276295672473687, 0.0496995264510597, 0.0793901526426017,
          0.112910439313922, 0.142631776121115, 0.158735363747693,
          0.153165701861808, 0.124552328986526, 0.0810915163188868,
          0.0381323201737623, 0.00988166643612601
        ),
        posterior = c(
          0.273531410553188,
          0.337791887816968, 0.4060867759263, 0.476483339905221, 0.546948408163723,
          0.615503378263203, 0.68036482033612, 0.74005747797379, 0.793490884253671,
          0.839995719658816, 0.879320792671476, 0.91159559002206, 0.93726634490541,
          0.957015305250502, 0.971673319374667, 0.982135113639861,
          0.989284955003113, 0.993938086698407
        ),
        success = c(
          FALSE,
          FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
          FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE
        )
      ),
      class = "data.frame",
      row.names = c(
        NA,
        -18L
      )
    )
  )
  expect_equal(result, expected)
})

# h_predprobdist ----
test_that("h_predprobdist gives correct list", {
  result <- h_predprobdist(
    NmaxControl = 20,
    Nmax = 40,
    n = 23,
    nS = 10,
    x = 16,
    xS = 5,
    parE = rbind(c(1, 1), c(50, 10)),
    parS = rbind(c(1, 1), c(20, 40)),
    weights = c(2, 1),
    weightsS = c(2, 1),
    delta = 0.1,
    relativeDelta = FALSE,
    thetaT = 0.5
  )
  expect_equal(result$result, 0.9322923, tolerance = 1e-4)
  expect_identical(result$table, data.frame(counts = 0:17, cumul_counts = as.numeric(16:33)))

  expect_matrix(result$density, mode = "numeric", any.missing = FALSE)
  expect_identical(rownames(result$density), as.character(0:17))
  expect_identical(colnames(result$density), as.character(0:10))

  expect_matrix(result$posterior, mode = "numeric", any.missing = FALSE)
  expect_identical(rownames(result$posterior), as.character(0:17))
  expect_identical(colnames(result$posterior), as.character(0:10))

  expect_matrix(result$success, mode = "logical", any.missing = FALSE)
  expect_identical(rownames(result$success), as.character(0:17))
  expect_identical(colnames(result$success), as.character(0:10))
})

test_that("h_predprobdist gives higher predictive probability when thetaT is lower", {
  is_lower <- h_predprobdist(
    NmaxControl = 20,
    Nmax = 40,
    n = 23,
    nS = 10,
    x = 16,
    xS = 5,
    parE = rbind(c(1, 1), c(50, 10)),
    parS = rbind(c(1, 1), c(20, 40)),
    weights = c(2, 1),
    weightsS = c(2, 1),
    delta = 0.1,
    relativeDelta = FALSE,
    thetaT = 0.9
  )
  is_higher <- h_predprobdist(
    NmaxControl = 20,
    Nmax = 40,
    n = 23,
    nS = 10,
    x = 16,
    xS = 5,
    parE = rbind(c(1, 1), c(50, 10)),
    parS = rbind(c(1, 1), c(20, 40)),
    weights = c(2, 1),
    weightsS = c(2, 1),
    delta = 0.1,
    relativeDelta = FALSE,
    thetaT = 0.5
  )
  expect_true(is_higher$result > is_lower$result)
})

test_that("sum of joint density in h_predprobdist is 1 and predictive probabilities are less than 1", {
  result <- h_predprobdist(
    NmaxControl = 10,
    Nmax = 10,
    n = 5,
    nS = 5,
    x = 2,
    xS = 1,
    parE = rbind(c(1, 1)),
    parS = rbind(c(1, 1)),
    weights = 1,
    weightsS = 1,
    delta = 0.1,
    relativeDelta = FALSE,
    thetaT = 0.5
  )
  expect_equal(sum(result$density), 1, tolerance = 1e-4)
  expect_true(all(result$posterior < 1))
})

## predprobDist ----
test_that("predprobDist gives the correct predictive probability in a single-arm study", {
  result <- predprobDist(
    NmaxControl = 10,
    Nmax = 10,
    n = 5,
    nS = 5,
    x = 2,
    xS = 1,
    parE = rbind(c(1, 1)),
    parS = rbind(c(1, 1)),
    weights = 1,
    weightsS = 1,
    delta = 0.1,
    relativeDelta = FALSE,
    thetaT = 0.5
  )
  expect_equal(result$result, 0.5513671, tolerance = 1e-4)
  expect_true(all(result$posterior < 1))
  expect_equal(sum(result$density), 1, tolerance = 1e-4)
})

test_that("predprobDist gives the correct results in a two-arm study", {
  result <- predprobDist(
    x = 16,
    n = 23,
    xS = 5,
    nS = 10,
    Nmax = 40,
    NmaxControl = 20,
    delta = 0.1,
    thetaT = 0.9,
    parE = rbind(c(1, 1), c(50, 10)),
    weights = c(2, 1),
    parS = rbind(c(1, 1), c(20, 40)),
    weightsS = c(2, 1)
  )
  expect_equal(result$result, 0.5989817, tolerance = 1e-4)
  expect_true(all(result$posterior <= 1))
  expect_equal(sum(result$density), 1, tolerance = 1e-4)
})

test_that("h_predprobdist_single_arm gives higher predictive probability when thetaT is lower in a single-arm trial", {
  is_lower <- predprobDist(
    x = 16,
    n = 23,
    xS = 5,
    nS = 10,
    Nmax = 40,
    NmaxControl = 20,
    delta = 0.1,
    thetaT = 0.9,
    parE = rbind(c(1, 1), c(50, 10)),
    weights = c(2, 1),
    parS = rbind(c(1, 1), c(20, 40)),
    weightsS = c(2, 1)
  )
  is_higher <- predprobDist(
    x = 16,
    n = 23,
    xS = 5,
    nS = 10,
    Nmax = 40,
    NmaxControl = 20,
    delta = 0.1,
    thetaT = 0.5,
    parE = rbind(c(1, 1), c(50, 10)),
    weights = c(2, 1),
    parS = rbind(c(1, 1), c(20, 40)),
    weightsS = c(2, 1)
  )
  expect_true(is_higher$result > is_lower$result)
})

test_that("h_predprobdist_single_arm gives higher predictive probability when thetaT is lower in a two-arm trial", {
  is_lower <- predprobDist(
    x = 16,
    n = 23,
    Nmax = 40,
    delta = 0.1,
    thetaT = 0.9,
    parE = rbind(c(1, 1), c(50, 10)),
    weights = c(2, 1),
  )
  is_higher <- predprobDist(
    x = 16,
    n = 23,
    Nmax = 40,
    delta = 0.1,
    thetaT = 0.5,
    parE = rbind(c(1, 1), c(50, 10)),
    weights = c(2, 1),
  )
  expect_true(is_higher$result > is_lower$result)
})
