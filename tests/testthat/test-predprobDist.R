# h_predprobdist_single_arm ----
test_that("predprobdist gives correct predictive probability", {
  result <- h_predprobdist_single_arm( # From Lee & Liu (2008) example
    x = 16,
    Nmax = 40,
    delta = 0.1,
    relativeDelta = FALSE,
    parE = c(0.6, 0.4),
    weights = 1,
    parS = c(7, 11),
    weightsS = 1,
    thetaT = 0.9,
    density = dbetabinomMix(x = 0:17, m = 17, par = t(c(0.6, 0.4)), weights = 1),
    mE = 17
  )
  expect_equal(result$result, 0.5426927, tolerance = 1e-4)
})

test_that("predictive probability is higher when thetaT is lower", {
  is_lower <- h_predprobdist_single_arm( # From Lee & Liu (2008) example
    x = 16,
    Nmax = 40,
    delta = 0.1,
    relativeDelta = FALSE,
    parE = c(0.6, 0.4),
    weights = 1,
    parS = c(7, 11),
    weightsS = 1,
    thetaT = 0.9,
    density = dbetabinomMix(x = 0:17, m = 17, par = t(c(0.6, 0.4)), weights = 1),
    mE = 17
  )
  is_higher <- h_predprobdist_single_arm( # From Lee & Liu (2008) example
    x = 16,
    Nmax = 40,
    delta = 0.1,
    relativeDelta = FALSE,
    parE = c(0.6, 0.4),
    weights = 1,
    parS = c(7, 11),
    weightsS = 1,
    thetaT = 0.5,
    density = dbetabinomMix(x = 0:17, m = 17, par = t(c(0.6, 0.4)), weights = 1),
    mE = 17
  )
  expect_true(is_higher$result > is_lower$result, )
})

test_that("predprobdist gives correct list", {
  result <- h_predprobdist_single_arm( # From Lee & Liu (2008) example
    x = 16,
    Nmax = 40,
    delta = 0.1,
    relativeDelta = FALSE,
    parE = c(0.6, 0.4),
    weights = 1,
    parS = c(7, 11),
    weightsS = 1,
    thetaT = 0.9,
    density = dbetabinomMix(x = 0:17, m = 17, par = t(c(0.6, 0.4)), weights = 1),
    mE = 17
  )
  expected <- list(
    result = 0.54269272350537,
    table = data.frame(
      counts = 0:17,
      cumul_counts = c(
        16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
        26, 27, 28, 29, 30, 31, 32, 33
      ),
      density = c(
        0.081783685071388,
        0.0508654626663511, 0.0422777871512528, 0.0381674467337698,
        0.0358887931974255, 0.0346153198904199, 0.0340080335765529,
        0.0339146049128812, 0.0342753985821671, 0.0350914795007901,
        0.0364192652116307, 0.0383850778224858, 0.0412284169204477,
        0.0454089207340597, 0.0518959094103539, 0.0631400231159307,
        0.0879450321971892, 0.214689343304903
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
    )
  )
  expect_equal(result, expected)
})

# h_predprobdist ----
test_that("predprobdist gives correct predictive probability", {
  mE <- 17
  parE <- rbind(c(1, 1), c(50, 10))
  result <- h_predprobdist(
    NmaxControl = 20,
    Nmax = 40,
    nS = 10,
    xS = 5,
    parS = rbind(c(1, 1), c(20, 40)),
    weightsS = c(2, 1),
    x = 16,
    density_y = dbetabinomMix(x = 0:17, m = 23, par = rbind(c(1, 1), c(50, 10)), weights = c(0.5, 0.5)),
    delta = 0.1, relativeDelta = FALSE, mE = mE
  )
  expect_equal(result$result, 0.5426927, tolerance = 1e-4)
})


test_that("predictive probability is higher when thetaT is lower", {
  is_lower <- h_predprobdist_single_arm( # From Lee & Liu (2008) example
    x = 16,
    Nmax = 40,
    delta = 0.1,
    relativeDelta = FALSE,
    parE = c(0.6, 0.4),
    weights = 1,
    parS = c(7, 11),
    weightsS = 1,
    thetaT = 0.9,
    density = dbetabinomMix(x = 0:17, m = 17, par = t(c(0.6, 0.4)), weights = 1),
    mE = 17
  )
  is_higher <- h_predprobdist_single_arm( # From Lee & Liu (2008) example
    x = 16,
    Nmax = 40,
    delta = 0.1,
    relativeDelta = FALSE,
    parE = c(0.6, 0.4),
    weights = 1,
    parS = c(7, 11),
    weightsS = 1,
    thetaT = 0.5,
    density = dbetabinomMix(x = 0:17, m = 17, par = t(c(0.6, 0.4)), weights = 1),
    mE = 17
  )
  expect_true(is_higher$result > is_lower$result, )
})
