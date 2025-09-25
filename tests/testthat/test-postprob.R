# postprobBeta ----
test_that("postprobBeta gives the correct number result", {
  # Example from Lee & Liu (2006) A predictive probability design for phase II cancer clinical trials
  result <- postprobBeta(x = 16, n = 23, p = 0.60, a = 0.6, b = 0.4)
  expect_equal(result, 0.8359808, tolerance = 1e-5)
})

test_that("postprobBeta gives incrementally higher values with increase x support", {
  is_lower <- postprobBeta(x = 10, n = 23, p = 0.60, a = 0.6, b = 0.4)
  is_higher <- postprobBeta(x = 16, n = 23, p = 0.60, a = 0.6, b = 0.4)
  expect_true(is_lower < is_higher)
})

# postprob ---
test_that("postprob gives the correct number result", {
  # Example from Lee & Liu (2006) A predictive probability design for phase II cancer clinical trials
  result <- postprob(x = 16, n = 23, p = 0.60, par = c(0.6, 0.4))
  expect_equal(result, 0.8359808, tolerance = 1e-5)
})

test_that("postprob gives the correct number result", {
  # 2 component beta mixture prior, i.e., P_E ~ 0.5*beta(0.6,0.4) + 0.5*beta(1,1) and Pr(P_E > p | data) = 0.05559802
  result <- postprob(
    x = 10,
    n = 23,
    p = 0.60,
    par = rbind(
      c(0.6, 0.4),
      c(1, 1)
    )
  )
  expect_equal(result, 0.05559802, tolerance = 1e-5)
})

test_that("postprob gives incrementally higher values with increased x", {
  is_lower <- postprob(
    x = 10,
    n = 23,
    p = 0.60,
    par = rbind(
      c(0.6, 0.4),
      c(1, 1)
    )
  )
  is_higher <- postprob(
    x = 16,
    n = 23,
    p = 0.60,
    par = rbind(
      c(0.6, 0.4),
      c(1, 1)
    )
  )
  expect_true(is_lower < is_higher)
})

test_that("postprob works with vector x", {
  result <- postprob(x = 0:23, n = 23, p = 0.60, par = c(0.6, 0.4))
  expected <- c(
    1.12066620085448e-10,
    6.73786529927603e-09,
    1.45879637562279e-07,
    1.86374536434781e-06,
    1.64656040420248e-05,
    0.000108838231763851,
    0.000564103325535708,
    0.00236446983272442,
    0.00819197194809839,
    0.0238449136766029,
    0.0590640325657381,
    0.125847456119664,
    0.232931221473374,
    0.378259188739121,
    0.54495891589689,
    0.705949748288983,
    0.835980805221058,
    0.922929283049132,
    0.970355725500809,
    0.991009176245894,
    0.997963909660055,
    0.999685712592687,
    0.999972679748126,
    0.99999934483779
  )
  expect_equal(result, expected, tolerance = 1e-5)
})

test_that("postprob from beta mixture priors utilise updated weights", {
  result1 = postprob(x = 16, n = 23, p = 0.60, parE = c(0.6, 0.4), weights = 1)
  result2 = postprob(x = 16, n = 23, p = 0.60, parE = c(2, 4), weights = 1)
  result = 0.5 %*% (result1 + result2)
  expected = postprob(
    x = 16,
    n = 23,
    p = 0.60,
    par = rbind(c(0.6, 0.4), c(2, 4)),
    weights = c(0.5, 0.5)
  )
  result3 = postprob(x = 16, n = 23, p = 0.60, parE = c(2, 4), weights = 1)
  expected3 = postprob(
    x = 16,
    n = 23,
    p = 0.60,
    par = rbind(c(0.6, 0.4), c(2, 4)),
    weights = c(0, 1)
  )
  expected4 = postprob(
    x = 16,
    n = 23,
    p = 0.60,
    par = rbind(c(0.6, 0.4), c(2, 4)),
    weights = c(0, 1)
  )
  expect_true(result != expected)
  expect_equal(result3, expected3, tolerance = 1e-7)
  expect_equal(expected3, expected4, tolerance = 1e-7)
})

test_that("postprob can correct weights not summing to 1", {
  expect_warning(
    postprob(
      x = 16,
      n = 23,
      p = 0.60,
      par = rbind(c(0.6, 0.4), c(2, 4)),
      weights = c(2, 3)
    ),
    "Weights have been corrected. Advise to review allocated weights"
  )
})
