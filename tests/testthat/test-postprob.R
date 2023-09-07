# -- postprobOld
test_that("postprobOld gives the correct number result", {
  # Example from Lee & Liu (2006) A predictive probability design for phase II cancer clinical trials
  result <- postprobOld(x = 16, n = 23, p = 0.60, a = 0.6, b = 0.4)
  expect_equal(result, 0.8359808, tolerance = 1e-5)
})

test_that("postprobOld gives incrementally higher cdf with increase x support", {
  is_lower <- postprobOld(x = 10, n = 23, p = 0.60, a = 0.6, b = 0.4)
  is_higher <- postprobOld(x = 16, n = 23, p = 0.60, a = 0.6, b = 0.4)
  expect_true(is_lower < is_higher)
})


# -- postprob
test_that("postprob gives the correct number result", {
  # Example from Lee & Liu (2006) A predictive probability design for phase II cancer clinical trials
  result <- postprob(x = 16, n = 23, p = 0.60, par = c(0.6, 0.4))
  expect_equal(result, 0.8359808, tolerance = 1e-5)
})

test_that("postprob gives the correct number result", {
  # 2 component beta mixture prior, i.e., P_E ~ 0.6*beta(0.6,0.4) + 0.4*beta(1,1) and Pr(P_E > p | data) = 0.823
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

test_that("postprob gives incrementally higher cdf with increase x support", {
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
