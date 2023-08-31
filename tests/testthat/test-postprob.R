# -- postprobOld
test_that("The postprob has the correct number result", {
  # Example from Lee & Liu (2006) A predictive probability design for phase II cancer clinical trials
  result <- postprob(x = 16, n = 23, p = 0.60, par = c(0.6, 0.4))
  expect_equal(result, 0.8359808, tolerance = 1e-5)
})

test_that("The postprob has incrementally higher cdf with increase x support", {
  is_lower <- postprob(x = 10, n = 23, p = 0.60, par = c(0.6, 0.4))
  is_higher <- postprob(x = 16, n = 23, p = 0.60, par = c(0.6, 0.4))
  expect_true(is_lower < is_higher)
})
