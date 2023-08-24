test_that("the dbetabinom density for every x support is between 0 and 1", {
  results <- dbetabinom(10, 20, 0.7, 2)
  expect_number(results, lower = 0, upper = 1)
})

test_that("the sum of the dbetabinom density for all x is 1", {
  result <- sum(dbetabinom(0:10, 10, 1, 1))
  expect_equal(result, 1)
})

test_that("the pbetaMix has incrementally higher cdf with increase x support", {
  is_lower <- pbetaMix(x = 0.3, par = rbind(c(0.2, 0.4)), weights = 1)
  is_higher <- pbetaMix(x = 0.5, par = rbind(c(0.2, 0.4)), weights = 1)
  expect_true(is_lower < is_higher)
})

test_that("the pbetaMix has the correct numeric result", {
  result <- pbetaMix(
    x = 0.3, par = rbind(c(0.2, 0.4), c(1, 1)),
    weights = c(0.6, 0.4)
  )
  expect_equal(result, 0.4768404, tolerance = 1e9)
})

test_that("the complement of pbetaMix can be derived with a different lower.tail flag", {
  result <- pbetaMix(x = 0.3, par = rbind(c(0.2, 0.4)), weights = 1, lower.tail = FALSE)
  result_inversed <- pbetaMix(x = 0.3, par = rbind(c(0.2, 0.4)), weights = 1, lower.tail = TRUE)
  expect_equal(result, 1 - result_inversed, tolerance = 1e9)
})
