# dbetabinom ----

test_that("the dbetabinom density for every x support is between 0 and 1", {
  results <- dbetabinom(10, 20, 0.7, 2)
  expect_number(results, lower = 0, upper = 1)
})

test_that("the sum of the dbetabinom density for all x is 1", {
  result <- sum(dbetabinom(0:10, 10, 1, 1))
  expect_equal(result, 1)
})

test_that("Beta binomial density has correct numeric result for specific inputs", {
  result <- dbetabinom(x = 2, m = 29, a = 0.2, b = 0.4)
  expect_equal(result, 0.04286893)
})

# dbetabinomMix ----

test_that("the beta mixture has a result between 0 and 1", {
  result <- dbetabinomMix(
    x = 2, m = 29,
    par = rbind(c(0.2, 0.4)),
    weights = 1
  )
  expect_numeric(result, lower = 0, upper = 1, finite = TRUE)
})

test_that("the beta mixture density has the correct numeric result", {
  result <- dbetabinomMix(
    x = 2, m = 29,
    par = rbind(c(0.2, 0.4)),
    weights = 1
  )
  expect_equal(result, 0.04286893, tolerance = 1e-6)
})

test_that("the sum of the beta mixture density for all x is 1", {
  result <- sum(dbetabinomMix(0:20, 20,
    matrix(c(1, 2), ncol = 2, nrow = 1),
    weights = c(0.2, 0.8)
  ))
  expect_equal(result, 1)
})

test_that("Beta mixture density has the correct numeric result", {
  result <- dbetabinomMix(x = 2, m = 29, par = rbind(c(0.2, 0.4), c(1, 1)), weights = c(0.6, 0.4))
  expect_equal(result, 0.03905469, tolerance = 1e-6)
})
