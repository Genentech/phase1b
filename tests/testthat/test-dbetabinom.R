# dbetabinom ----

test_that("the dbetabinom density for every x support is between 0 and 1", {
  results <- dbetabinom(x = 10, m = 20, a = 0.7, b = 2)
  expect_number(results, lower = 0, upper = 1)
})

test_that("the sum of the dbetabinom density for all x is 1", {
  result <- sum(dbetabinom(x = 0:10, m = 10, a = 1, b = 1))
  expect_equal(result, 1)
})

test_that("Beta binomial density has correct numeric result for specific inputs", {
  result <- dbetabinom(x = 2, m = 29, a = 0.2, b = 0.4)
  expect_equal(result, 0.04286893, tolerance = 1e-6)
})

# dbetabinomMix ----

test_that("the beta mixture has a result between 0 and 1", {
  result <- dbetabinomMix(
    x = 2,
    m = 29,
    par = rbind(c(0.2, 0.4)),
    weights = 1
  )
  expect_numeric(result, lower = 0, upper = 1, finite = TRUE)
})

test_that("the beta mixture density has the correct numeric result", {
  result <- dbetabinomMix(
    x = 2,
    m = 29,
    par = rbind(c(0.2, 0.4)),
    weights = 1
  )
  expect_equal(result, 0.04286893, tolerance = 1e-6)
})

test_that("the sum of the beta mixture density for all x is 1", {
  result <- sum(
    dbetabinomMix(
      x = 0:20,
      m = 20,
      par = matrix(c(1, 2), ncol = 2, nrow = 1),
      weights = c(0.2, 0.8)
    )
  )
  expect_equal(result, 1)
})

test_that("Beta mixture density has the correct numeric result", {
  result <- dbetabinomMix(
    x = 2,
    m = 29,
    par = rbind(c(0.2, 0.4), c(1, 1)),
    weights = c(0.6, 0.4)
  )
  expect_equal(result, 0.03905469, tolerance = 1e-6)
})

## pbetaMix ----

test_that("The pbetaMix has incrementally higher cdf with increase x support", {
  is_lower <- pbetaMix(
    q = 0.3,
    par = rbind(c(0.2, 0.4)),
    weights = 1
  )
  is_higher <- pbetaMix(
    q = 0.5,
    par = rbind(c(0.2, 0.4)),
    weights = 1
  )
  expect_true(is_lower < is_higher)
})

test_that("The pbetaMix has the correct number result", {
  result <- pbetaMix(
    q = 0.3,
    par = rbind(c(0.2, 0.4), c(1, 1)),
    weights = c(0.6, 0.4)
  )
  expect_equal(result, 0.4768404, tolerance = 1e-5)
})

test_that("The complement of pbetaMix can be derived with a different lower.tail flag", {
  result <- pbetaMix(
    q = 0.3,
    par = rbind(c(0.2, 0.4)),
    weights = 1,
    lower.tail = FALSE
  )
  result_inversed <- pbetaMix(
    q = 0.3,
    par = rbind(c(0.2, 0.4)),
    weights = 1,
    lower.tail = TRUE
  )
  expect_equal(result, 1 - result_inversed, tolerance = 1e-5)
})

## qbetaMix ----

test_that("The qbetaMix has the correct number result", {
  result <- qbetaMix(
    p = 0.6,
    par = rbind(c(0.2, 0.4)),
    weights = 1
  )
  expect_equal(result, 0.3112068, tolerance = 1e-6)
})

test_that("The qbetaMix has the correct number result", {
  result <- qbetaMix(
    p = 0.6,
    par = rbind(c(0.2, 0.4), c(1, 1)),
    weights = c(0.6, 0.4)
  )
  expect_equal(result, 0.488759, tolerance = 1e-6)
})

test_that("The qbetaMix has a number result", {
  result <- qbetaMix(
    p = seq(0, 1, .01),
    par = rbind(c(0.2, 0.4), c(1, 1)),
    weights = c(0.6, 0.4)
  )
  expect_numeric(result)
})
