# dbetabinom ----

test_that("dbetabinom for every x support is between 0 and 1", {
  results <- dbetabinom(x = 10, m = 20, a = 0.7, b = 2)
  expect_number(results, lower = 0, upper = 1)
})

test_that("sum of the dbetabinom values over the whole support for x is 1", {
  result <- sum(dbetabinom(x = 0:10, m = 10, a = 1, b = 1))
  expect_identical(result, 1)
})

test_that("dbetabinom gives correct numeric result with uniform parameters", {
  result <- dbetabinom(x = 2, m = 29, a = 0.2, b = 0.4)
  expect_equal(result, 0.04286893, tolerance = 1e-6)
})

# dbetabinomMix ----

test_that("dbetabinomMix gives a result between 0 and 1", {
  result <- dbetabinomMix(
    x = 2,
    m = 29,
    par = rbind(c(0.2, 0.4)),
    weights = 1
  )
  expect_numeric(result, lower = 0, upper = 1, finite = TRUE)
})

test_that("dbetabinomMix gives the correct numeric result with non-uniform parameters", {
  result <- dbetabinomMix(
    x = 2,
    m = 29,
    par = rbind(c(0.2, 0.4)),
    weights = 1
  )
  expect_equal(result, 0.04286893, tolerance = 1e-6)
})

test_that("Sum of dbetabinomMix for all x is 1", {
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

test_that("dbetabinomMix gives the correct numeric result with beta-mixture", {
  result <- dbetabinomMix(
    x = 2,
    m = 29,
    par = rbind(c(0.2, 0.4), c(1, 1)),
    weights = c(0.6, 0.4)
  )
  expect_equal(result, 0.03905469, tolerance = 1e-6)
})

# pbetaMix ----

test_that("The pbetaMix has incrementally higher cdf with larger x", {
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

test_that("pbetaMix gives the correct number result with beta-mixture", {
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

# qbetaMix ----

test_that("qbetaMix gives the correct number result with beta-mixture", {
  result <- qbetaMix(
    p = 0.6,
    par = rbind(c(0.2, 0.4)),
    weights = 1
  )
  expect_equal(result, 0.3112068, tolerance = 1e-6)
})

test_that("qbetaMix gives the correct number result with beta-mixture with increased parameters", {
  result <- qbetaMix(
    p = 0.6,
    par = rbind(c(0.2, 0.4), c(1, 1)),
    weights = c(0.6, 0.4)
  )
  expect_equal(result, 0.488759, tolerance = 1e-6)
})

test_that("qbetaMix gives a number result with beta-mixture", {
  result <- qbetaMix(
    p = seq(0, 1, .01),
    par = rbind(c(0.2, 0.4), c(1, 1)),
    weights = c(0.6, 0.4)
  )
  expect_numeric(result)
})

# dbetaMix ----

test_that("dbetaMix gives the correct result with a 1 mixture component", {
  result <- dbetaMix(x = 0.3, par = rbind(c(0.2, 0.4)), weights = 1)
  expect_equal(result, 0.4745802, tolerance = 1e-4)
})

test_that("dbetaMix gives the correct result with increased parameters", {
  result <- dbetaMix(x = 0.3, par = rbind(c(0.2, 0.4), c(1, 2)), weights = c(1, 1))
  expect_equal(result, 1.87458, tolerance = 1e-4)
})

test_that("dbetaMix gives error when weights do not sum to 1", {
  expect_error(
    results <- dbetaMix(
      x = 0.3,
      par = rbind(c(0.2, 0.4), c(1, 1)),
      weights = c(1, 1)
    ), "failed"
  )
})

test_that("dbetaMix gives the correct result as dbeta", {
  result <- dbetaMix(
    x = 0.3, par = rbind(c(0.2, 0.4), c(1, 1)),
    weights = c(0.6, 0.4)
  )
  result2 <- 0.6 * dbeta(
    x = 0.3,
    shape1 = 0.2,
    shape2 = 0.4
  ) + 0.4 * dbeta(
    x = 0.3,
    shape1 = 1,
    shape2 = 1,
  )
  expect_equal(result, result2, tolerance = 1e-4)
})

# h_getBetamixPost ----

test_that("h_getBetamixPost gives the correct beta-mixture parameters", {
  result <- h_getBetamixPost(
    x = 16,
    n = 23,
    par = matrix(c(1, 2), ncol = 2),
    weights = 1
  )
  expected <- list(
    par = matrix(c(17, 9), nrow = 1),
    weights = 1
  )
  expect_identical(result[result$par], expected)
})

test_that("h_getBetamixPost gives weight of 1 for a single beta distribution", {
  results <- h_getBetamixPost(
    x = 16,
    n = 23,
    par = rbind(c(1, 2)),
    weights = 0.1
  )
  expect_identical(results$weights, 1)
})

test_that("h_getBetamixPost gives correct weights with 2 beta-mixture component", {
  result <- h_getBetamixPost(
    x = 16,
    n = 23,
    par = rbind(c(1, 2), c(3, 4)),
    weights = c(0.6, 0.4)
  )
  expect_equal(result$weights, c(0.5085758, 0.4914242), tolerance = 1e-4)
})

test_that("h_getBetamixPost gives correct beta parameters with beta-mixture", {
  result <- h_getBetamixPost(
    x = 16,
    n = 23,
    par = rbind(c(1, 2), c(3, 4)),
    weights = c(0.6, 0.4)
  )
  expect_identical(result$par, rbind(c(17, 9), c(19, 11)))
})

test_that("h_getBetamixPost gives the correct weights when sum of weights is not 1 in beta-mixture", {
  result <- h_getBetamixPost(
    x = 16,
    n = 23,
    par = rbind(c(1, 2), c(3, 4), c(10, 10)),
    weights = c(0.6, 0.4, 0.5)
  )
  expect_equal(result$weights, c(.2776991, 0.2683337, 0.4539671), tolerance = 1e-4)
})

test_that("h_getBetamixPost gives the correct beta parameters when sum of weights is not 1", {
  result <- h_getBetamixPost(
    x = 16,
    n = 23,
    par = rbind(c(1, 2), c(3, 4), c(10, 10)),
    weights = c(0.6, 0.4, 0.5)
  )
  expect_identical(result$par, rbind(c(17, 9), c(19, 11), c(26, 17)))
})

test_that("h_getBetamixPost gives error when K rows of weights exceed length of par", {
  expect_error(
    results <- h_getBetamixPost(
      x = 16,
      n = 23,
      par = rbind(c(1, 2)),
      weights = c(0.6, 0.4)
    ), "Must have length 1, but has length 2."
  )
})
