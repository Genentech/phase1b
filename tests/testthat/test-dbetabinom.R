test_that("the dbetabinom density for every x support is between 0 and 1", {
  results <- dbetabinom(10, 20, 0.7, 2)
  expect_number(results, lower = 0, upper = 1)
})

test_that("the sum of the dbetabinom density for all x is 1", {
  result <- sum(dbetabinom(0:10, 10, 1, 1))
  expect_equal(result, 1)
})

test_that("the beta mixture has a numeric result between 0 and 1", {
  result <- dbetabinomMix(10, 20,
    matrix(c(1, 2), ncol = 2, nrow = 1),
    weights = c(0.2, 0.8)
  )
  expect_numeric(result, lower = 0, upper = 1, finite = TRUE)
})

test_that("the sum of the dbetabinom density for all x is 1", {
  result <- sum(dbetabinomMix(0:20, 20,
    matrix(c(1, 2), ncol = 2, nrow = 1),
    weights = c(0.2, 0.8)
  ))
  expect_equal(result, 1)
})
