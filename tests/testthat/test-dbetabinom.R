test_that("the dbetabinom density for every x support is between 0 and 1", {
  results <- dbetabinom(10, 20, 0.7, 2)
  expect_number(results, lower = 0, upper = 1)
})

test_that("the sum of the dbetabinom density for all x is 1", {
  result <- sum(dbetabinom(0:10, 10, 1, 1))
  expect_equal(result, 1)
})
