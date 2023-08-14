test_that("the dbetabinom density for every x support is between 0 and 1", {
  results <- round(dbetabinom(10, 20, 0.7, 2), digits = 2)
  expected <- 0.04
  expect_number(results, lower = 0, upper = 1)
})
