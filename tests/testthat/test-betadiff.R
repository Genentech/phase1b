# The following examples use these parameters:
parX <- c(1, 52)
parY <- c(5.5, 20.5)

# dbetadiff ----
test_that("dbetadiff gives error with empty numeric for z", {
  z <- NA
  expect_error(dbetadiff(z, parY = parY, parX = parX))
})

test_that("Monte Carlo result converges to Go probability", {
  set.seed(1989)
  results <- integrate(
    f = dbetadiff,
    parY = parY,
    parX = parX,
    lower = 0.15,
    upper = 1
  )
  expected <- mean(rbeta(n = 1e6, parY[1], parY[2]) - rbeta(n = 1e6, parX[1], parX[2]) > 0.15)
  expect_equal(results$value, expected, tolerance = 1e-4)
})

test_that("Monte Carlo result converges to Stop probability", {
  set.seed(1989)
  results <- integrate(
    f = dbetadiff,
    parY = parY,
    parX = parX,
    lower = -1,
    upper = 0.5
  )
  expected <- mean(rbeta(n = 1e6, parY[1], parY[2]) - rbeta(n = 1e6, parX[1], parX[2]) < 0.5)
  expect_equal(results$value, expected, tolerance = 1e-4)
})

# pbetadiff ----
test_that("pbetadiff gives the correct number result", {
  result <- pbetadiff(
    q = 0.4,
    parY = parY,
    parX = parX
  )
  expect_equal(result, 0.9884184, tolerance = 1e-6)
})

test_that("The pbetadiff has incrementally higher cdf with increase x support", {
  is_lower <- pbetadiff(
    q = 0.4,
    parY = parY,
    parX = parX
  )
  is_higher <- pbetadiff(
    q = 0.6,
    parY = parY,
    parX = parX
  )
  expect_true(is_lower < is_higher)
})

test_that("The entire Z domain from -1 to 1 gives a pbetadiff of 1", {
  result <- pbetadiff(1, parY, parX)
  expect_equal(result, 1)
})

# qbetadiff ----
test_that("qbetadiff gives the correct number result", {
  result <- qbetadiff(
    p = 0.2,
    parY = parY,
    parX = parX
  )
  expect_equal(result, 0.1228383, tolerance = 1e-6)
})

test_that("The result of qbetadiff corresponds to the result of pbetadiff ", {
  test <- qbetadiff(
    p = 0.2,
    parY = parY,
    parX = parX
  )
  result <- pbetadiff(
    q = test,
    parY = parY,
    parX = parX
  )
  expect_equal(0.2, result, tolerance = 1e-4)
})
