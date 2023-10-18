# dbetadiff ----

# We calculate the Go criteria and compare it with a Monte Carlo result
test_that("Monte Carlo result converges to Go probability", {
  results <- integrate(
    f = dbetadiff,
    parY = parY,
    parX = parX,
    lower = 0.15,
    upper = 1
  )
  resultsMC <- mean(rbeta(n = 1e6, parY[1], parY[2]) - rbeta(n = 1e6, parX[1], parX[2]) > 0.15)
  expect_true(results$value - resultsMC < 1e-7)
})

# We calculate the Go criteria and compare it with a Monte Carlo result
test_that("Monte Carlo result converges to Stop probability", {
  results <- integrate(
    f = dbetadiff,
    parY = parY,
    parX = parX,
    lower = -1,
    upper = 0.5
  )
  resultsMC <- mean(rbeta(n = 1e6, parY[1], parY[2]) - rbeta(n = 1e6, parX[1], parX[2]) < 0.5)
  expect_true(results$value - resultsMC < 1e-4)
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
  expect_true(0.2 - result < 1e-7)
})
