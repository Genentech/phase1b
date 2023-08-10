test_that("unfortunately we don't have unit tests for phase1b yet", {
  parX <- c(1, 52)
  parY <- c(5.5, 20.5)
  z <- seq(from = -1, to = 1, length = 100)
  result <- expect_silent(dbetadiff(z, parY = parY, parX = parX))
})

test_that("unfortunately that is not a numeric input", {
  x <- c(seq(0, 1, 0.01))
  result <- logit(x)
  expect_numeric(result)
  expect_equal(x, result)
})
