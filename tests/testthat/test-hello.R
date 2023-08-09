test_that("unfortunately we don't have unit tests for phase1b yet", {
  parX <- c(1, 52)
  parY <- c(5.5, 20.5)
  z <- seq(from = -1, to = 1, length = 100)
  result <- expect_silent(dbetadiff(z, parY = parY, parX = parX))
})
