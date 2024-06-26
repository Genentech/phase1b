# plotBetaDist ----
test_that("plotBeta gives a correct result", {
  result <- plotBeta(alpha = 4, beta = 5)
  expect_equal(length(result$data$grid), 1000)
  expect_numeric(result$data$grid)
  expect_numeric(result$data$xticks)
  expect_numeric(result$data$density)
  expect_identical(result$labels$title, "Beta density with alpha = 4 and beta = 5 parameters.")
})
