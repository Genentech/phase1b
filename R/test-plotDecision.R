# plotDecision ----
testthat::test_that("plotDecision gives a correct result", {
  summaries <- do.call(
    cbind,
    lapply(c(0:8),
           sumTable,
           n = 25,
           parX = c(1, 52),
           go_cut = 0.2,
           stop_cut = 0.05
    )
  )
  result <- plotDecision(summaries, efficacious_prob = 60, futile_prob = 60)
  testthat::expect_equal(result$data$responders, c(0, 1, 2, 3, 4, 5, 6, 7, 8))
  expect_numeric(result$data$obs)
  expect_numeric(result$data$mode)
  expect_numeric(result$data$ci_lower)
  expect_numeric(result$data$ci_upper)
  expect_numeric(result$data$prob_go)
  expect_numeric(result$data$prob_stop)
  testthat::expect_identical(result$labels$title, "Probability of Difference and respective Go and Stop probabilities.")
  vdiffr::expect_doppelganger("plot of Probability of Difference and respective Go and Stop probabilities", result)
})


