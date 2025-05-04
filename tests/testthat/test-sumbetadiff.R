# sumBetaDiff ----
test_that("sumbetadiff works as expected", {
  parX <- c(1, 52) # Control group's parameters
  parY <- c(5.5, 20.5) # Treatment group's parameters
  result <- sumBetaDiff(
    parX = parX,
    parY = parY,
    ci_level = 0.8, # 80 % credible interval
    go_cut = 0.9,
    stop_cut = 0.2
  )
  expect_equal(result$mode, 0.1704, tolerance = 1e-3)
  expect_equal(result$ci[1], 0.0942, tolerance = 1e-3)
  expect_equal(result$ci[2], 0.3007, tolerance = 1e-3)
  expect_equal(result$go, 1.078614e-17, tolerance = 1e-4)
  expect_equal(result$stop, 0.568, tolerance = 1e-3)
  expect_list(result,
    types = "numeric",
    any.missing = FALSE
  )
})

test_that("sumbetadiff gives a error when at least one alpha = 0", {
  parX <- c(0, 10)
  parY <- c(5.5, 20.5)
  expect_error(sumBetaDiff(
    parX = parX,
    parY = parY,
    ci_level = 0.8,
    go_cut = 0.9,
    stop_cut = 0.2
  ))
})
