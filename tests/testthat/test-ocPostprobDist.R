# h_get_decisionDist ----
test_that("h_get_decisionDist gives correct result when relativeDelta = TRUE", {
  set.seed(1989)
  example_dist <- h_get_distance(nn = c(10, 20, 30))
  looks <- h_get_looks(dist = example_dist, nnE = c(10, 20, 30), nnF = c(10, 20, 30))
  looks_nnrE <- looks$nnrE
  looks_nnrF <- looks$nnrF

  # input values from above arguments
  result <- h_get_decisionDist(
    nnr = c(10, 20, 30),
    nnrE = looks_nnrE,
    nnrF = looks_nnrF,
    truep = 0.4,
    parE = c(1, 1),
    tL = 0.8,
    tU = 0.6,
    deltaF = 0.1,
    deltaE = 0.2,
    relativeDelta = TRUE
  )
  expect_list(result, any.missing = FALSE)
  expect_equal(result$decision, NA)
  expect_equal(result$all_sizes, 30)
})

test_that("h_get_decisionDist gives correct result relativeDelta = FALSE", {
  set.seed(1989)
  example_dist <- h_get_distance(nn = c(10, 20, 30))
  looks <- h_get_looks(dist = example_dist, nnE = c(10, 20, 30), nnF = c(10, 20, 30))
  looks_nnrE <- looks$nnrE
  looks_nnrF <- looks$nnrF

  # input values from above arguments
  result <- h_get_decisionDist(
    nnr = c(10, 20, 30),
    nnrE = looks_nnrE,
    nnrF = looks_nnrF,
    truep = 0.4,
    parE = c(1, 1),
    tL = 0.8,
    tU = 0.6,
    deltaF = 0.1,
    deltaE = 0.2,
    relativeDelta = FALSE
  )
  expect_list(result, any.missing = FALSE)
  expect_equal(result$all_sizes, 30)
  expect_equal(result$decision, NA)
})


test_that("h_get_decisionDist gives correct result when nnE <U+2260> nnF", {
  set.seed(1989)
  example_dist <- h_get_distance(nn = c(10, 20, 30))
  looks <- h_get_looks(dist = example_dist, nnE = c(10, 20, 30), nnF = c(10, 20))
  looks_nnrE <- looks$nnrE
  looks_nnrF <- looks$nnrF

  # input values from above arguments
  result <- h_get_decisionDist(
    nnr = c(10, 20, 30),
    nnrE = looks_nnrE,
    nnrF = looks_nnrF,
    truep = 0.4,
    parE = c(1, 1),
    tL = 0.8,
    tU = 0.6,
    deltaF = 0.1,
    deltaE = 0.2,
    relativeDelta = FALSE
  )
  expect_list(result, any.missing = FALSE)
  expect_equal(result$decision, NA)
  expect_equal(result$all_sizes, 30)
})
