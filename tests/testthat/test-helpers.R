test_that("logit works as expected", {
  probs <- seq(0, 1, length = 100)
  result <- logit(probs)
  expect_numeric(result, len = length(probs), sorted = TRUE, unique = TRUE)
  expect_identical(result[1], -Inf)
  expect_identical(result[100], Inf)
})

test_that("logit gives expected value for example value 0.2", {
  result <- logit(0.2)
  expected <- -1.386294
  expect_equal(result, expected, tolerance = 1e-4)
})
