# sumTable ----
test_that("sumTable works as expected", {
  result <- sumTable(
    x = 2,
    n = 40,
    parX = c(1, 52), # Control group's parameters
    parY = c(1, 1), # Treatment group's parameters
    go_cut = 0.2,
    stop_cut = 0.05
  )
  expected <- data.frame(
    list(
      summaries = c(
        2,
        5,
        3.66,
        -0.92,
        13.07,
        0.37,
        52.26
      )
    ),
    row.names = c(
      "responders",
      "obs ORR [%]",
      "mode [%]",
      "CI lower [%]",
      "CI upper [%]",
      "prob.go [%]",
      "prob.nogo [%]"
    )
  )
  expect_identical(result, expected)
})

test_that("sumTable gives a error when at least one alpha = 0", {
  expect_error(
    sumTable(
      x = 2,
      n = 40,
      parX = c(0, 10), # Control group's parameters
      parY = c(1, 1), # Treatment group's parameters
      go_cut = 0.2,
      stop_cut = 0.05
    )
  )
})

test_that("sumTable works as expected when x = 0", {
  result <- sumTable(
    x = 0,
    n = 40,
    parX = c(2, 10), # Control group's parameters
    parY = c(1, 1), # Treatment group's parameters
    go_cut = 0.2,
    stop_cut = 0.05
  )
  expected <- data.frame(
    list(
      summaries = c(0, 0, -8.03, -34.32, -0.44, 0, 99.53)
    ),
    row.names =
      c(
        "responders", "obs ORR [%]", "mode [%]", "CI lower [%]",
        "CI upper [%]", "prob.go [%]", "prob.nogo [%]"
      )
  )
  expect_identical(result, expected)
})

test_that("sumTable works as expected when n = 0", {
  expect_error(
    sumTable(
      x = 5,
      n = 0,
      parX = c(2, 10), # Control group's parameters
      parY = c(1, 1), # Treatment group's parameters
      go_cut = 0.2,
      stop_cut = 0.05
    )
  )
})
