#-- get_distance (helper function)
test_that("get_distance gives an error with one element numeric", {
  expect_error(get_distance(10), "error!")
})

test_that("get_distance gives results within range", {
  results <- get_distance(c(10, 20, 30))
  expect_number(results, lower = 10, upper = 30)
})

test_that("get_distance gives results within range", {
  set.seed(1989)
  results <- get_distance(c(10, 20, 30))
  results_inv <- get_distance(c(10, 20, 30))
  expect_true(results && results_inv, lower = 10, upper = 30)
})

#-- get_looks (helper function)
test_that("get_looks gives correct results if input is identical", {
  dist <- c(0, 5)
  results <- get_looks(dist = dist, nnE = c(10, 20, 30), nnF = nnE)
  expect_equal(results$nnrE, results$nnrF)
})

test_that("get_looks gives correct results if input is identical", {
  dist <- c(0, 5)
  results <- get_looks(dist = dist, nnE = c(10, 20, 30), nnF = nnE)
  expect_equal(results$nnrE, results$nnrF)
})

#-- get_decision (helper function)
test_that("get_decision has list outputs of length of sim each", {
  p <- 0.3
  p0 <- 0.2
  p1 <- 0.3
  tL <- 0.5
  tU <- 0.8
  wiggle <- TRUE
  randomdist <- NULL
  decision <- all_sizes <- NA
  sim <- 10000
  for (k in 1:sim) {
    if (length(nn) != 1 && wiggle && is.null(randomdist)) {
      dist <- get_distance(nn)
      nnr <- get_looks(dist, nnE, nnF)
      nnrE <- nnr$nnrE
      nnrF <- nnr$nnrF
    } else {
      nnrE <- nnE
      nnrF <- nnF
    }
    nnr <- unique(c(nnrE, nnrF))
    tmp <- get_decision(
      nnr = nnr, response = response,
      truep = truep, p0 = p0, p1 = p1,
      parE = c(1, 1), nnE = nnrE,
      nnF = nnrF, tL = tL, tU = tU
    )
    decision[k] <- tmp$decision
    all_sizes[k] <- tmp$all_sizes
  }
  expect_numeric(tmp$decision, max.len = sim)
})

test_that("get_decision has list outputs of length of sim each", {
  p <- 0.3
  p0 <- 0.2
  p1 <- 0.3
  tL <- 0.5
  tU <- 0.8
  wiggle <- TRUE
  randomdist <- NULL
  decision <- all_sizes <- NA
  sim <- 10000
  for (k in 1:sim) {
    if (length(nn) != 1 && wiggle && is.null(randomdist)) {
      dist <- get_distance(nn)
      nnr <- get_looks(dist, nnE, nnF)
      nnrE <- nnr$nnrE
      nnrF <- nnr$nnrF
    } else {
      nnrE <- nnE
      nnrF <- nnF
    }
    nnr <- unique(c(nnrE, nnrF))
    tmp <- get_decision(
      nnr = nnr, response = response,
      truep = truep, p0 = p0, p1 = p1,
      parE = c(1, 1), nnE = nnrE,
      nnF = nnrF, tL = tL, tU = tU
    )
    decision[k] <- tmp$decision
    all_sizes[k] <- tmp$all_sizes
  }
  expect_numeric(tmp$all_sizes, max.len = sim)
})

test_that("get_decision has list outputs of length of sim each", {
  p <- 0.3
  p0 <- 0.2
  p1 <- 0.3
  tL <- 0.5
  tU <- 0.8
  wiggle <- TRUE
  randomdist <- NULL
  decision <- all_sizes <- NA
  sim <- 10000
  for (k in 1:sim) {
    if (length(nn) != 1 && wiggle && is.null(randomdist)) {
      dist <- get_distance(nn)
      nnr <- get_looks(dist, nnE, nnF)
      nnrE <- nnr$nnrE
      nnrF <- nnr$nnrF
    } else {
      nnrE <- nnE
      nnrF <- nnF
    }
    nnr <- unique(c(nnrE, nnrF))
    tmp <- get_decision(
      nnr = nnr, response = response,
      truep = truep, p0 = p0, p1 = p1,
      parE = c(1, 1), nnE = nnrE,
      nnF = nnrF, tL = tL, tU = tU
    )
    decision[k] <- tmp$decision
    all_sizes[k] <- tmp$all_sizes
  }
  expect_equal(length(tmp$decision), length(tmp$all_sizes))
})

#-- get_oc (helper function)
test_that("the probability results of get_oc are less than 1", {
  oc <- get_oc(
    all_sizes = sample(c(11, 14, 20), 10000, replace = TRUE),
    decision = sample(c(NA, TRUE, FALSE), 10000, replace = TRUE),
    sim = 10000,
    SizeEff = c(11, 14, 20),
    SizeFut = c(11, 14, 20)
  )
  expect_true(oc$PrStopEarly > 1) # can have more than 1 expect_true ?
})

test_that("the ExpectedN is within range based on vector of looks", {
  oc <- get_oc(
    all_sizes = sample(c(11, 14, 20), 10000, replace = TRUE),
    decision = sample(c(NA, TRUE, FALSE), 10000, replace = TRUE),
    sim = 10000,
    SizeEff = c(11, 14, 20),
    SizeFut = c(11, 14, 20)
  )
  expect_numper(oc$ExpectedN, lower = min(all_sizes), upper = max(all_sizes)) # can have more than 1 expect_true ?
})

# -- ocPostprob
test_that("the sum of Eff, Fut, Gray zone probabiliy is 1", {
  results <- sum(ocPostprob$oc[4:7])
  expect_equal(result, 1)
})
