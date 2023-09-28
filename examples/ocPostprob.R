# Three looks--
# design details (example)
# multiple looks @ 10, 20, 30 patients
# True response rate of the treatment group=40%
# Look for futility: P(response rate < 20% )> 60%
# Look for efficacy: P(response rate > 30% )> 80%
# prior of treatment arm parE= Beta(1,1)
res <- ocPostprob(
  nnE = c(10, 20, 30), truep = 0.40, p0 = 0.20, p1 = 0.30, tL = 0.60, tU = 0.80, parE = c(1, 1),
  sim = 1000, wiggle = TRUE, randomdist = NULL, nnF = c(10, 20, 30)
)

res$oc

# Specify distance
res <- ocPostprob(
  nnE = c(10, 20, 30), truep = 0.40, p0 = 0.20, p1 = 0.30, tL = 0.60, tU = 0.80, parE = c(1, 1),
  sim = 1000, wiggle = TRUE, randomdist = c(-1, 3), nnF = c(10, 20, 30)
)

res$oc

# No Wiggle
res <- ocPostprob(
  nnE = c(10, 20, 30), truep = 0.40, p0 = 0.20, p1 = 0.30, tL = 0.60, tU = 0.80, parE = c(1, 1),
  sim = 1000, wiggle = FALSE, randomdist = NULL, nnF = c(10, 20, 30)
)

res$oc

# Only one efficacy + many futility
res <- ocPostprob(
  nnE = c(10), truep = 0.40, p0 = 0.20, p1 = 0.30, tL = 0.60, tU = 0.80, parE = c(1, 1),
  sim = 1000, wiggle = FALSE, randomdist = NULL, nnF = c(10)
)

res$oc

# Only one futility
res <- ocPostprob(
  nnE = c(10, 20, 30), truep = 0.40, p0 = 0.20, p1 = 0.30, tL = 0.60, tU = 0.80, parE = c(1, 1),
  sim = 1000, wiggle = FALSE, randomdist = NULL, nnF = c(10)
)

res$oc
