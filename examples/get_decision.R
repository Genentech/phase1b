# multiple looks @ 10, 20, 30 patients for Efficacy and Futility
# True response rate of the treatment group=40%
# Look for futility: P(response rate < 20% )> 60%
# Look for efficacy: P(response rate > 30% )> 80%
# Prior of treatment arm parE = Beta(1,1)

# argument inputs for get_decision
nn <- c(10, 20, 30)
dist <- get_distance(c(10, 20, 30))
looks <- get_looks(dist, c(10, 20, 30), c(10, 20, 30))

get_decision(
  nnr = nn,
  response = rbinom(n = max(nn), size = 1, prob = 0.40),
  truep = 0.4,
  p0 = 0.20,
  p1 = 0.30,
  parE = c(1, 1),
  nnE = looks$nnrE,
  nnF = looks$nnrF,
  tL = 0.80,
  tU = 0.60
)
