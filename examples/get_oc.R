# argument inputs for get_decision
nn <- c(10, 20, 30)
dist <- get_distance(c(10, 20, 30))
looks <- get_looks(dist, c(10, 20, 30), c(10, 20, 30))

tmp <- get_decision(
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

get_oc(all_sizes = tmp$all_sizes, nnr = c(10, 20, 30), decision = tmp$decision, nnrE = looks$nnrE, nnrF = looks$nnrF)
