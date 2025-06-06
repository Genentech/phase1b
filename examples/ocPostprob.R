# For three looks of 10, 20 and 30 we have the following assumptions :
# True response rate or truep of the treatment group = 40%
# The following are the Stop rules respectively :
# Look for Efficacy: Pr(RR > 30% )> 80%
# Look for Futility: Pr(RR < 20% )> 60%
# Prior of treatment arm parE= Beta(1,1).
res <- ocPostprob(
  nnE = c(10, 20, 30), truep = 0.40, p0 = 0.20, p1 = 0.30, tL = 0.60, tU = 0.80, parE = c(1, 1),
  sim = 50, wiggle = TRUE, nnF = c(10, 20, 30)
)

res$oc

# We specify the distance in this example.
res <- ocPostprob(
  nnE = c(10, 20, 30), truep = 0.40, p0 = 0.20, p1 = 0.30, tL = 0.60, tU = 0.80, parE = c(1, 1),
  sim = 50, wiggle = TRUE, nnF = c(10, 20, 30)
)

res$oc

# Here, nnE = nnF, and no wiggle room is allowed. Random distance also not supplied.
res <- ocPostprob(
  nnE = c(10, 20, 30), truep = 0.40, p0 = 0.20, p1 = 0.30, tL = 0.60, tU = 0.80, parE = c(1, 1),
  sim = 50, wiggle = FALSE, nnF = c(10, 20, 30)
)

res$oc

# Here, we only have one Futility and Efficacy look or stop.
res <- ocPostprob(
  nnE = c(10), truep = 0.40, p0 = 0.20, p1 = 0.30, tL = 0.60, tU = 0.80, parE = c(1, 1),
  sim = 50, wiggle = FALSE, nnF = 10
)

res$oc

# Here, we only have one Futility but many Efficacy looks or stop.
res <- ocPostprob(
  nnE = c(10, 20, 30), truep = 0.40, p0 = 0.20, p1 = 0.30, tL = 0.60, tU = 0.80, parE = c(1, 1),
  sim = 50, wiggle = FALSE, nnF = 10
)

res$oc
