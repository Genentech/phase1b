# get operating character result from oc.postprob

oc <- ocPostprob(
  nnE = c(10, 20, 30), truep = 0.4, p0 = 0.2,
  p1 = 0.3, tL = 0.6, tU = 0.8, parE = c(1, 1), sim = 100, wiggle = FALSE
)
oc$oc
# plotOc(oc)


oc <- ocPostprob(
  nnE = c(10, 20, 30), truep = 0.4, p0 = 0.2,
  p1 = 0.3, tL = 0.6, tU = 0.8, parE = c(1, 1), sim = 100, wiggle = TRUE
)
oc$oc

