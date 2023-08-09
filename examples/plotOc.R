# get operating character result from oc.postprob

res1 <- ocPostprob(nn = c(10, 20, 30), p = 0.4, p0 = 0.2, p1 = 0.3, tL = 0.6, tU = 0.8, parE = c(1, 1), ns = 10000)
res1$oc

plotOc(res1)
