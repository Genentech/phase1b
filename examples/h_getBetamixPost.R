# example from Lee and Liu (2008) :
h_getBetamixPost(x = 16, n = 23, par = t(c(0.6, 0.4)), weights = 1)

h_getBetamixPost(
  x = 16, n = 23,
  par =
    rbind(
      c(0.6, 0.4),
      c(1, 1)
    ),
  weights = c(0.6, 0.4)
)
