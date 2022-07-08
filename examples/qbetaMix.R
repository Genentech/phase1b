## Calculating the quantile (inverse CDF) of a mixture
## of beta densities at x
## q = 0.6; a = 0.2; b = 0.4
##
## Only 1 mixture component, i.e., weights = 1
## Compare to qbeta(0.6,0.2,0.4) = 0.3112065
##
qbetaMix(q = 0.60, par = rbind(c(0.2, 0.4)), weights = 1)

## With 2 mixture components
## Weight 0.6 for component 1; a = 0.2, b = 0.4
## Weight 0.4 for component 2; a = 1.0, b = 1.0
##
qbetaMix(
  q = 0.6, par = rbind(c(0.2, 0.4), c(1, 1)),
  weights = c(0.6, 0.4)
)

## Can also specify q as a vector
## q = seq(0,1,.01)
##
qbetaMix(
  q = seq(0, 1, .01), par = rbind(c(0.2, 0.4), c(1, 1)),
  weights = c(0.6, 0.4)
)
