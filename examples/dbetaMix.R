## Calculating the density of a mixture
## of beta densities at x
## x = 0.3; a = 0.2; b = 0.4
##
## Only 1 mixture component, i.e., weights = 1
## Compare to dbeta(0.3,0.2,0.4) = 0.4745802
##
dbetaMix(x = 0.3, par = rbind(c(0.2, 0.4)), weights = 1)

## With 2 mixture components
## Weight 0.6 for component 1; a = 0.2, b = 0.4
## Weight 0.4 for component 2; a = 1.0, b = 1.0
## Compare to 0.6*dbeta(0.3,0.2,0.4) + 0.4*dbeta(0.3,1,1) = 0.6847481
##
dbetaMix(
  x = 0.3, par = rbind(c(0.2, 0.4), c(1, 1)),
  weights = c(0.6, 0.4)
)

## Can also specify x as a vector
## x = seq(0,1,.01)
##
dbetaMix(
  x = seq(0, 1, .01), par = rbind(c(0.2, 0.4), c(1, 1)),
  weights = c(0.6, 0.4)
)
