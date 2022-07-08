## Calculating the density of a mixture
## of beta binomial densities at x
## x = 2; m = 29; a = 0.2; b = 0.4
##
## Only 1 mixture component, i.e., weights = 1
## p(x) = choose(29,2)*beta(2.2,27.4)/beta(0.2,0.4) = 0.04286893
##
dbetabinomMix(x = 2, m = 29, par = rbind(c(0.2, 0.4)), weights = 1)

## With 2 mixture components
## Weight 0.6 for component 1; a = 0.2, b = 0.4
## Weight 0.4 for component 2; a = 1.0, b = 1.0
## p(x) = 0.6*choose(29,2)*beta(2.2,27.4)/beta(0.2,0.4) +
##        0.4*choose(29,2)*beta(3,28)/beta(1,1) = 0.03905469
##
dbetabinomMix(
  x = 2, m = 29, par = rbind(c(0.2, 0.4), c(1, 1)),
  weights = c(0.6, 0.4)
)

## Can also specify x as a vector
## x = 1:28
##
dbetabinomMix(
  x = 1:28, m = 29, par = rbind(c(0.2, 0.4), c(1, 1)),
  weights = c(0.6, 0.4)
)
