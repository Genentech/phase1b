## Calculating the beta binomial density, x = 2; m = 29; a = 0.2; b = 0.4
##
## when p(x) = choose(29,2)*beta(2.2,27.4)/beta(0.2,0.4) = 0.04286893
##
dbetabinom(2, 29, 0.2, 0.4)

## Can also specify x as a vector, x = 1:28
##
##
dbetabinom(1:28, 29, 0.2, 0.4)
