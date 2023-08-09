## If x = 0.2
## then logit(x) = log(0.2/(1-0.2)) = -1.386294
##
x <- 0.2
logit(x)

## Can also be a vector of probabilities
##
x <- seq(0.1, 0.9, 0.1)
logit(x)
