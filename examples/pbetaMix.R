pbetaMix(x = 0.3, par = rbind(c(0.2, 0.4)), weights = 1)

## Can get the one minus CDF values
pbetaMix(x = 0.3, par = rbind(c(0.2, 0.4)), weights = 1, lower.tail = FALSE)

## With 2 mixture components
## Weight 0.6 for component 1; a = 0.2, b = 0.4
## Weight 0.4 for component 2; a = 1.0, b = 1.0
pbetaMix(
  x = 0.3, par = rbind(c(0.2, 0.4), c(1, 1)),
  weights = c(0.6, 0.4)
)

## Can also specify x as a vector.
pbetaMix(
  x = seq(0, 1, .01), par = rbind(c(0.2, 0.4), c(1, 1)),
  weights = c(0.6, 0.4)
)
