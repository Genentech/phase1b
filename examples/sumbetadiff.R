parX <- c(0, 52) # Control group's parameters
parY <- c(5.5, 20.5) # Treatment group's parameters
sumBetadiff(parX = parX, parY = parY, level = 0.9, go_cut = 0.8, stop_cut = 0.2)

#' Density, distribution function and quantile function for
#' the distribution of the difference of two Beta distributions with parameters `parX` and `parY`.
#' We denote `X` and `Y` as two random variables representing the response rate of Control and Treatment
#' group respectively. The assignment of Control and Treatment is practically interchangeable.
#' We denote `Z` as the difference between two groups such that `Z = Y-X`.
