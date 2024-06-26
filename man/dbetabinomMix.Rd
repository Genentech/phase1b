% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbetabinom.R
\name{dbetabinomMix}
\alias{dbetabinomMix}
\title{Beta-Mixture-Binomial Density Function}
\usage{
dbetabinomMix(x, m, par, weights, log = FALSE)
}
\arguments{
\item{x}{(\code{numeric}):\cr number of successes.}

\item{m}{(\code{number}):\cr number of trials.}

\item{par}{(\code{matrix}):\cr the beta parameters matrix, with K rows and 2 columns,
corresponding to the beta parameters of the K components.}

\item{weights}{(\code{numeric}):\cr the mixture weights of the beta mixture prior of length K.
Each element corresponds to the row of beta parameters in \code{par}.}

\item{log}{(\code{flag}):\cr whether to return the log density value (not default).}
}
\value{
The (log) density values of the mixture of beta-binomial distributions at \code{x}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Calculates the density function for a mixture of beta-binomial distributions.
}
\examples{
dbetabinomMix(x = 2, m = 29, par = rbind(c(0.2, 0.4)), weights = 1)

dbetabinomMix(
  x = 2, m = 29, par = rbind(c(0.2, 0.4), c(1, 1)),
  weights = c(0.6, 0.4)
)
# x could be a vector
dbetabinomMix(
  x = 1:28, m = 29, par = rbind(c(0.2, 0.4), c(1, 1)),
  weights = c(0.6, 0.4)
)
}
