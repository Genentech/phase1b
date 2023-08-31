% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbetabinom.R
\name{dbetaMix}
\alias{dbetaMix}
\title{Beta-mixture density function}
\usage{
dbetaMix(x, par, weights, log = FALSE)
}
\arguments{
\item{x}{the abscissa}

\item{par}{the beta parameters matrix, with K rows and 2 columns,
corresponding to the beta parameters of the K components}

\item{weights}{the mixture weights of the beta mixture prior}

\item{log}{return the log value? (not default)}
}
\value{
the (log) density values
}
\description{
Note that \code{x} can be a vector.
}