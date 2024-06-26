% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predprobDist.R
\name{h_predprobdist}
\alias{h_predprobdist}
\title{The predictive probability of success in two-arm studies.}
\usage{
h_predprobdist(
  x,
  n,
  xS,
  nS,
  Nmax,
  NmaxControl,
  delta,
  relativeDelta,
  parE,
  parS,
  weights,
  weightsS,
  thetaT
)
}
\arguments{
\item{x}{(\code{number}):\cr number of successes in the \code{E} group at interim.}

\item{n}{(\code{number}):\cr number of patients in the \code{E} group at interim.}

\item{xS}{(\code{number}):\cr number of successes in the \code{S} group at interim.}

\item{nS}{(\code{number}):\cr number of patients in the \code{E} group at interim.}

\item{Nmax}{(\code{number}):\cr maximum number of patients in the \code{E} group at final analysis.}

\item{NmaxControl}{(\code{number}):\cr maximum number of patients in the \code{S} group at final analysis.}

\item{delta}{(\code{number}):\cr difference between response rates to be met.}

\item{relativeDelta}{(\code{flag}):\cr If \code{TRUE}, then a \code{relativeDelta} is used. Represents that a minimum
response rate in magnitude of \code{delta} of the \code{S} non-responding patients. See \code{\link[=postprobDist]{postprobDist()}}.}

\item{parE}{(\code{numeric}):\cr parameters for beta distribution. If it is a matrix, it needs to have 2 columns,
and each row corresponds to each component of a beta-mixture distribution
for the \code{E} group.}

\item{parS}{(\code{numeric}):\cr parameters for beta distribution. If it is a matrix, it needs to have 2 columns,
and each row corresponds to each component of a beta-mixture distribution for the \code{S} group.}

\item{weights}{(\code{numeric}):\cr the mixture weights of the beta mixture prior.}

\item{weightsS}{(\code{numeric}):\cr weights for the \code{S} group.}

\item{thetaT}{(\code{number}):\cr threshold on the probability to be used.}
}
\value{
A \code{list} is returned with names \code{result} for predictive probability and
\code{table} of numeric values with counts of responses in the remaining patients, \code{density} for
probabilities of these counts, \code{posterior} for corresponding probabilities to be above threshold,
and \code{success}for trial success indicators.
}
\description{
The helper function to generate the predictive probability of success
based on the difference in treatment group (\code{E}) and control or
standard of care (\code{S}) group.
}
\keyword{internal}
