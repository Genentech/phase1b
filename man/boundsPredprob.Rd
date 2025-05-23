% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boundsPredprob.R
\name{boundsPredprob}
\alias{boundsPredprob}
\title{Decision cutpoints for boundary (based on predictive probability) for Decision 1 rule.}
\usage{
boundsPredprob(
  looks,
  Nmax = max(looks),
  p0,
  tT,
  phiL,
  phiU,
  parE = c(1, 1),
  weights
)
}
\arguments{
\item{looks}{(\code{numeric}):\cr A vector of number of patients in each look.}

\item{Nmax}{(\code{number}):\cr maximum number of patients at the end of the trial in the \code{E} group.}

\item{p0}{(\code{number}):\cr lower Futility threshold of response rate.}

\item{tT}{(\code{number}):\cr threshold of which assumed \code{truep} exceeds acceptable threshold of \code{p0}.}

\item{phiL}{(\code{number}):\cr lower threshold on the predictive probability.}

\item{phiU}{(\code{number}):\cr upper threshold on the predictive probability.}

\item{parE}{(\code{numeric}):\cr the beta parameters matrix, with K rows and 2 columns,
corresponding to the beta parameters of the K components.}

\item{weights}{(\code{numeric}):\cr the mixture weights of the beta mixture prior.}
}
\value{
A matrix for each same size in \code{looks}. For each sample size, the following is returned:
\itemize{
\item \code{xL} : the maximum number of responses that meet the futility
threshold.
\item \code{pL} : response rate corresponding to \code{xL}.
\item \code{predL} : predictive probability corresponding to \code{xL}
\item \code{postL}: posterior probability corresponding to \code{xL}.
\item \code{pL_upper_ci} : upper bound of one sided 95\% CI for the response rate based on an
exact binomial test.
\item \code{xU} : the minimal number of responses that meet the efficacy threshold.
\item \code{pU} : response rate corresponding to \code{xU}.
\item \code{predU} : predictive probability corresponding to \code{xU}.
\item \code{postU}: posterior probability corresponding to \code{xU}.
\item \code{pU_lower_ci} : lower bound of one sided 95\% CI for the response rate based on exact
binomial test.
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

This function is used to identify the efficacy boundary and futility
boundary based on rules in @details.
}
\details{
see also \code{predprob()}
The following rules are Decision 1 rules:
Efficacy boundary: find minimum x (xU) where
Pr(Pr(RR > p0 | data) >= tT | x) >= phiU,
Futility boundary: find maximum x (xL) where
Pr(Pr(RR > p0 | data) >= tT | x) =< phiL
}
\examples{
# 40 pts trial with interim looks after each 10 patients.
# Final efficacy decision if more than 80\% probability to be above 20\% ORR,
# Final futility decision otherwise.
# Interim efficacy decision if more than 90\% predictive probability reach this or
# Efficacy look Pr(Pr(RR > p0 | x, Y) >= tT | x) >= phiU,
# Interim futility decision if less than 10\% predictive probability or
# Futility look Pr(Pr(RR > p0 | x, Y) >= tT | x) =< phiL
# Uniform prior (i.e. beta(1, 1)) on the ORR:
boundsPredprob(
  looks = c(10, 20, 30, 40),
  p0 = 0.20,
  tT = 0.80,
  phiL = 0.60,
  phiU = 0.90
)

# 25 pts trial with interim looks at 7 and 15 pts.
# Efficacy decision if more than 80\% probability to be above 20\% ORR,
# Final futility decision otherwise.
# Interim efficacy decision if more than 90\% predictive probability reach this or
# Efficacy look Pr(Pr(RR > p0 | x, Y) >= tT | x) >= phiU,
# Interim futility decision if less than 60\% predictive probability or
# Futility look Pr(Pr(RR > p0 | x, Y) >= tT | x) =< phi
# with mixed prior and weights:
boundsPredprob(
  looks = c(7, 15, 25),
  p0 = 0.20,
  tT = 0.80,
  phiL = 0.60,
  phiU = 0.90,
  parE = cbind(c(1, 1), c(3, 10)),
  weights = c(0.2, 0.8)
)
}
\keyword{graphics}
