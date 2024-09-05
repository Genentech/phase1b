# phase1b

<!-- markdownlint-disable -->

<img src="man/figures/hex_logo3.png" align = "right" alt="hex logo" style="display: inline-block; width:200px; margin: 0 auto auto auto;" />
<!-- markdownlint-enable -->

The phase1b package project is a a Bayesian approach to decision making
in early development clinical trials. As a background, the main purpose
of early trials is to determine whether a novel treatment demonstrates
sufficient safety and efficacy signals to warrant further investment
(Lee & Liu, 2008).

The new R package `phase1b` is a flexible toolkit that calculates many
properties to this end, especially in the oncology therapeutic area. The
primary focus of this package is on binary endpoints. The benefit of a
Bayesian approach is the possibility to account for prior data (Thall &
Simon, 1994) in that a new drug may have shown some signals of efficacy
owing to its proposed mode of action, or similar activity based on prior
data. The concept of the phase1b package is to evaluate the posterior
probability that the response rate with a novel drug is better than with
the current standard of care treatment in early phase trials such as
Phase I.

The phase1b package provides a facility for early development study
teams to decide on further development of a drug either through
designing for phase 2 or 3, or expanding current cohorts. The prior
distribution can incorporate any previous data via mixtures of beta
distributions. Furthermore, based on an assumed true response rate if
the novel drug was administered in the wider population, the package
calculates the frequentist probability that a current clinical trial
would be stopped for efficacy or futility conditional on true values of
the response, otherwise known as operating characteristics.

The intended user is the early clinical trial statistician in the design
and interim stage of their study and offers a flexible approach to
setting priors and weighting.

## Installation

### Development

You can install the development version of phase1b from
[GitHub](https://github.com/) with:

```r
devtools::install_github("https://github.com/Genentech/phase1b/", force = TRUE)
library(phase1b)
```

## Getting Started

An introductory vignette is currently being prepared. Use the help
function in your console to access the documentation.

## Citing `phase1b`

To cite `phase1b` please see
[here](https://genentech.github.io/phase1b/main/authors.html#citation).
