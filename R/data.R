#' Example of time to event dataset
#'
#' An example dataset containing oncology time to event endpoints
#' formatted for time to event analysis functions in the package
#'
#' @format A data frame with 80 obs. of  12 variables:
#' \describe{
#'   \item{ID}{Patient number formatted as USUBJID}
#'   \item{treatment}{Factor w/ 2 levels: "Control" and "Experimental"}
#'   \item{PFStime}{progression free surivival, days}
#'   \item{PFSevent}{censoring indicator for PFS}
#'   \item{DoR}{duration of response}
#'   \item{responder}{responder status, used as censoring variable for DoR}
#'   \item{TtR}{time to response}
#'   \item{TtRevent}{censoring indicator for TtR}
#'   \item{OStime}{survival time, days}
#'   \item{OSevent}{death indicator}
#'   \item{SEX}{covariate}
#'   \item{AGE}{covariate}
#' }
"TTEdata"

#' An example dataset formatted as subject level VAD
#'
#' @format A data frame with 80 obs. of  4 variables:
#' \describe{
#'   \item{SUBJID}{}
#'   \item{SEX}{}
#'   \item{AGE}{}
#'   \item{ARM}{}
#' }
"ASLdata"

#' An example dataset formatted as time to event efficacy VAD
#'
#' @format A data frame with 80 obs. of  4 variables:
#'
"ATEdata"
