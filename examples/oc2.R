# this looks OK now:
oc2(
  method = "Prior.PP",
  histSize = 1e5L,
  trialSize = 100,
  drift = 0,
  controlRate = 0.5,
  nmeRate = 0.70,
  nSim = 20,
  delta = 0.15
)

## this one as well:
oc2(
  method = "Prior.Bayes",
  histSize = 1e6L,
  trialSize = 100,
  drift = 0,
  controlRate = 0.5,
  nmeRate = 0.65,
  nSim = 20,
  delta = 0.15
)

## this example takes longer to run, therefore we don't execute it here:
if (FALSE) {
  ## the different methods we are looking at:
  methods <- c(
    "PointMass.Bayes", "Prior.Bayes", "RCT.Bayes", "RCTvanilla.Bayes",
    "PointMass.PP", "Prior.PP", "RCT.PP", "RCTvanilla.PP"
  )

  ## the control rate will always be 50%
  ## but the historical data will be generated randomly from that
  ## binomial distribution!

  ## delta will be 0.15

  ## 200 simulations

  ## the different prior sample sizes (historical data size, histSizes <- c(10, 20, 50, 150))
  ##
  ## later: add very large size, but with heterogeneity =>
  ## mixture of beta priors
  histSizes <- c(20, 150)

  ## the trial sample size (total)
  trialSizes <- c(40)

  ## the drift
  drift <- c(0, 0.1, 0.2)

  ## the different true rates for the NME: nmeRates <- seq(from=0.05, to=0.95, by=0.05)
  ##
  nmeRates <- c(0.4, 0.5, 0.6, 0.7, 0.8)

  ## so the whole grid is
  wholeGrid <- expand.grid(
    method = methods,
    drift = drift,
    histSize = histSizes,
    trialSize = trialSizes,
    nmeRate = nmeRates, stringsAsFactors = FALSE
  )

  ## summarize the methods

  ## compute the operating characteristics for all combinations
  if (file.exists(savefile <- "allSims4.RData")) {
    load(savefile)
  } else {
    ## setup the result list
    allOcs <- vector(
      mode = "list",
      length = nrow(wholeGrid)
    )

    for (i in seq_len(nrow(wholeGrid))) {
      set.seed(i)
      allOcs[[i]] <- with(
        wholeGrid[i, ],
        oc2(
          method = method,
          histSize = histSize,
          trialSize = trialSize,
          drift = drift,
          controlRate = 0.5,
          nmeRate = nmeRate,
          nSim = 200,
          delta = 0.15
        )
      )
    }

    save(allOcs,
      file = savefile
    )
  }

  length(allOcs)
  allOcs

  allOcs[[1]]

  allPossibleNames <- colnames(allOcs[[1]])
  allPossibleNames

  allOcsMatrix <-
    t(sapply(
      allOcs,
      function(x) {
        res <- rep(NA, length(allPossibleNames))
        names(res) <- allPossibleNames
        res[colnames(x)] <- x
        res
      }
    ))


  allOcsMatrix
  wholeMatrix <- cbind(wholeGrid, allOcsMatrix)

  library(reshape)


  ## write a table to a Word file in the current directory
  outputTab <- function(tab,
                        out = "output.docx", # name of the output file
                        # how many digits for numbers?
                        digits = 2) {
    library(officer)
    doc <- read_docx()
    dat <- as.data.frame(tab)
    whichNum <- which(sapply(dat, is.numeric))
    whichInt <- which(sapply(dat, function(x) {
      is.numeric(x) && all(x == round(x))
    }))
    for (i in whichNum) {
      if (i %in% whichInt) {
        dat[[i]] <- format(dat[[i]], nsmall = 0)
      } else {
        dat[[i]] <- format(dat[[i]], digits = digits, nsmall = digits)
      }
    }

    doc <- body_add_table(doc, dat, style = "Normal Table")
    print(doc, target = paste0(getwd(), "/myreport.docx"))
  }


  ## --------------------------------------------------
  ## derive the tables
  ## (here change drift and histSize values manually to get the
  ## different tables for the slides)
  tab1 <- subset(
    wholeMatrix,
    (drift == 0.2) &
      (histSize == 150) &
      (method %in% ppNames)
  )
}
