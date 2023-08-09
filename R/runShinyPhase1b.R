##' Launch the ShinyPhase1b app
##'
##' Launch the ShinyPhase1b app in the default web browser.
##'
##' @export

runShinyPhase1b <- function() {
  appDir <- system.file("shinyapp", "ShinyPhase1b", package = "phase1b")
  if (appDir == "") {
    stop("Could not find phase1b app directory. Try re-installing `phase1b`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}




# reference https://www.r-bloggers.com/supplementing-your-r-package-with-a-shiny-app-2/;
