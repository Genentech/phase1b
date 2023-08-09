source("ui_utils.R", local = TRUE)

library(shiny)

shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  includeCSS("css/ShinyPh1b.css"),
  navbarPage(
    title = NULL,
    id = "nav",
    position = "fixed-top",
    collapsible = TRUE,
    theme = shinythemes::shinytheme("flatly"),
    windowTitle = "phase1b",

    #### HOME ####
    tabPanel(
      title = strong(style = "color: #B2011D;", "phase 1b"),
      value = "home",
      source_ui("PAGE_home.R")
    ),

    #### Binomial ####
    tabPanel(
      title = "Binomial",

      # define class fileinput_2 to hide inputTag in fileInput2
      tags$head(tags$style(HTML(
        ".fileinput_2 {
                                    width: 0.1px;
                                    height: 0.1px;
                                    opacity: 0;
                                    overflow: hidden;
                                    }"
      ))),
      div(
        style = "position:absolute;top:2.5em;right:1em;",
        fileInput2("inputfile", "Load Inputs",
          labelIcon = "folder-open-o",
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          ), progress = FALSE
        ),
        downloadButton("save_inputs", " Save Inputs",
          class = "butt"
        ),
        tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: white;}"))
      ),
      div(
        style = "margin-top: 10px;",
        source_ui("PAGE_binomial.R")
      )
    ),

    #### TBD ####
    tabPanel(
      title = "EndpointTBD1",
      source_ui("PAGE_TBD.R")
    ),

    #### TDB ####
    tabPanel(
      title = "EndpointTBD2",
      source_ui("PAGE_TBD.R")
    ),

    #### More ####
    source_ui("PAGE_more_menu.R")
  ) # End navbarPage
))
