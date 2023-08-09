navbarMenu(
  title = "More",

  #### about ####
  tabPanel(
    title = "About",
    div(
      style = "margin-top: 75px;",
      source_ui("about.R")
    )
  ),

  #### help ####
  tabPanel(
    title = "Help",
    h1(style = "text-align: center;", "Help"),
    source_ui("help.R")
  )
)
