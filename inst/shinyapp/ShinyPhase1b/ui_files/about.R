div(
  style = "text-align: center; margin-top: 100px;",
  phase1b_version(),
  logo_and_name2(),
  br(), br(),
  br(), br(),
  h6("Authors"),
  helpText(style = "font-size: 12px;", "Jiawen Zhu", br(), "Tony Pourmohamad", br(), "Daniel Sabanes Bove"),
  br(),
  h6(style = "font-size: 12px;", "Contributors"),
  helpText(style = "font-size: 12px;", includeHTML("html/contribs.html")),
  br(),
  h6("Shiny"),
  helpText(
    style = "font-size: 12px;",
    "Phase1b is powered by the",
    a(
      href = "http://shiny.rstudio.com",
      "Shiny web application framework"
    ),
    "(RStudio)"
  ),
  br()
)
