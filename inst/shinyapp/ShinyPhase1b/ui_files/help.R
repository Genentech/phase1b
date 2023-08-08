div(
  class = "help-glossary-div",
  br(), br(),
  div(
    class = "help-glossary-nav-container",
    navlistPanel(
      well = TRUE,
      id = "help_navlist",
      "Topics",
      tabPanel(
        "Use the shiny app",
        div(
          class = "glossary-entry",
          h4("Users' guide"),
          p("This shiny app is designed to support decision making in phase1b trials."),
          p("Currently methods including posterior probability and predictive probability for
                        bionomial endpoints, such as ORR, CRR have been developed in the app."),
          h5("The binomial endpoints:"),
          p("The app is organized by four modules--Prior, Design, Operating character evaluation, Analysis."),
          p("Prior--Define a prior of the phase1b trial and a prior(=posterior) of the historical control"),
          p("Design--First select a type of design on the left panel of the window;
                        Second, enter the design parameters on the left panel;
                        Third, customize the output on the right hand side of the window"),
          p("OC evaluation--Evaluate the type 1 error and the power of the design"),
          p("Analysis--Detial illustrations on a single scenario of the trial outcome"),
          h5("Load/Save user input"),
          p("The user input of a trial design can be saved by clicking the 'Save inputs'
                        bottom on top right of each tab;
                        the design parameters can be reloaded by clicking the 'Load inputs'
                        bottom on the side of the 'Save inputs' bottom")
        )
      ),
      tabPanel(
        "New features",
        div(
          class = "glossary-entry",
          h4("New features"),
          p(
            "This version of shiny phase1b app includes following new features:"
          ),
          p("1) new structure of the user interface, classified by the type of endpoints"),
          p("2) operating character evaluation for designs guided by the posterior probability method"),
          p("3) a trial outcome overview based on the predictive probability method"),
          p("4) operating character evaluation for designs guided by the predictive probability method"),
          p("5) single trial analysis based on the predictive probability method"),
          p("6) allows upload/download user inputs"),
          br()
        )
      ),
      tabPanel(
        "Phase1b R package",
        div(
          class = "glossary-entry",
          h4("Phase1b R package"),
          p("The shiny phase1b toolkit is supported by the R package 'phase1b',
            which includes additional functionalities
            such as seting-up a mixsure prior distribution,
            excluding futility decision at early interim etc.")
        )
      ),
      tabPanel(
        "Feedback",
        div(
          class = "glossary-entry",
          h4("Useful links"),
          p(
            "Please file any bugs or questions on",
            a("github",
              href = "https://github.com/Genentech/phase1b/issues",
              target = "_blank"
            )
          ),
        )
      )
    ) # navlistPanel end
  ),
  br(), br()
)
