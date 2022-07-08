
tabsetPanel(
  id = "binomial_tabset",
  tabPanel(
    title = "Prior",
    column(
      width = 6,
      h4("Historical Control"),
      p(),
      fluidRow(
        column(
          width = 4,
          numericInput(
            inputId = "alpha_con1",
            label = HTML("&alpha;"),
            value = 2, min = 0
          )
        ),
        column(
          width = 4, offset = 1,
          numericInput(
            inputId = "beta_con1",
            label = HTML("&beta;"),
            value = 8, min = 0
          )
        ),
        plotOutput("prior_con", height = "300px", width = "400px")
      )
    ),
    column(
      width = 6,
      h4("Combination Treatment Trial"),
      p(),
      fluidRow(
        column(
          width = 4,
          numericInput(
            inputId = "alpha_trial1",
            label = HTML("&alpha;"),
            value = 0.5, min = 0
          )
        ),
        column(
          width = 4, offset = 1,
          numericInput(
            inputId = "beta_trial1",
            label = HTML("&beta;"),
            value = 0.5, min = 0
          )
        )
      ),
      plotOutput("prior_trial", height = "300px", width = "400px")
    )
  ),
  tabPanel(
    title = "Design",
    fluidRow(
      sidebarPanel(
        selectizeInput(
          inputId = "designtype",
          label = h4("Select Design Type"),
          choices = c("Posterior Probability Design", "Predictive Probability Design"),
          selected = c("Posterior Probability Design")[1],
          multiple = FALSE
        ),
        conditionalPanel(
          condition = "input.designtype == 'Posterior Probability Design'",
          h4("phase 1B trial information"),
          numericInput(
            inputId = "post_n",
            label = "Total Number of Patients",
            value = 25,
            min = 0
          ),
          p(strong("Please enter the interim looks")),
          textInput("postlook1", "Interim looks at", "15,20"),
          p(strong("Improvement Threshold [%]")),
          fluidRow(
            column(
              width = 5,
              numericInput(
                inputId = "cut_B",
                label = "Efficacy",
                value = 15,
                min = 0, max = 100
              )
            ),
            column(
              width = 5,
              numericInput(
                inputId = "cut_W",
                label = "Futility",
                value = 5,
                min = 0, max = 100
              )
            )
          )
        ),
        conditionalPanel(
          condition = "input.designtype == 'Predictive Probability Design'",
          h4("phase 1B trial information"),
          numericInput(
            inputId = "pred_n",
            label = "Total Number of Patients",
            value = 25,
            min = 0
          ),
          p(strong("Please enter the interim looks")),
          textInput("predlook1", "Interim looks at", "15,20"),
          p(strong("Improvement Threshold [%]")),
          fluidRow(
            column(
              width = 5,
              numericInput(
                inputId = "cut_B_pred",
                label = "Efficacy",
                value = 15,
                min = 0, max = 100
              )
            ),
            column(
              width = 5,
              numericInput(
                inputId = "cut_W_pred",
                label = "Futility",
                value = 5,
                min = 0, max = 100
              )
            )
          ),
          p("Posterior probability cutoff to define a positive/negative trial result[%]"),
          fluidRow(
            column(
              width = 4,
              numericInput(
                inputId = "pred_tU",
                label = "Efficacy",
                value = 60,
                min = 0, max = 100
              )
            ),
            column(
              width = 4,
              numericInput(
                inputId = "pred_tL",
                label = "Futility",
                value = 60,
                min = 0, max = 100
              )
            )
          )
        )
      ),
      mainPanel(
        conditionalPanel(
          condition = "input.designtype == 'Posterior Probability Design'",
          fluidRow(
            uiOutput("selectlook1"),
            p(strong("Please enter the response outcome that you would like to report in the summary table/plot")),
            fluidRow(
              column(
                width = 2,
                numericInput("resp_list1", "from",
                  1,
                  min = 0
                )
              ),
              column(
                width = 2,
                numericInput(
                  inputId = "resp_list2", label = "to",
                  value = 25, min = 0, step = 1
                )
              ),
              column(
                width = 2, offset = .5,
                numericInput("resp_list3", "by",
                  2,
                  min = 0, step = 1, max = 5000
                )
              ),
              column(
                width = 3, offset = .5,
                textInput("resp_list4", "Additional values", "2,4")
              ),
              column(
                width = 2, offset = .5,
                br(),
                actionButton("RunButton1", "Calculate!")
              )
            ),
            helpText("Calculation may take a while, please wait till the table and
                                                 figure show up"),
            tabsetPanel(
              tabPanel(
                "Table",
                h4("The summary table for selected outcomes"),
                uiOutput("SumUI1")
              ),
              tabPanel(
                "Plot",
                h4("The posterior summary plot for go/no go decisions"),
                uiOutput("SumUI2"),
                p(ggvisnote)
              )
            )
          )
        ),
        uiOutput("ilook"),
        uiOutput("tpout")
      ) # end mainpanel
    )
  ),
  tabPanel(
    title = "OC Evaluation",
    conditionalPanel(
      condition = "input.designtype == 'Posterior Probability Design'",
      fluidRow(
        sidebarPanel(
          h4("Trial Simulation Parameters"),
          p("The range of true ps"),
          fluidRow(
            column(
              width = 4,
              numericInput("posttrue_p1", "from",
                0.1,
                min = 0, max = 1
              )
            ),
            column(
              width = 4, offset = .5,
              numericInput("posttrue_p2", "to",
                0.9,
                min = 0, max = 1
              )
            ),
            column(
              width = 4, offset = .5,
              numericInput("posttrue_p3", "by",
                0.2,
                min = 0, max = 1
              )
            )
          ),
          p("Posterior Probability Cutoff[%]"),
          fluidRow(
            column(
              width = 4,
              numericInput(
                inputId = "post_tU",
                label = "Efficacy",
                value = 60,
                min = 0, max = 100
              )
            ),
            column(
              width = 4,
              numericInput(
                inputId = "post_tL",
                label = "Futility",
                value = 60,
                min = 0, max = 100
              )
            )
          ),
          numericInput(
            inputId = "nsim",
            label = "Number of Simulation Runs",
            value = 2,
            min = 1
          ),
          numericInput(
            inputId = "postOCseed",
            label = "Simulation Seed",
            value = 123,
            min = 0
          ),
          actionButton("RunButton2", "Calculate!")
        ),
        helpText("Calculation may take a while, please wait till the table and
                                 figure show up"),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Table",
              h4("The summary table over True p"),
              uiOutput("PostOCUI1")
            ),
            tabPanel(
              "Plot1",
              h4("The summary plot for go/no go decisions"),
              uiOutput("PostOCUI2"),
              p(ggvisnote)
            ),
            tabPanel(
              "Plot2",
              h4("The cumulative probability plot for go/no go decisions"),
              plotOutput("ocpostplot2", height = "300px", width = "800px"),
              helpText("Thanks Francesca Michielin for the plot source code")
            )
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input.designtype == 'Predictive Probability Design'",
      fluidRow(
        sidebarPanel(
          h4("Trial Simulation Parameters"),
          p("The range of true ps"),
          fluidRow(
            column(
              width = 4,
              numericInput("predtrue_p1", "from",
                0.1,
                min = 0, max = 1
              )
            ),
            column(
              width = 4, offset = .5,
              numericInput("predtrue_p2", "to",
                0.9,
                min = 0, max = 1
              )
            ),
            column(
              width = 4, offset = .5,
              numericInput("predtrue_p3", "by",
                0.2,
                min = 0, max = 1
              )
            )
          ),
          p("Predictive Probability Cutoff[%]"),
          fluidRow(
            column(
              width = 4,
              numericInput(
                inputId = "pred_phiU",
                label = "Efficacy",
                value = 60,
                min = 0, max = 100
              )
            ),
            column(
              width = 4,
              numericInput(
                inputId = "pred_phiFu",
                label = "Futility",
                value = 60,
                min = 0, max = 100
              )
            )
          ),
          numericInput(
            inputId = "prednsim",
            label = "Number of Simulation Runs",
            value = 2,
            min = 1
          ),
          numericInput(
            inputId = "predOCseed",
            label = "Simulation Seed",
            value = 123,
            min = 0
          ),
          actionButton("RunButton4", "Calculate!")
        ),
        helpText("Calculation may take a while, please wait till the table and
                                         figure show up"),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Table",
              h4("The summary table over True p"),
              uiOutput("PredOCUI1")
            ),
            tabPanel(
              "Plot1",
              h4("The summary plot for go/no go decisions"),
              uiOutput("PredOCUI2"),
              p(ggvisnote)
            ),
            tabPanel(
              "Plot2",
              h4("The cumulative probability plot for go/no go decisions"),
              plotOutput("ocpredplot2", height = "300px", width = "800px"),
              helpText("Thanks Francesca Michielin for the plot source code")
            )
          )
        )
      )
    )
  ), # endtabPanel
  tabPanel(
    title = "Analysis",
    conditionalPanel(
      condition = "input.designtype == 'Posterior Probability Design'",
      sidebarPanel(
        uiOutput("selectlook2"),
        numericInput(
          inputId = "resp",
          label = "# of Responders",
          value = 1
        )
      ),
      mainPanel(
        column(
          width = 6,
          h4("Historical Control"),
          p(),
          plotOutput("post_con", height = "300px", width = "400px")
        ),
        column(
          width = 6,
          h4("Combination Treatment Trial"),
          p(),
          plotOutput("post_trial", height = "300px", width = "400px")
        ),
        fluidRow(
          align = "center",
          hr(),
          h4("Comparison of two posteriors"),
          plotOutput("compare", height = "300px", width = "1000px")
        )
      )
    ),
    uiOutput("ilook2"),
    uiOutput("ilook3")
  ) # endtabPanel
)
