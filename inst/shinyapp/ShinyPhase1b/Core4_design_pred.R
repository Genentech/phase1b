
predlooks <- reactive({
  ilookE <- eval(parse(text = paste("c(", input$predlook1, ")", sep = "")))
  look_l <- unique(sort(c(ilookE, input$pred_n)))
  look_l[look_l <= input$pred_n]
})


output$ilook <- renderUI({
  if (input$designtype == "Predictive Probability Design") {
    fluidRow(
      selectInput("pred_selectlook", "select looks at:", predlooks(), selected = max(predlooks())),
      p(strong("Please enter the response outcome that you would like to report in the summary table/plot")),
      fluidRow(
        column(
          width = 2,
          numericInput("pred_resp_list1", "from",
            1,
            min = 0
          )
        ),
        column(
          width = 2,
          numericInput(
            inputId = "pred_resp_list2", label = "to",
            value = max(predlooks()), min = 0, step = 1
          )
        ),
        column(
          width = 2, offset = .5,
          numericInput("pred_resp_list3", "by",
            2,
            min = 0, step = 1, max = 5000
          )
        ),
        column(
          width = 3, offset = .5,
          textInput("pred_resp_list4", "Additional values", "2,4")
        ),
        column(
          width = 2, offset = .5,
          br(),
          actionButton("RunButton3", "Calculate!")
        )
      ),
      helpText("Calculation may take a while, please wait till the table and
                                 figure show up")
    ) # FluidRow end;
    #                 x <- as.numeric(input$pred_selectlook)
    #                 updateNumericInput(session, "pred_resp_list2", value=x,min=0, max =x)
  }
})

observeEvent(input$pred_selectlook, {
  x <- as.numeric(input$pred_selectlook)
  updateNumericInput(session, "pred_resp_list2", value = x, min = 0, max = x)
})

predT_list <- eventReactive(input$pred_selectlook, {
  plook_n <- as.numeric(input$pred_selectlook)

  presp_seq <- seq(from = input$pred_resp_list1, to = input$pred_resp_list2, by = input$pred_resp_list3)

  presp_other <- eval(parse(text = paste("c(", input$pred_resp_list4, ")", sep = "")))

  presp_all <- unique(sort(c(presp_seq, presp_other)))

  presp_all <- presp_all[presp_all <= plook_n]
})


predprobT <- eventReactive(input$pred_selectlook, {
  input$RunButton3

  pred_look_n <- as.numeric(input$pred_selectlook)

  predlook_list <- predT_list()

  predprobE <- apply(as.matrix(predlook_list), 1, function(i) {
    as.vector(predprobDist(
      x = i, n = pred_look_n,
      Nmax = input$pred_n,
      delta = input$cut_B_pred / 100, thetaT = input$pred_tU / 100,
      parE = c(input$alpha_trial1, input$beta_trial1),
      parS = c(input$alpha_con1, input$beta_con1)
    ))
  })


  predprobF <- apply(as.matrix(predlook_list), 1, function(i) {
    as.vector(1 - predprobDist(
      x = i, n = pred_look_n,
      Nmax = input$pred_n,
      delta = input$cut_W_pred / 100, thetaT = 1 - input$pred_tL / 100,
      parE = c(input$alpha_trial1, input$beta_trial1),
      parS = c(input$alpha_con1, input$beta_con1)
    ))
  })




  data_diff <- do.call(cbind, lapply(as.matrix(predlook_list), sumTable,
    TotalSample = pred_look_n, cut_B = input$cut_B_pred / 100,
    cut_W = input$cut_W_pred / 100,
    parX = c(input$alpha_con1, input$beta_con1),
    YPri = c(input$alpha_trial1, input$beta_trial1),
    Round = 4
  ))

  m <- data.frame(prob = predprobE, resp = predlook_list, diff = as.numeric(data_diff[3, ]), type = "Efficacy")
  m <- rbind(m, data.frame(prob = predprobF, resp = predlook_list, diff = as.numeric(data_diff[3, ]), type = "Futility"))
  # exam<<-m
  m
})



output$tbl3 <- renderTable({
  input$RunButton3

  isolate({
    atable <- predprobT()

    atable$prob <- round(atable$prob, 2)

    atable$diff <- round(atable$diff, 1)

    names(atable)[names(atable) == "prob"] <- "Resp"

    res <- reshape(atable, idvar = c("resp", "diff"), timevar = "type", direction = "wide")

    pred_look_n <- as.numeric(input$pred_selectlook)

    res$resp <- paste(res$resp, " (", round(res$resp / pred_look_n * 100, 1), ")", sep = "")
    res <- rbind(c(
      "Responses (Obs. rate [%])", "Estimated diff. [%]", "PP of go at the end",
      "PP of no go at the end"
    ), res)
    res <- as.data.frame(t(res))
    rownames(res) <- NULL
    colnames(res) <- NULL
    res
  })
})





output$tpout <- renderUI({
  if (input$designtype == "Predictive Probability Design") {
    tabsetPanel(
      tabPanel(
        "Table",
        h4("The summary table for selected outcomes"),
        uiOutput("SumUI3")
      ),
      tabPanel(
        "Plot",
        h4("The summary plot for go/no go decisions"),
        ggvisOutput("mplot2"),
        p(ggvisnote)
      )
    )
  }
})
output$SumUI3 <- renderUI({
  if (input$RunButton3 != 0) {
    tableOutput("tbl3")
  }
})
