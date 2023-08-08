postlooks <- reactive({
  interimlookE <- eval(parse(text = paste("c(", input$postlook1, ")", sep = "")))
  look_list <- unique(sort(c(interimlookE, input$post_n)))
  look_list[look_list <= input$post_n]
})

output$selectlook1 <- renderUI({
  selectInput("post_selectlook1", "select looks at:", postlooks(), selected = max(postlooks()))
})




output$SumUI1 <- renderUI({
  if (input$RunButton1 > 0) {
    tableOutput("table")
  }
})


output$SumUI2 <- renderUI({
  ggvisOutput("main_plot1")
})


observeEvent(input$post_selectlook1, {
  x <- as.numeric(input$post_selectlook1)
  updateNumericInput(session, "resp_list2", value = x, min = 0, max = x)
})


design_table_list <- eventReactive(input$post_selectlook1, {
  look_n <- as.numeric(input$post_selectlook1)

  resp_seq <- seq(from = input$resp_list1, to = input$resp_list2, by = input$resp_list3)

  resp_other <- eval(parse(text = paste("c(", input$resp_list4, ")", sep = "")))

  resp_all <- unique(sort(c(resp_seq, resp_other)))

  resp_all <- resp_all[resp_all <= look_n]
})


post_table_data <- eventReactive(input$post_selectlook1, {
  post_look_n <- as.numeric(input$post_selectlook1)

  resp_list_all <- design_table_list()

  data <- do.call(cbind, lapply(resp_list_all, sumTable,
    TotalSample = post_look_n, cut_B = input$cut_B / 100, cut_W = input$cut_W / 100,
    parX = c(input$alpha_con1, input$beta_con1),
    YPri = c(input$alpha_trial1, input$beta_trial1),
    Round = 4
  ))
  data
})

output$table <- renderTable({
  input$RunButton1

  isolate({
    post_table <- post_table_data()

    STable <- post_table[, post_table["# resp", ] == as.integer(post_table["# resp", ])]

    STable <- round(STable, 1)

    STable["# resp", ] <- paste(as.integer(STable["# resp", ]), " (", STable["obs ORR [%]", ], ")", sep = "")

    STable <- STable[c("# resp", "mode [%]", "prob.go [%]", "prob.nogo [%]"), ]

    STable <- cbind(c(
      "Responses (Obs. rate [%])", "Estimated diff. [%]",
      paste("Chance of a > ", input$cut_B, "% improvement [%]", sep = ""),
      paste("Chance of a < ", input$cut_W, "% improvement [%]", sep = "")
    ), STable)

    colnames(STable) <- NULL
    rownames(STable) <- NULL

    as.data.frame(STable)
  })
})
