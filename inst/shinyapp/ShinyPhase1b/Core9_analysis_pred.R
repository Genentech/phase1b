output$ilook2 <- renderUI({
  if (input$designtype == "Predictive Probability Design") {
    sidebarPanel(
      selectInput("ppselectlook2", "Select looks at:", predlooks(), selected = min(predlooks())),
      numericInput(
        inputId = "ppresp",
        label = "# of Responders",
        value = 1
      )
      # sliderInput("ppresp", "# of Responders",
      #             min=0, max=as.numeric(input$ppselectlook2), value=1,step= 1)
    )
  }
})
output$ilook3 <- renderUI({
  if (input$designtype == "Predictive Probability Design") {
    mainPanel(
      plotOutput("ppAnalysis", height = "300px", width = "600px")
    )
  }
})



pp_1case <- reactive({

  pplooks <- predlooks()
  ppU <- predprobDist(
    x = input$ppresp, n = as.numeric(input$ppselectlook2), Nmax = max(pplooks),
    delta = input$cut_B_pred / 100, thetaT = input$pred_tU / 100,
    parE = c(a = input$alpha_trial1, b = input$beta_trial1),
    parS = c(a = input$alpha_con1, b = input$beta_con1)
  )

  ppU <- as.data.frame(attr(ppU, "tables"))

  ppL <- predprobDist(
    x = input$ppresp, n = as.numeric(input$ppselectlook2), Nmax = max(pplooks),
    delta = input$cut_W_pred / 100, thetaT = 1 - input$pred_tL / 100,
    parE = c(a = input$alpha_trial1, b = input$beta_trial1),
    parS = c(a = input$alpha_con1, b = input$beta_con1)
  )
  ppL <- as.data.frame(attr(ppL, "tables"))

  ppRes <- data.frame(
    i = ppU[, 1], ORR = (ppU[, 1] + input$ppresp) / max(pplooks), py = ppU[, 2], bE = ppU[, 3], bET = ppU[, 4],
    bF = 1 - ppL[, 3], bFT = as.numeric((1 - ppL[, 3]) >= input$pred_tL / 100)
  )


  ppRes
})

output$ppAnalysis <- renderPlot({
  use <- pp_1case()

  cols <- ifelse(use$bET == 1, "green", ifelse(use$bFT == 1, "red", "gray"))

  boxp <- barplot(use$py,
    col = cols, names.arg = use$i + input$ppresp,
    xlab = "# response at the end of the trial",
    ylab = "PP of a single event"
  )

  graphics::axis(
    side = 3, at = boxp,
    labels =
      paste(round(use$ORR * 100, 0), "%", sep = "")
  )
  mtext("ORR at the end of the trial", side = 3, line = 2)
  box()
  ## now color the go / stop prob areas

  A_value <- sum(use$py[use$bFT == 1])
  graphics::mtext(paste("PP(no go)=",
                        sprintf("%1.2f%%", 100 * as.numeric(A_value)), sep = ""),
                  side = 1, line = 2, adj = 0, cex = 1, col = "red")


  B_value <- sum(use$py[use$bET == 1])


  graphics::mtext(paste(sprintf("%1.2f%%", 100 * as.numeric(B_value)), "=PP(go)", sep = ""),
                  side = 1, line = 2, adj = 1, cex = 1, col = "green")
})
