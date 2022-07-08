
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
  #         test1<<-input$ppresp
  #         test2<<-input$ppselectlook2
  #         test3<<-input$cut_B_pred/100
  #         test4<<-input$pred_tU/100
  #         test5<<-c(a = input$alpha_trial1, b = input$beta_trial1)
  #         test6<<-c(a = input$alpha_con1, b = input$beta_con1)

  pplooks <- predlooks()
  ppU <- predprobDist(
    x = input$ppresp, n = as.numeric(input$ppselectlook2), Nmax = max(pplooks),
    delta = input$cut_B_pred / 100, thetaT = input$pred_tU / 100,
    parE = c(a = input$alpha_trial1, b = input$beta_trial1),
    parS = c(a = input$alpha_con1, b = input$beta_con1)
  )

  ppU <- as.data.frame(attr(ppU, "tables"))
  # ppU$type<-"Efficacy"

  ppL <- predprobDist(
    x = input$ppresp, n = as.numeric(input$ppselectlook2), Nmax = max(pplooks),
    delta = input$cut_W_pred / 100, thetaT = 1 - input$pred_tL / 100,
    parE = c(a = input$alpha_trial1, b = input$beta_trial1),
    parS = c(a = input$alpha_con1, b = input$beta_con1)
  )
  ppL <- as.data.frame(attr(ppL, "tables"))
  # ppL$b<-1-ppL$b
  # ppL$bgttheta<-as.numeric((1-ppL$bgttheta)>=input$pred_tL/100)
  # ppL$type<-"Futility"

  # ppRes<-rbind(ppU,ppL)

  # ppRes$ORR<-(ppRes$i+input$ppresp)/max(pplooks)

  ppRes <- data.frame(
    i = ppU[, 1], ORR = (ppU[, 1] + input$ppresp) / max(pplooks), py = ppU[, 2], bE = ppU[, 3], bET = ppU[, 4],
    bF = 1 - ppL[, 3], bFT = as.numeric((1 - ppL[, 3]) >= input$pred_tL / 100)
  )


  ppRes
})

output$ppAnalysis <- renderPlot({
  use <- pp_1case()

  # graphics::par(mar=c(5,15,5,15)+.1)

  # xticks <- use$i[use$i%%2==0]
  cols <- ifelse(use$bET == 1, "green", ifelse(use$bFT == 1, "red", "gray"))

  boxp <- barplot(use$py,
    col = cols, names.arg = use$i + input$ppresp,
    xlab = "# response at the end of the trial",
    ylab = "PP of a single event"
  )
  #         with(use,
  #              graphics::plot(x=i,
  #                             y=py,
  #                             ylab="",
  #                             xaxt="n",
  #                             #yaxt="n",
  #                             type="s",
  #                             xaxs="i",
  #                             yaxs="i"))
  #
  #         graphics::axis(side=1, at=xticks,
  #                        labels=xticks)

  graphics::axis(
    side = 3, at = boxp,
    labels =
      paste(round(use$ORR * 100, 0), "%", sep = "")
  )
  mtext("ORR at the end of the trial", side = 3, line = 2)
  box()
  ## now color the go / stop prob areas

  ## first stop:
  #         stopGrid <- use$i[use$bFT==1]
  #         nStop <- length(stopGrid)
  #         stopGrid2<-rep(c(stopGrid,max(stopGrid)+1),each=2)[-1]
  #
  #         graphics::polygon(x=
  #                                   c(stopGrid,
  #                                     rev(stopGrid2)),
  #                           y=
  #                                   c(rep(0, nStop+1),
  #                                     use$py[rev(rep((1:dim(use)[1])[use$bFT==1],each=2))]),
  #                           col="red",border=NA)
  #
  A_value <- sum(use$py[use$bFT == 1])
  graphics::mtext(paste("PP(no go)=", sprintf("%1.2f%%", 100 * as.numeric(A_value)), sep = ""), side = 1, line = 2, adj = 0, cex = 1, col = "red")


  ## then go:
  #
  # stopGrid <- use$i[use$bFT==1]
  # nStop <- length(stopGrid)
  # stopGrid2<-rep(c(stopGrid,max(stopGrid)+1),each=2)[-1]
  #
  # graphics::polygon(x=
  #                           c(stopGrid,
  #                             rev(stopGrid2)),
  #                   y=
  #                           c(rep(0, nStop+1),
  #                             use$py[rev(rep((1:dim(use)[1])[use$bFT==1],each=2))]),
  #                   col="red",border=NA)

  #         goGrid <- use$i[use$bET==1]
  #         nGo <- length(goGrid)
  #         goGrid2<-rep(c(min(goGrid)-1,goGrid),each=2)
  #
  #         graphics::polygon(x=
  #                                   c(goGrid,
  #                                     rev(goGrid2)[-1]),
  #                           y=
  #                                   c(rep(0, nGo),
  #                                     use$py[rev(rep((1:dim(use)[1])[use$bET==1],each=2))],0),
  #
  #                           col="green")
  #
  B_value <- sum(use$py[use$bET == 1])


  graphics::mtext(paste(sprintf("%1.2f%%", 100 * as.numeric(B_value)), "=PP(go)", sep = ""), side = 1, line = 2, adj = 1, cex = 1, col = "green")
})
