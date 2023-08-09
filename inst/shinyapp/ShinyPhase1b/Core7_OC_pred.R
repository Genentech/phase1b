output$PredOCUI1 <- renderUI({
  if (input$RunButton4 != 0) {
    tableOutput("predOCtable")
  }
})

output$PredOCUI2 <- renderUI({
  # if(input$RunButton2 ==0){ return(NULL)}else{
  ggvisOutput("predOCplot")
  # }
})


pred_oc <- reactive({
  input$RunButton4
  isolate({
    pplooks <- predlooks()
    ppp_par <- seq(from = input$predtrue_p1, to = input$predtrue_p2, by = input$predtrue_p3)
    set.seed(input$predOCseed)

    ppMysim <- sapply(ppp_par, function(x) {
      res <- ocPredprobDist(pplooks, x,
        delta = input$cut_B_pred / 100, deltaFu = input$cut_W_pred / 100,
        relativeDelta = FALSE, tT = input$pred_tU / 100, tFu = input$pred_tL / 100,
        phiFu = input$pred_phiFu / 100, phiU = input$pred_phiU / 100,
        parE = c(a = input$alpha_trial1, b = input$beta_trial1), parS = c(a = input$alpha_con1, b = input$beta_con1),
        ns = input$prednsim, nr = FALSE, d = NULL
      )
      return(res$oc)
    })

    rownames(ppMysim) <- c(
      "ExpectedN", "PrStopEarly", "PrEarlyEff",
      "PrEarlyFut", "PrEfficacy", "PrFutility", "PrGrayZone"
    )
    ppMysim
  })
})

pred_oc_plot <- reactive({
  input$RunButton4
  isolate({
    ppMysim <- pred_oc()
    pptruep <- seq(from = input$predtrue_p1, to = input$predtrue_p2, by = input$predtrue_p3)
    ppeffbp <- data.frame(trueP = pptruep, prob = as.numeric(ppMysim[5, ]), type = "Efficacy Stops", color = "green")
    ppfutbp <- data.frame(trueP = pptruep, prob = as.numeric(ppMysim[6, ]), type = "Futility Stops", color = "red")
    ppgrabp <- data.frame(trueP = pptruep, prob = as.numeric(ppMysim[7, ]), type = "Gray Zone", color = "gray")

    ppres <- rbind(ppeffbp, ppfutbp, ppgrabp)
    ppres$type <- factor(ppres$type, levels = c("Gray Zone", "Futility Stops", "Efficacy Stops"))

    ppres
  })
})



predprobOC_tooltip <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.null(x$prob)) {
    return(NULL)
  }
  if (x$prob == 0 && x$trueP == 0) {
    return(NULL)
  }

  # Pick out the movie with this ID
  ppall_table <- pred_oc_plot()

  predprobTable <- ppall_table[ppall_table$trueP == x$trueP & abs(ppall_table$prob - x$prob) < 0.0001, ]

  paste0(
    "<b> Response=", predprobTable$trueP,
    "</b><br><b> Prob for ", predprobTable$type, ":</b><br>",
    round(predprobTable$prob, 2), "<br>"
    #                ,
    #                "$", format(movie$BoxOffice, big.mark = ",", scientific = FALSE)
  )
}

ppOCplot <- reactive({
  xvar_name <- "Assumed true response rate"
  yvar_name <- "Chance of stop"


  pred_oc_plot %>%
    ggvis(x = ~trueP, y = ~prob, stroke = ~type) %>%
    add_tooltip(predprobOC_tooltip, "hover") %>%
    add_axis("x", title = xvar_name) %>%
    add_axis("y", title = yvar_name) %>%
    group_by(type) %>%
    layer_points(size = 3, fill = ~type, opacity := 2) %>%
    layer_paths(stroke = ~type, strokeWidth := 2) %>%
    set_options(width = 650, height = 280)
})

ppOCplot %>% bind_shiny("predOCplot")

output$predOCtable <- renderTable({
  input$RunButton4
  isolate({
    pred_table2 <- cbind(TrueP = seq(
      from = input$predtrue_p1,
      to = input$predtrue_p2, by = input$predtrue_p3
    ), t(pred_oc()))
    as.data.frame(pred_table2)
  })
})




output$ocpredplot2 <- renderPlot({
  Mysim <- pred_oc()
  p <- seq(from = input$predtrue_p1, to = input$predtrue_p2, by = input$predtrue_p3)
  dec.GO <- as.numeric(Mysim[5, ])
  dec.EVAL2 <- as.numeric(Mysim[6, ])
  dec.STOP2 <- as.numeric(Mysim[7, ])

  polygon1.y <- dec.STOP2 + dec.EVAL2 + dec.GO
  polygon3.y <- dec.STOP2 + dec.EVAL2
  polygon4.y <- dec.STOP2


  plot(p, dec.GO,
    type = "n", main = "Cumulative probability of various decisions",
    xlab = "True ORR", ylab = "Probability"
  )

  polygon(
    x = c(
      p,
      rev(p)
    ),
    y = c(polygon1.y, rep(0, length(p))),
    col = "darkgreen"
  )

  polygon(
    x = c(
      p,
      rev(p)
    ),
    y = c(polygon3.y, rep(0, length(p))),
    col = "gold"
  )
  polygon(
    x = c(
      p,
      rev(p)
    ),
    y = c(polygon4.y, rep(0, length(p))),
    col = "red"
  )
})
