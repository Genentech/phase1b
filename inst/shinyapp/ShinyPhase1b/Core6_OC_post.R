output$PostOCUI1 <- renderUI({
  if (input$RunButton2 != 0) {
    tableOutput("table2")
  }
})

output$PostOCUI2 <- renderUI({
  # if(input$RunButton2 ==0){ return(NULL)}else{
  ggvisOutput("main_plot3")
  # }
})


post_oc <- reactive({
  input$RunButton2
  isolate({
    looks <- postlooks()
    p_par <- seq(from = input$posttrue_p1, to = input$posttrue_p2, by = input$posttrue_p3)
    set.seed(input$postOCseed)

    Mysim <- sapply(p_par, function(x) {
      res <- ocPostprobDist(looks, x,
        deltaE = input$cut_B / 100, deltaF = -1 * input$cut_W / 100,
        relativeDelta = FALSE, tL = input$post_tL / 100, tU = input$post_tU / 100,
        parE = c(a = input$alpha_trial1, b = input$beta_trial1), parS = c(a = input$alpha_con1, b = input$beta_con1),
        ns = input$nsim, nr = FALSE, d = NULL
      )
      return(res$oc)
    })

    rownames(Mysim) <- c("ExpectedN", "PrStopEarly", "PrEarlyEff",
                         "PrEarlyFut", "PrEfficacy", "PrFutility", "PrGrayZone")
    Mysim
  })
})

post_oc_plot <- reactive({
  input$RunButton2
  isolate({
    Mysim <- post_oc()
    truep <- seq(from = input$posttrue_p1, to = input$posttrue_p2, by = input$posttrue_p3)
    peffbp <- data.frame(trueP = truep, prob = as.numeric(Mysim[5, ]), type = "Efficacy Stops", color = "green")
    pfutbp <- data.frame(trueP = truep, prob = as.numeric(Mysim[6, ]), type = "Futility Stops", color = "red")
    pgrabp <- data.frame(trueP = truep, prob = as.numeric(Mysim[7, ]), type = "Gray Zone", color = "gray")

    res <- rbind(peffbp, pfutbp, pgrabp)
    res$type <- factor(res$type, levels = c("Gray Zone", "Futility Stops", "Efficacy Stops"))

    res
  })
})

postprobOC_tooltip <- function(x) {
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
  all_table <- post_oc_plot()

  postprobTable <- all_table[all_table$trueP == x$trueP & abs(all_table$prob - x$prob) < 0.0001, ]

  paste0(
    "<b> Response=", postprobTable$trueP,
    "</b><br><b> Prob for ", postprobTable$type, ":</b><br>",
    round(postprobTable$prob, 2), "<br>"
    #                ,
    #                "$", format(movie$BoxOffice, big.mark = ",", scientific = FALSE)
  )
}

vis3 <- reactive({
  xvar_name <- "Assumed true response rate"
  yvar_name <- "Chance of stop"


  post_oc_plot %>%
    ggvis(x = ~trueP, y = ~prob, stroke = ~type) %>%
    add_tooltip(postprobOC_tooltip, "hover") %>%
    add_axis("x", title = xvar_name) %>%
    add_axis("y", title = yvar_name) %>%
    group_by(type) %>%
    layer_points(size = 3, fill = ~type, opacity := 2) %>%
    layer_paths(stroke = ~type, strokeWidth := 2) %>%
    set_options(width = 650, height = 280)
})

vis3 %>% bind_shiny("main_plot3")

output$table2 <- renderTable({
  input$RunButton2
  isolate({
    post_table2 <- cbind(TrueP = seq(from = input$posttrue_p1,
                                     to = input$posttrue_p2, by = input$posttrue_p3), t(post_oc()))
    as.data.frame(post_table2)
  })
})


output$ocpostplot2 <- renderPlot({
  Mysim <- post_oc()
  p <- seq(from = input$posttrue_p1, to = input$posttrue_p2, by = input$posttrue_p3)
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
