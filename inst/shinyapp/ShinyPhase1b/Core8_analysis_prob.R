output$selectlook2 <- renderUI({
  selectInput("post_selectlook2", "Select looks at:", postlooks(), selected = max(postlooks()))
})


output$post_con <- renderPlot({
  # if(input$update==FALSE){
  myPlot(
    alpha = input$alpha_con1,
    beta = input$beta_con1,
    xlab = "Response Rate",
    main = "Prior Distribution",
    col = "red",
    lwd = 3
  )
})



output$post_trial <- renderPlot({
  myPlot(
    alpha = input$resp + input$alpha_trial1,
    beta = as.numeric(input$post_selectlook2) - input$resp + input$beta_trial1,
    xlab = "Response Rate",
    main = "Posterior Distribution",
    col = "blue",
    lwd = 3
  )
})

output$compare <- renderPlot({
  myPlotDiff(
    parY = c(input$resp + input$alpha_trial1, as.numeric(input$post_selectlook2) - input$resp + input$beta_trial1),
    parX = c(input$alpha_con1, input$beta_con1), shade = 1,
    cut_B = input$cut_B / 100, cut_W = input$cut_W / 100,
    xlab = "(Combo Response) - (Control Response)",
    col = "black",
    lwd = 3
  )
  legend("topright", c("Prob. of No Go", "Prob. of Go"), pch = 15, col = c("red", "green"), bty = "n")
})
