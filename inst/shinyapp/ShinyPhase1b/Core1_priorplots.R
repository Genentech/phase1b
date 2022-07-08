

output$prior_con <- renderPlot({
  myPlot(
    alpha = input$alpha_con1,
    beta = input$beta_con1,
    xlab = "Response Rate",
    main = "Prior Distribution",
    col = "red",
    lwd = 3
  )
})


output$prior_trial <- renderPlot({
  myPlot(
    alpha = input$alpha_trial1,
    beta = input$beta_trial1,
    xlab = "Response Rate",
    main = "Prior Distribution",
    col = "blue",
    lwd = 3
  )
})
