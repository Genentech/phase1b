post_plot <- eventReactive(input$post_selectlook1, {
  post_table <- post_table_data()

  go <- data.frame(resp = as.numeric(post_table[1, ]),
                   prob = as.numeric(post_table[6, ]),
                   type = "Efficacy")
  # number of response,posterior prob of go;

  nogo <- data.frame(resp = as.numeric(post_table[1, ]),
                     prob = as.numeric(post_table[7, ]),
                     type = "Futility")
  # number of response,posterior prob of no go;

  post_plot <- rbind(go, nogo)

  post_plot
  # }
})
postprobTable_tooltip <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.null(x$prob)) {
    return(NULL)
  }
  if (x$prob == 0 && x$resp == 0) {
    return(NULL)
  }

  # Pick out the movie with this ID
  all_table <- post_plot()

  postprobTable <- all_table[all_table$resp == x$resp & abs(all_table$prob - x$prob) < 0.0001, ]

  paste0(
    "<b> Response=", postprobTable$resp,
    "</b><br><b> Posterior Prob for ", postprobTable$type, ":</b><br>",
    round(postprobTable$prob, 2), "<br>"
    #                ,
    #                "$", format(movie$BoxOffice, big.mark = ",", scientific = FALSE)
  )
}



vis2 <- reactive({
  input$RunButton1

  isolate({
    # Lables for axes
    xvar_name <- "# of response"
    yvar_name <- "Posterior probability"

    # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
    # but since the inputs are strings, we need to do a little more work.



    post_plot %>%
      ggvis(x = ~resp, y = ~prob, stroke = ~type) %>%
      #                 layer_points(size := 50, size.hover := 200,
      #                              fillOpacity := 0.2, fillOpacity.hover := 0.5,
      #                              stroke = ~has_oscar, key := ~ID) %>%
      add_tooltip(postprobTable_tooltip, "hover") %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name) %>%
      # add_legend("stroke", title = "Won Oscar", values = c("Yes", "No")) %>%
      #                 scale_nominal("stroke", domain = c("Yes", "No"),
      #                               range = c("orange", "#aaa")) %>%
      group_by(type) %>%
      layer_points(size = 3, fill = ~type, opacity := 2) %>%
      layer_paths(stroke = ~type, strokeWidth := 2) %>%
      set_options(width = 650, height = 280)
    # }
  })
})
observeEvent(input$RunButton1, {
  vis2 %>% bind_shiny("main_plot1")
})
