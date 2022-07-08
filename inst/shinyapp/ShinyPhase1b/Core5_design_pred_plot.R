
ppTable_tooltip <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.null(x$prob)) {
    return(NULL)
  }
  if (x$prob == 0 & x$resp == 0) {
    return(NULL)
  }

  # Pick out the movie with this ID
  altable <- predprobT() # isolate(predprobT())

  predprobTable <- altable[altable$resp == x$resp & abs(altable$prob - x$prob) < 0.0001, ]

  paste0(
    "<b> Response=", predprobTable$resp,
    "</b><br><b> Predictive Prob for ", predprobTable$type, ":</b><br>",
    round(predprobTable$prob, 2), "<br>"
    #                ,
    #                "$", format(movie$BoxOffice, big.mark = ",", scientific = FALSE)
  )
}


# A reactive expression with the ggvis plot
gvis <- eventReactive(input$pred_selectlook, {

  # Lables for axes
  xvar_name <- "# of response"
  yvar_name <- "Predictive probability"

  # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
  # but since the inputs are strings, we need to do a little more work.

  predprobT %>%
    ggvis(x = ~resp, y = ~prob, stroke = ~type) %>%
    add_tooltip(ppTable_tooltip, "hover") %>%
    add_axis("x", title = xvar_name) %>%
    add_axis("y", title = yvar_name) %>%
    group_by(type) %>%
    layer_points(size = 3, fill = ~type, opacity := 2) %>%
    layer_paths(stroke = ~type, strokeWidth := 2) %>%
    set_options(width = 500, height = 500)
})

observeEvent(input$RunButton3, {
  gvis %>% bind_shiny("mplot2")
})
