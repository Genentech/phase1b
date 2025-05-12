# sumTable ----
sumTable(
  thisResp = 10,
  TotalSample = 20,
  parX = c(1, 1),
  go_cut = 0.8,
  stop_cut = 0.4
)

summaries <- do.call(
  cbind,
  lapply(c(0:8),
    sumTable,
    TotalSample = 25,
    parX = c(1, 52),
    go_cut = 0.2,
    stop_cut = 0.05
  )
)

plotDecision(summaries, go_cut = 60, stop_cut = 60)

# plotting more results
summaries <- do.call(
  cbind,
  lapply(c(0:8),
    sumTable,
    TotalSample = 25,
    parX = c(1, 52),
    go_cut = 0.5,
    stop_cut = 0.2
  )
)

plotDecision(summaries, go_cut = 10, stop_cut = 2)
