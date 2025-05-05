# sumTable ----
sumTable(
  thisResp = 10,
  TotalSample = 20,
  parX = c(1, 1),
  go_cut = 0.8,
  stop_cut = 0.4
)

# plotting more results
summaries <- do.call(
  cbind,
  lapply(c(0:8),
    sumTable,
    TotalSample = 25,
    parX = c(1, 52),
    go_cut = 0.6,
    stop_cut = 0.2
  )
)

# plotDecision(summaries, Pos_cut = 60, Neg_cut = 60)
