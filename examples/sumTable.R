sumTable(2, 25, parX = c(1, 52), go_cut = 0.2, stop_cut = 0.05, YPri = c(1, 1))

# for multiple response scenarios (e.g. 0 to 8 responses out of 25)
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
summaries
