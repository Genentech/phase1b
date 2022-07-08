
summaries <- do.call(cbind, lapply(c(0:8), sumTable, TotalSample = 25, parX = c(1, 52), cut_B = 0.2, cut_W = 0.05))

plotDecision(summaries, Pos_cut = 60, Neg_cut = 60)
