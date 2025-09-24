# plotBetaDiff
test_that("plotBetaDiff works as expected", {
  parX <- c(1, 52) # prior  parameters of control or SOC
  parY <- c(5.5, 20.5) # prior  parameters of experimental arm
  result1 <- plotBetaDiff(
    parX = parX,
    parY = parY,
    go_cut = 0.3,
    stop_cut = 0.1, # below a difference of 10%, is an unsuccessful trial
    shade = TRUE,
    note = TRUE
  )
  result2 <- plotBetaDiff(
    parX = c(1, 1),
    parY = c(1, 1),
    go_cut = 0.3,
    stop_cut = 0.1, # below a difference of 10%, is an unsuccessful trial
    shade = TRUE,
    note = TRUE
  )
  vdiffr::expect_doppelganger(
    title = "Plot of plotBetaDiff with beta mixture",
    fig = result1
  )
  vdiffr::expect_doppelganger(
    title = "Plot of plotBetaDiff without beta mixture",
    fig = result2
  )
})
