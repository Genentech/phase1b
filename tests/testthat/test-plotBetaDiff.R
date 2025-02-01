# plotBetaDiff
test_that("plotBetaDiff works as expected", {
  parX <- c(1, 52) # prior  parameters of experimental arm
  parY <- c(5.5, 20.5) # prior  parameters of control or SOC
  result1 <- plotBetaDiff(
    parE = parY,
    parS = parX,
    go_cut = 0.3,
    stop_cut = 0.1, # below a difference of 10%, is an unsuccessful trial
    shade = TRUE,
    note = TRUE
  )
  result2 <- plotBetaDiff(
    parE = c(1, 1), # prior  parameters for experimental arm
    parS = c(1, 1), # prior parameters for control or SOC arm
    go_cut = 0.3,
    stop_cut = 0.1, # below a difference of 10%, is an unsuccessful trial
    shade = TRUE,
    note = TRUE
  )
  vdiffr::expect_doppelganger("Plot of distibution of difference of two arms", result1)
  vdiffr::expect_doppelganger("Plot of distibution of difference of two arms with beta mixture", result2)
})
