# plotBetaDiff
test_that("plotBetaDiff works as expected", {
  result <- plotBetaDiff(
    parY = c(1, 1),
    parX = c(6, 10),
    Go_cut = 0.3,
    Stop_cut = 0.1, # below a difference of 10%, is an unsuccesful trial
    shade = TRUE,
    note = TRUE
  )
  result <- plotBetaDiff(
    parY = c(2, 3),
    parX = c(4, 10),
    Go_cut = 0.3,
    Stop_cut = 0.1, # below a difference of 10%, is an unsuccesful trial
    shade = TRUE,
    note = TRUE
  )
  vdiffr::expect_doppelganger("", result)
  vdiffr::expect_doppelganger("", result)
})
