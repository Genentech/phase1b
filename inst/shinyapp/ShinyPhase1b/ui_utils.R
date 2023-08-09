source_ui <- function(...) {
  source(
    file.path("ui_files", ...),
    local = TRUE
  )$value
}

phase1b_version <- function() {
  # prevents error when deployed to shinyapps.io
  ver <- "1.0.0"
  strong(paste("Version", ver))
}

logo_and_name <- function() {
  div()
}

logo_and_name2 <- function() {
  div()
}
