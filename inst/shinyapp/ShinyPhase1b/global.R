library(shiny)
library(ggvis)
library(magrittr)
library(ggplot2)
library(ggvis)
library(rtable)
library(phase1b)
library(data.table)
library(tcltk)


mainDir <- getwd()
a <- file.path(mainDir)


helpPopup <- function(title, content,
                      placement = c("right", "top", "left", "bottom"),
                      trigger = c("click", "hover", "focus", "manual")) {
  tagList(
    singleton(
      tags$head(
        tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })")
      )
    ),
    tags$a(
      href = "#", class = "btn btn-mini", `data-toggle` = "popover",
      title = title, `data-content` = content, `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok = TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok = TRUE)[1],
      tags$i(class = "icon-question-sign")
    )
  )
}

ImproveLikelihood <- function(n, TargetPET_CR_delta = 0.2) {
  cbind(
    OR = c(0:n), PET_CR = c(round((0:n) / n * 100, 1)),
    P = as.data.frame(do.call(rbind, lapply(0:n, function(x) {
      round(postprobDist(x, n, xS = 0, nS = 0,
                         parS = c(beta_par$alpha, beta_par$beta), delta = TargetPET_CR_delta) * 100, 1)
    })))
  )
}

ggvisnote <- "note: due to a known issue of the ggvis tooltips, please move the mouse
closer to the linked points on the curve, in order to show the corresponding
values of them"

# based on the Shiny fileInput function
fileInput2 <- function(inputId, label = NULL, labelIcon = NULL, multiple = FALSE,
                       accept = NULL, width = NULL, progress = TRUE, ...) {
  # add class fileinput_2 defined in UI to hide the inputTag
  inputTag <- tags$input(
    id = inputId, name = inputId, type = "file",
    class = "fileinput_2"
  )
  if (multiple) {
    inputTag$attribs$multiple <- "multiple"
  }
  if (length(accept) > 0) {
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  }

  div(...,
    style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
    inputTag,
    # label customized with an action button
    tags$label(`for` = inputId, div(icon(labelIcon), label,
      class = "btn btn-default action-button"
    )),
    # optionally display a progress bar
    if (progress) {
      tags$div(
        id = paste(inputId, "_progress", sep = ""),
        class = "progress shiny-file-input-progress",
        tags$div(class = "progress-bar")
      )
    }
  )
}
