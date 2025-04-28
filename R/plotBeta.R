#' Plot the Beta distribution
#'
#' This function will plot a PDF of a beta distribution
#'
#' @typed alpha : number
#'  first parameter of the Beta distribution
#' @typed beta : number
#'  second parameter of the Beta distribution
#' @return A beta distribution density plot
#'
#' @importFrom graphics axis
#'
#' @example examples/plotBeta.R
#' @export
#' @keywords graphics
plotBeta <- function(alpha, beta) {
  assert_number(alpha, finite = TRUE)
  assert_number(beta, finite = TRUE)
  x_support <- seq(from = 0, to = 1, length = 1000)
  data <- data.frame(
    grid = x_support,
    xticks = seq(from = 0, to = 1, by = 0.25),
    density = dbeta(x_support, alpha, beta)
  )
  ggplot2::ggplot(data) +
    ggplot2::geom_line(ggplot2::aes(x = grid, y = density)) +
    ggplot2::ggtitle(paste("Beta density with alpha =", alpha, "and beta =", beta, "parameters.")) +
    ggplot2::xlab("response rate") +
    ggplot2::ylab(quote(f(x))) +
    ggplot2::theme(axis.ticks.x = ggplot2::element_line(linewidth = 0.5)) +
    ggplot2::scale_x_continuous(labels = scales::percent_format())
}

#' Plot difference Between two Beta distributions
#'
#' This function will plot the PDF of a difference between two Beta distributions
#'
#' @typed parX : numeric
#'  non-negative parameters of the control Beta distribution
#' @typed parY : numeric
#'  non-negative parameters of the treatment Beta distribution.
#' @typed go_cut : number
#'  a meaningful improvement threshold, the lower boundary of a meaningfully improvement in response rate
#' @typed stop_cut : number
#'  a poor improvement threshold, the upper boundary of a meaningfully poor improvement in response rate
#' @typed shade : flag
#'  paint the two areas under the curve, default value = TRUE
#' @typed note : flag
#'  show values of the colored area, default value = TRUE
#' @return a ggplot object
#'
#' @example examples/plotBetaDiff.R
#'
#' @importFrom ggplot2 geom_line ggtitle geom_area
#' @importFrom stats integrate
#'
#' @export
#' @keywords graphics
plotBetaDiff <- function(parX, # parameters of control or SOC
                         parY, # parameters of experimental arm
                         go_cut = 0.20, # a meaningful improvement threshold
                         stop_cut = 0.1, # a poor improvement threshold
                         shade = TRUE, # paint the two areas under the curve
                         note = TRUE) { # show values of the colored area
  assert_numeric(parX, lower = 0, finite = TRUE, any.missing = FALSE)
  assert_numeric(parY, lower = 0, finite = TRUE, any.missing = FALSE)
  assert_number(go_cut, finite = TRUE)
  assert_number(stop_cut, finite = TRUE)
  assert_flag(shade)
  assert_flag(note)

  diff <- seq(from = -1, to = 1, length = 1000)
  data <- data.frame(
    grid = diff,
    density = dbetadiff(z = diff, parY = parY, parX = parX)
  )
  data$stop <- ifelse(diff > -1 & diff < stop_cut, TRUE, FALSE)
  data$go <- ifelse(diff > go_cut & diff < 1, TRUE, FALSE)

  go_auc <- integrate(
    f = dbetadiff,
    parX = parX,
    parY = parY,
    lower = go_cut, # Calculate probability of go, if difference was at least `go_cut`.
    upper = 1
  )
  stop_auc <- integrate(
    f = dbetadiff,
    parX = parX,
    parY = parY,
    lower = -1,
    upper = stop_cut # Calculate probability of stop, if difference was at most `stop_cut`.
  )

  go_label <- paste("P(Go) is", round(go_auc$value * 100, digits = 2), "%")
  stop_label <- paste("P(Stop) is", round(stop_auc$value * 100, digits = 2), "%")
  plot_title <- paste("According to Beta difference density", go_label, "and", stop_label)

  pbetadiff_plot <- if (shade) {
    ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = grid, y = density)) +
      ggplot2::geom_line(colour = "#888888") +
      ggplot2::geom_area(
        data = data[data$grid < stop_cut, ], fill = "#FF0046",
        mapping = ggplot2::aes(x = ifelse(grid < 0.2 & grid < 0.5, grid, 0))
      ) +
      ggplot2::geom_area(
        data = data[data$grid > go_cut, ], fill = "#009E73",
        mapping = ggplot2::aes(x = ifelse(grid > 0.3, grid, 0))
      ) +
      ggplot2::xlab("Difference between treatment") +
      ggplot2::ylab(quote(f(x))) +
      ggplot2::ggtitle(plot_title)
  } else {
    pbetadiff_plot <- ggplot2::ggplot(data = data) +
      ggplot2::geom_line(aes(x = grid, y = density, colour = "#888888")) +
      xlab("Difference between treatment") +
      ggplot2::ylab(quote(f(x))) +
      ggplot2::ggtitle(plot_title)
  }
  if (note) {
    pbetadiff_plot <- pbetadiff_plot +
      ggplot2::annotate("text", x = -0.5, y = 3.75, size = 5, label = stop_label, colour = "#FF0046") +
      ggplot2::annotate("text", x = -0.5, y = 3.25, size = 5, label = go_label, colour = "#009E73")
  }


sumBetadiff <- function(parX, # Treatment group's parameters
                        parY,  # Control group's parameters
                        coverage = 0.9,
                        go_cut,
                        stop_cut,
                        seed = as.numeric(R.Version()$year)) {
  assert_numeric(parY, len = 2, lower = .Machine$double.xmin, any.missing = FALSE, finite = TRUE)
  assert_numeric(parX, len = 2, lower = .Machine$double.xmin, any.missing = FALSE, finite = TRUE)
  assert_number(coverage, finite = TRUE)
  assert_number(go_cut, finite = TRUE)
  assert_number(stop_cut, finite = TRUE)

  res <- try(
    {
      mode <- stats::optimize(
        f = dbetadiff,
        interval = c(-0.999, 0.999),
        parY = parY,
        parX = parX,
        maximum = TRUE
      )$maximum

      lower <- qbetadiff( # to recover x when F(x) is at lower percentile
        p = (1 - coverage) / 2,
        parY = parY,
        parX = parX
      )

      upper <- qbetadiff( # to recover x when F(x) is at upper percentile
        p = (1 + coverage) / 2,
        parY = parY,
        parX = parX
      )
      # go_auc <- integrate(
      #   f = dbetadiff,
      #   parX = parX,
      #   parY = parY,
      #   lower = go_cut, # Calculate probability of go, if difference was at least `go_cut`.
      #   upper = 1
      # )
      # Prob for Go:
      auc_go <- stats::integrate(
        f = dbetadiff,
        parX = parX,
        parY = parY,
        lower = go_cut,
        upper = 1,
        subdivisions = 1000L,
        rel.tol = .Machine$double.eps^0.1
      )$value

      # Prob for Stop:
      auc_stop <- stats::integrate(
        f = dbetadiff,
        parX = parX,
        parY = parY,
        lower = -1,
        upper = stop_cut,
        subdivisions = 1000L,
        rel.tol = .Machine$double.eps^0.1
      )$value

      assert_true(mode > 0)
      assert_number(mode, upper = 1, na.ok = FALSE)
      assert_number(lower, lower = -1, upper = 1, na.ok = FALSE)
      assert_number(upper, lower = 0, upper = 1, na.ok = FALSE)
      assert_number(auc_go, upper = 1, na.ok = FALSE)
      assert_number(auc_stop, upper = 1, na.ok = FALSE)

      list(
        mode = mode,
        ci = c(lower, upper),
        go = auc_go,
        nogo = auc_stop
      )
    },
    silent = TRUE
  )

  ## if there were any errors, fall back to Monte Carlo estimation
  if (inherits(res, "try-error")) { # try-error is a class
    set.seed = seed
    samples <- stats::rbeta(n = 2e6, parY[1], parY[2]) -
      rbeta(n = 2e6, parX[1], parX[2])

    lower <- stats::quantile(samples, prob = (1 - coverage) / 2)
    upper <- stats::quantile(samples, prob = (1 + coverage) / 2)

    auc_go <- mean(samples > go_cut)
    auc_stop <- mean(samples < stop_cut)

    samples <- cut(samples, breaks = seq(from = -1, to = 1, length = 801))
    samples <- table(samples)
    mode <- seq(from = -1, to = 1, by = 0.0025)[which.max(samples)]

    assert_true(mode > 0)
    assert_number(mode, upper = 1, na.ok = FALSE)
    assert_number(lower, lower = -1, upper = 1, na.ok = FALSE)
    assert_number(upper, lower = 0, upper = 1, na.ok = FALSE)
    assert_number(auc_go, upper = 1, na.ok = FALSE)
    assert_number(auc_stop, upper = 1, na.ok = FALSE)

    res <- list(
      mode = mode,
      ci = c(lower, upper),
      go = auc_go,
      nogo = auc_stop
    )
  }
  assert_list(res)
  res
}


  results <- list(plot = plotbeta_plot,
                  table = summary_betadiff)
  results
}


