# IRF plots in a common format (ggplot2).

regime_labels <- c(pre = "Before March 2022", post = "After March 2022")
regime_colors <- stats::setNames(c("#1F77B4", "#D62728"), regime_labels)

theme_irf <- function(legend = FALSE) {
  ggplot2::theme_bw(base_size = 11, base_family = "serif") +
    ggplot2::theme(
      panel.grid.major  = ggplot2::element_line(linewidth = 0.25, color = "grey88"),
      panel.grid.minor  = ggplot2::element_blank(),
      panel.border      = ggplot2::element_rect(linewidth = 0.4, color = "black"),
      axis.ticks        = ggplot2::element_line(linewidth = 0.35, color = "black"),
      axis.ticks.length = ggplot2::unit(2.5, "pt"),
      axis.title        = ggplot2::element_text(size = 11, color = "black"),
      axis.text         = ggplot2::element_text(size = 10, color = "black"),
      plot.margin       = ggplot2::margin(6, 8, 4, 4),
      legend.position   = if (legend) "bottom" else "none",
      legend.title      = ggplot2::element_blank(),
      legend.text       = ggplot2::element_text(size = 10, color = "black")
    )
}

common_layers <- function(h) {
  list(
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.35, color = "grey35"),
    ggplot2::scale_x_continuous(breaks = sort(unique(h)), expand = ggplot2::expansion(mult = 0.01)),
    ggplot2::annotate("text", x = -Inf, y = Inf, label = "(appreciation \u2191)",
                      hjust = -0.08, vjust = 1.6, size = 3.6, family = "serif"),
    ggplot2::labs(x = "Horizon, months", y = "Impulse response")
  )
}

# Point estimate + band: columns h, mean, low, up (optional factor `regime`)
plot_irf_lp <- function(df) {
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["h"]]))
  if ("regime" %in% names(df)) {
    p <- p +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[["low"]], ymax = .data[["up"]],
                                        fill = .data[["regime"]]), alpha = 0.25) +
      ggplot2::geom_line(ggplot2::aes(y = .data[["mean"]], color = .data[["regime"]]),
                         linewidth = 0.75) +
      ggplot2::scale_color_manual(values = regime_colors) +
      ggplot2::scale_fill_manual(values = regime_colors)
    leg <- TRUE
  } else {
    p <- p +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[["low"]], ymax = .data[["up"]]),
                           fill = "grey75", alpha = 0.55) +
      ggplot2::geom_line(ggplot2::aes(y = .data[["mean"]]), linewidth = 0.75, color = "black")
    leg <- FALSE
  }
  p + common_layers(df$h) + theme_irf(leg)
}

# Identified set (ribbon) + median (line): columns h, lo, hi, med (optional factor `regime`)
plot_irf_set <- function(df) {
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["h"]]))
  if ("regime" %in% names(df)) {
    p <- p +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[["lo"]], ymax = .data[["hi"]],
                                        fill = .data[["regime"]]), alpha = 0.25) +
      ggplot2::geom_line(ggplot2::aes(y = .data[["med"]], color = .data[["regime"]]),
                         linewidth = 0.75) +
      ggplot2::scale_color_manual(values = regime_colors) +
      ggplot2::scale_fill_manual(values = regime_colors)
    leg <- TRUE
  } else {
    p <- p +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[["lo"]], ymax = .data[["hi"]]),
                           fill = "grey75", alpha = 0.55) +
      ggplot2::geom_line(ggplot2::aes(y = .data[["med"]]), linewidth = 0.75, color = "black")
    leg <- FALSE
  }
  p + common_layers(df$h) + theme_irf(leg)
}
