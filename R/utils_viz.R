library(dplyr)
library(tidyr)
library(ggplot2)
library(ggdist)
library(scales)

plot_metrics <- function(
    data,
    metrics,
    hue = NULL,
    facet_ncol = 1,
    scales = "fixed",        
    shared_xlim = TRUE,      
    widths = c(0.50, 0.80, 0.95),
    add_zero_line = TRUE,
    base_size = 12
) {
  
  # Create long format
  long <- data %>%
    pivot_longer(
      cols = all_of(metrics), 
      names_to = "metric", 
      values_to = "value"
    ) %>%
    mutate(
      metric = factor(metric, levels = metrics),
      y_dummy = ""  # Single y-value for all
    )
  
  # Calculate x limits
  xlim_final <- if (shared_xlim) {
    rng <- range(long$value, na.rm = TRUE)
    pad <- diff(rng) * 0.04
    c(rng[1] - pad, rng[2] + pad)
  } else NULL
  
  # Build plot - use y_dummy instead of metric for y-axis
  p <- ggplot(long, aes(x = value, y = y_dummy)) +
    stat_halfeye(
      aes(fill = if (is.null(hue)) metric else .data[[hue]],
          color = if (is.null(hue)) metric else .data[[hue]]),
      point_interval = median_qi,
      .width = widths,
      adjust = 0.7,
      slab_alpha = 0.6
    ) +
    facet_wrap(
      ~ metric, 
      ncol = facet_ncol,
      scales = scales,
      strip.position = "top"
    ) +
    scale_x_continuous(
      limits = xlim_final, 
      breaks = breaks_extended(7),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_discrete(expand = expansion(add = c(0.5, 0.5))) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = base_size) +
    theme(
      legend.position = if (is.null(hue)) "none" else "right",
      strip.text = element_text(face = "bold", size = rel(1.0)),
      axis.text.y = element_blank(),      # Remove y-axis text
      axis.ticks.y = element_blank(),     # Remove y-axis ticks
      panel.spacing.y = unit(1, "lines")
    )
  
  # Add zero line
  if (add_zero_line) {
    vline_data <- data.frame(
      metric = factor(metrics[grepl("diff", metrics)], levels = metrics),
      y_dummy = ""
    )
    
    if (nrow(vline_data) > 0) {
      p <- p + geom_vline(
        data = vline_data,
        aes(xintercept = 0),
        linetype = "dashed",
        alpha = 0.5
      )
    }
  }
  
  return(p)
}