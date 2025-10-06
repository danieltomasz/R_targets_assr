library(dplyr)
library(tidyr)
library(ggplot2)
library(ggdist)
library(scales)
library(patchwork)

plot_metrics <- function(
    data,
    metrics,
    hue = NULL,
    group_axis = "auto",     
    facet_ncol = 1,
    widths = c(0.50, 0.80, 0.95),
    add_zero_line = TRUE,
    base_size = 12
) {
  
  # Determine axis grouping - FIX: Check type first!
  if (is.list(group_axis)) {
    # User-provided grouping
    metric_groups <- group_axis
  } else if (is.character(group_axis) && length(group_axis) == 1) {
    if (group_axis == "auto") {
      # Automatically separate diff metrics from others
      diff_metrics <- metrics[grepl("diff|change|delta", metrics, ignore.case = TRUE)]
      base_metrics <- setdiff(metrics, diff_metrics)
      
      if (length(diff_metrics) > 0 && length(base_metrics) > 0) {
        metric_groups <- list(
          base = base_metrics,
          diff = diff_metrics
        )
      } else {
        metric_groups <- list(all = metrics)
      }
    } else if (group_axis == "all") {
      metric_groups <- list(all = metrics)
    } else if (group_axis == "none") {
      metric_groups <- as.list(metrics)
      names(metric_groups) <- metrics
    } else {
      stop("group_axis must be 'auto', 'all', 'none', or a list")
    }
  } else {
    stop("group_axis must be a character string or a list")
  }
  
  # Create a plot for each group
  plot_list <- lapply(names(metric_groups), function(group_name) {
    group_metrics <- metric_groups[[group_name]]
    
    # Determine which columns to keep
    cols_to_keep <- if (!is.null(hue)) {
      c(group_metrics, hue)
    } else {
      group_metrics
    }
    
    # Create long format for this group
    long <- data %>%
      select(any_of(cols_to_keep)) %>%
      pivot_longer(
        cols = all_of(group_metrics), 
        names_to = "metric", 
        values_to = "value"
      ) %>%
      mutate(
        metric = factor(metric, levels = group_metrics),
        y_dummy = ""
      )
    
    # Calculate x limits for this group
    rng <- range(long$value, na.rm = TRUE)
    pad <- diff(rng) * 0.04
    xlim_group <- c(rng[1] - pad, rng[2] + pad)
    
    # Build plot
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
        scales = "fixed",
        strip.position = "top"
      ) +
      scale_x_continuous(
        limits = xlim_group, 
        breaks = breaks_extended(7),
        expand = expansion(mult = c(0.02, 0.02))
      ) +
      scale_y_discrete(expand = expansion(add = c(0.5, 0.5))) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = base_size) +
      theme(
        legend.position = if (is.null(hue)) "none" else "right",
        strip.text = element_text(face = "bold", size = rel(1.0)),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.spacing.y = unit(0.5, "lines")
      )
    
    # Add zero line if needed
    if (add_zero_line) {
      has_diff <- any(grepl("diff|change|delta", group_metrics, ignore.case = TRUE))
      if (has_diff) {
        vline_data <- data.frame(
          metric = factor(group_metrics, levels = group_metrics),
          y_dummy = ""
        )
        p <- p + geom_vline(
          data = vline_data,
          aes(xintercept = 0),
          linetype = "dashed",
          alpha = 0.5
        )
      }
    }
    
    return(p)
  })
  
  # Combine plots
  if (length(plot_list) == 1) {
    return(plot_list[[1]])
  } else {
    # Stack vertically with patchwork
    combined <- wrap_plots(plot_list, ncol = 1)
    return(combined)
  }
}