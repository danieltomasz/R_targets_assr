library(dplyr)
library(tidyr)
library(ggplot2)
library(ggdist)
library(scales)
library(patchwork)
library(ggseg)
library(ggsegDesterieux)
library(RColorBrewer)
library(scico)


plot_metrics <- function(
    data,
    metrics,
    hue = NULL,
    group_axis = "auto",     
    facet_ncol = 1,
    widths = c(0.50, 0.80, 0.95),
    add_zero_line = TRUE,
    base_size = 12,
    color_palette = NULL,      # NEW: single palette or list of palettes per group
    metric_labels = NULL       # NEW: named vector for custom labels
) {
  
  # Apply custom labels if provided
  if (!is.null(metric_labels)) {
    # Ensure all metrics have labels
    missing_labels <- setdiff(metrics, names(metric_labels))
    if (length(missing_labels) > 0) {
      warning(paste("No labels provided for:", paste(missing_labels, collapse = ", ")))
      # Use original names for missing labels
      for (m in missing_labels) {
        metric_labels[m] <- m
      }
    }
  } else {
    # Use original names
    metric_labels <- setNames(metrics, metrics)
  }
  
  # Determine axis grouping
  if (is.list(group_axis)) {
    metric_groups <- group_axis
  } else if (is.character(group_axis) && length(group_axis) == 1) {
    if (group_axis == "auto") {
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
  
  # Prepare color palettes
  if (is.null(color_palette)) {
    # Default palettes
    palettes_list <- rep(list(NULL), length(metric_groups))
  } else if (is.list(color_palette) && !is.null(names(color_palette))) {
    # Named list of palettes per group
    palettes_list <- lapply(names(metric_groups), function(g) {
      color_palette[[g]]
    })
  } else if (!is.list(color_palette) || all(sapply(color_palette, is.character))) {
    # Single palette for all groups
    palettes_list <- rep(list(color_palette), length(metric_groups))
  } else {
    # Unnamed list of palettes
    palettes_list <- color_palette
  }
  
  # Create a plot for each group
  plot_list <- lapply(seq_along(metric_groups), function(i) {
    group_name <- names(metric_groups)[i]
    group_metrics <- metric_groups[[group_name]]
    group_palette <- palettes_list[[i]]
    
    # Determine columns to keep
    cols_to_keep <- if (!is.null(hue)) {
      c(group_metrics, hue)
    } else {
      group_metrics
    }
    
    # Create long format
    long <- data %>%
      select(any_of(cols_to_keep)) %>%
      pivot_longer(
        cols = all_of(group_metrics), 
        names_to = "metric", 
        values_to = "value"
      ) %>%
      mutate(
        metric = factor(metric, levels = group_metrics),
        metric_label = factor(metric_labels[as.character(metric)], 
                              levels = metric_labels[group_metrics]),
        y_dummy = ""
      )
    
    # Calculate x limits
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
        ~ metric_label,  # Use labels instead of raw metric names
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
    
    # Apply custom color palette
    if (!is.null(group_palette)) {
      if (is.null(hue)) {
        p <- p + 
          scale_fill_manual(values = group_palette) +
          scale_color_manual(values = group_palette)
      } else {
        # For hue coloring, apply palette to the hue variable
        p <- p + 
          scale_fill_manual(values = group_palette) +
          scale_color_manual(values = group_palette)
      }
    }
    
    # Add zero line
    if (add_zero_line) {
      has_diff <- any(grepl("diff|change|delta", group_metrics, ignore.case = TRUE))
      if (has_diff) {
        # Create vline data with labeled metrics
        vline_data <- data.frame(
          metric = factor(group_metrics, levels = group_metrics),
          y_dummy = ""
        ) %>%
          mutate(metric_label = factor(metric_labels[as.character(metric)],
                                       levels = metric_labels[group_metrics]))
        
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
    return(wrap_plots(plot_list, ncol = 1))
  }
}


#' Create standardized condition comparison plots
#'
#' @param data Data frame with columns: {metric}_prestim, {metric}_stim, {metric}_diff
#' @param metric Character: column name prefix (e.g., "exponent", "offset", "itpc")
#' @param metric_label Character: display name (e.g., "Aperiodic Exponent", "ITPC")
#' @param color_palette List: optional custom colors for conditions and difference
#' @param facet_ncol Integer: number of columns for faceting
#'
#' @return ggplot object
plot_condition_comparison <- function(
    data,
    metric,
    metric_label,
    color_palette = NULL,
    facet_ncol = 1) {
  
  # Default color palette if not provided
  if (is.null(color_palette)) {
    color_palette <- list(
      conditions = c("#E76F51", "#2A9D8F"), # Coral and teal
      difference = c("#264653")              # Dark blue
    )
  }
  
  # Construct metric names
  metrics_to_plot <- glue::glue("{metric}_{c('prestim', 'stim', 'diff')}")
  
  # Construct labels
  metric_labels <- setNames(
    c(
      glue::glue("Pre-stimulus\n{metric_label}"),
      glue::glue("Stimulus\n{metric_label}"),
      glue::glue("Change in\n{metric_label}")
    ),
    metrics_to_plot
  )
  
  # Create plot
  plot_metrics(
    data,
    metrics = metrics_to_plot,
    group_axis = list(
      conditions = c(glue::glue("{metric}_prestim"), glue::glue("{metric}_stim")),
      difference = glue::glue("{metric}_diff")
    ),
    metric_labels = metric_labels,
    color_palette = color_palette,
    facet_ncol = facet_ncol
  )
}


my_brain_plot <- function(df, atlas_df, parameter, filltype = "Blues", legend = FALSE, limits = NULL) {
  # Define the expressions for each color palette
  Blues_expr <- 'scale_fill_distiller(palette = "Blues", direction = 1, limits = limits)'
  BuGn_expr <- 'scale_fill_distiller(palette = "BuGn", direction = 1, limits = limits)'
  Purples_expr <- 'scale_fill_distiller(palette = "Purples", direction = 1, limits = limits)'
  Vik_expr <- 'scale_fill_scico(palette = "vik",midpoint = 0, limits = limits)'
  # -1 reverses direction scales 
  RdBu_expr <- 'scale_fill_distiller(palette = "RdBu", direction = -1, limits = limits)' 
  RdYlBu_expr <- 'scale_fill_distiller(palette = "RdYlBu", direction = -1, limits = limits)'
  Spectral_expr <- 'scale_fill_distiller(palette = "Spectral", direction = 1, limits = limits)'
  
  
  
  # Use switch to select the appropriate expression based on filtype
  color_palette <- switch(filltype,
                          "Blues" = Blues_expr,
                          "BuGn" = BuGn_expr,
                          "Purples" = Purples_expr,
                          "Vik" = Vik_expr,
                          "RdBu" = RdBu_expr,
                          "RdYlBu" = RdYlBu_expr,
                          "Spectral" = Spectral_expr,
                          stop("Unknown filltype") # Default case if none of the above matches
  )
  # Go further with analysis
  parameter <- enquo(parameter)
  df_combined <- atlas_df %>%
    as_tibble() %>%
    left_join(df) %>%
    as_brain_atlas()
  
  p <- ggplot() +
    ggseg::geom_brain(
      atlas = df_combined,
      mapping = aes(fill = !!parameter),
      position = position_brain(side ~ hemi),
      show.legend = legend
    ) +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.background = element_rect(fill = "transparent", colour = NA),
      plot.margin = unit(c(-1, -1.2, -1.2, -1.5), "cm"), # Edited code
      legend.position = "none"
    ) + # Left margin
    theme_void() +
    eval(rlang::parse_expr(color_palette)) +
    theme(legend.position = "bottom")
  
  
  return(p)
}