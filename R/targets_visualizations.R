# R/visualizations.R

#' Create and save brain plot from brms fit
#' @param fit brmsfit object
#' @param parameter Which parameter to plot (e.g., "Intercept", "exponent")
#' @param region_var Name of the region variable (default "roi")
#' @param filltype Color palette for brain plot
#' @param filter_significant If TRUE, only plot regions with P+ > 0.95 or P+ < 0.05
#' @param title Plot title
#' @param filename Output filename (without extension)
#' @param width Plot width in inches
#' @param height Plot height in inches
#' @return Path to saved plot file
create_brain_plot <- function(fit, parameter = "Intercept", region_var = "roi", 
                              filltype = "RdBu", filter_significant = TRUE,
                              title = NULL, filename = NULL, 
                              width = 10, height = 6) {
  
  # Load required libraries and data
  library(ggsegDesterieux)
  library(ggplot2)
  
  # Extract region-specific effects
  effects <- extract_region_effects(
    fit = fit,
    region_var = region_var,
    parameter = parameter,
    digits = 4
  )
  
  # Create results dataframe with roi names
  results_df <- data.frame(
    label = rownames(effects),  # ggseg uses 'label' for region names
    mean_effect = effects$mean,
    pplus = effects$`P+`,
    lower95 = effects$`2.5%`,
    upper95 = effects$`97.5%`,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      significant = (pplus > 0.95) | (pplus < 0.05),
      effect_direction = case_when(
        mean_effect > 0 ~ "positive",
        mean_effect < 0 ~ "negative", 
        TRUE ~ "none"
      )
    )
  
  # Filter for significant effects if requested
  if (filter_significant) {
    plot_df <- results_df %>% filter(significant)
    if (nrow(plot_df) == 0) {
      warning("No significant effects found for plotting")
      return(NULL)
    }
  } else {
    plot_df <- results_df
  }
  
  # Create brain plot
  p <- my_brain_plot(
    df = plot_df,
    atlas_df = desterieux,  # from ggsegDesterieux
    parameter = mean_effect,
    filltype = filltype,
    legend = TRUE
  )
  
  # Add title if provided
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }
  
  # Create filename if not provided
  if (is.null(filename)) {
    significance_suffix <- if(filter_significant) "_significant" else "_all"
    filename <- paste0("brain_plot_", parameter, significance_suffix)
  }
  
  # Ensure figures directory exists
  dir.create("figures", recursive = TRUE, showWarnings = FALSE)
  
  # Save plot
  output_path <- file.path("figures", paste0(filename, ".png"))
  ggsave(
    filename = output_path,
    plot = p,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
  
  # Also save the underlying data
  data_path <- file.path("figures", paste0(filename, "_data.csv"))
  write_csv(results_df, data_path)
  
  # Print summary
  cat(sprintf(
    "Brain plot saved: %s\n- Total regions: %d\n- Significant regions: %d\n- Parameter: %s\n", 
    output_path, 
    nrow(results_df), 
    sum(results_df$significant),
    parameter
  ))
  
  return(output_path)
}

#' Create summary statistics plot from effects
#' @param fit brmsfit object  
#' @param parameter Parameter to analyze
#' @param filename Output filename
create_effects_summary_plot <- function(fit, parameter = "Intercept", 
                                        filename = "effects_summary") {
  
  effects <- extract_region_effects(fit, parameter = parameter)
  
  # Create summary dataframe
  summary_df <- data.frame(
    roi = rownames(effects),
    mean = effects$mean,
    lower = effects$`2.5%`,
    upper = effects$`97.5%`,
    pplus = effects$`P+`
  ) %>%
    mutate(
      significant = (pplus > 0.95) | (pplus < 0.05),
      roi = factor(roi, levels = roi[order(mean)])
    )
  
  # Create forest plot
  p <- ggplot(summary_df, aes(x = mean, y = roi, color = significant)) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_pointrange(aes(xmin = lower, xmax = upper), size = 0.3) +
    scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "red")) +
    labs(
      title = paste("Regional Effects:", parameter),
      x = paste(parameter, "Effect Size"),
      y = "Brain Region",
      color = "Significant"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 8),
      legend.position = "bottom"
    )
  
  # Save plot
  dir.create("figures", recursive = TRUE, showWarnings = FALSE)
  output_path <- file.path("figures", paste0(filename, ".png"))
  ggsave(output_path, p, width = 12, height = 8, dpi = 150)
  
  return(output_path)
}