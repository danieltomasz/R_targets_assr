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
  
  # Wrap everything in tryCatch to prevent script crashes
  tryCatch({
    
    # Load required libraries and data
    library(ggsegDesterieux)
    library(ggplot2)
    library(dplyr)
    library(readr)
    
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
    
    # Count significant effects
    n_significant <- sum(results_df$significant)
    
    # Handle case with no significant effects
    if (filter_significant && n_significant == 0) {
      warning(paste0("No significant effects found for parameter '", parameter, 
                     "' (P+ > 0.95 or P+ < 0.05). Plotting all regions instead."))
      plot_df <- results_df  # Use all regions instead of returning NULL
      filter_significant <- FALSE  # Update flag for filename
    } else if (filter_significant) {
      plot_df <- results_df %>% filter(significant)
    } else {
      plot_df <- results_df
    }
    
    # Create brain plot with error handling
    p <- tryCatch({
      my_brain_plot(
        df = plot_df,
        atlas_df = desterieux,  # from ggsegDesterieux
        parameter = mean_effect,
        filltype = filltype,
        legend = TRUE
      )
    }, error = function(e) {
      warning(paste0("Error creating brain plot: ", e$message))
      return(NULL)
    })
    
    # If brain plot failed, return NULL
    if (is.null(p)) {
      warning("Brain plot creation failed, returning NULL")
      return(NULL)
    }
    
    # Add title if provided
    if (!is.null(title)) {
      p <- p + ggtitle(title)
    }
    
    # Create filename if not provided
    if (is.null(filename)) {
      significance_suffix <- if(filter_significant) "_significant" else "_all"
      filename <- paste0("brain_plot_", gsub(":", "_", parameter), significance_suffix)
    }
    
    # Ensure figures directory exists
    dir.create("figures", recursive = TRUE, showWarnings = FALSE)
    
    # Save plot with error handling
    output_path <- file.path("figures", paste0(filename, ".png"))
    tryCatch({
      ggsave(
        filename = output_path,
        plot = p,
        width = width,
        height = height,
        dpi = 300,
        bg = "white"
      )
    }, error = function(e) {
      warning(paste0("Error saving plot: ", e$message))
      return(NULL)
    })
    
    # Also save the underlying data with error handling
    data_path <- file.path("figures", paste0(filename, "_data.csv"))
    tryCatch({
      write_csv(results_df, data_path)
    }, error = function(e) {
      warning(paste0("Error saving data: ", e$message))
    })
    
    # Print summary
    cat(sprintf(
      "Brain plot created: %s\n- Total regions: %d\n- Significant regions: %d\n- Parameter: %s\n", 
      output_path, 
      nrow(results_df), 
      n_significant,
      parameter
    ))
    
    # Additional helpful message if no significant effects
    if (n_significant == 0) {
      cat("Note: No regions showed significant effects (P+ > 0.95 or P+ < 0.05)\n")
    }
    
    return(output_path)
    
  }, error = function(e) {
    # Catch any unexpected errors
    warning(paste0("create_brain_plot failed for parameter '", parameter, "': ", e$message))
    return(NULL)
  })
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