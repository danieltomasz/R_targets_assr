library(ggseg)
library(ggplot2)
library(tibble)
library(readr)
library(dplyr)
library(scico)
library(ggsegDesterieux)
library(RColorBrewer)


my_brain_plot <- function(df, atlas_df, parameter, filltype = "Blues", legend = FALSE) {
  # Define the expressions for each color palette
  Blues_expr <- 'scale_fill_distiller(palette = "Blues", direction = 1)'
  BuGn_expr <- 'scale_fill_distiller(palette = "BuGn", direction = 1)'
  Purples_expr <- 'scale_fill_distiller(palette = "Purples", direction = 1)'
  Vik_expr <- 'scale_fill_scico(palette = "vik",midpoint = 0)'
  # -1 reverses direction scales 
  RdBu_expr <- 'scale_fill_distiller(palette = "RdBu", direction = -1)' 
  RdYlBu_expr <- 'scale_fill_distiller(palette = "RdYlBu", direction = -1)'
  Spectral_expr <- 'scale_fill_distiller(palette = "Spectral", direction = 1)'



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
}


extractorRData <- function(file, object = NULL) {
  #' Function for extracting an object from a .RData file created by R's save() command
  #' Inputs: RData file, object name (optional)
  #' If object is NULL or empty, returns the whole environment

  E <- new.env()
  load(file = file, envir = E)

  # If object is NULL or empty, return the whole environment
  if (is.null(object) || object == "") {
    return(E)
  } else {
    # Otherwise return the specified object from the environment
    return(get(object, envir = E, inherits = FALSE))
  }
}


extract_region_effects <- function(fit, region_var = "roi", parameter = "Intercept",
                                   probs = c(0.025, 0.05, 0.5, 0.95, 0.975),
                                   digits = 8, scale = 1) {
  # Check if the fit object is from brms
  if (!inherits(fit, "brmsfit")) {
    stop("The 'fit' argument must be a brmsfit object")
  }

  # Extract posterior samples for population-level effects
  aa <- fixef(fit, summary = FALSE) / scale

  # Extract group-level (random) effects
  bb <- ranef(fit, summary = FALSE)

  # Check if the specified parameter exists
  if (!parameter %in% colnames(aa)) {
    stop(paste0("Parameter '", parameter, "' not found in the model"))
  }

  # Check if the specified region variable exists
  if (!region_var %in% names(bb)) {
    stop(paste0("Region variable '", region_var, "' not found in the model random effects"))
  }

  # Calculate number of posterior samples
  ns <- nrow(aa)

  # Calculate posterior samples at each region for the parameter
  ps <- apply(bb[[region_var]][, , parameter], 2, "+", aa[, parameter])

  # Create initial result data frame
  result <- data.frame(
    mean = apply(ps, 2, mean),
    SD = apply(ps, 2, sd)
  )

  # Add P+ column (probability of effect being positive)
  # Using quote() to preserve the "+" in the column name
  p_plus <- apply(ps, 2, function(x) sum(x > 0) / ns)
  result <- cbind(result, "P+" = p_plus)

  # Add quantiles
  quantiles <- t(apply(ps, 2, quantile, probs = probs))
  colnames(quantiles) <- paste0(100 * probs, "%")

  # Combine results
  result <- cbind(result, quantiles)

  # Round values to specified number of digits
  result <- round(result, digits)

  # Return the summarized results
  return(result)
}
