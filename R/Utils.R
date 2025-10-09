
library(tibble)
library(readr)
library(dplyr)
library(brms)
library(here)


#' Prepare data for RBA analysis by aggregating to subject×roi level
#'
#' @param data Data frame with subject, roi, and metric columns
#' @param metric_col Character: name of metric column to analyze
#' @param avg_function Function: how to aggregate (default: mean)
#' @return Data frame with columns: Subj, ROI, Y
prepare_rba_data <- function(data, metric_col, avg_function = mean) {
  data %>%
    group_by(subject, roi) %>%
    summarise(
      value = avg_function(.data[[metric_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    transmute(
      Subj = subject,
      ROI = roi,
      Y = value
    ) %>%
    tidyr::drop_na(Y)
}

#' Run RBA model with standardized settings
#'
#' @param data Data frame with columns: Subj, ROI, Y
#' @param model_name Character: prefix for output files
#' @param dist_y Character: distribution type ("student" or "normal")
#' @param output_dir Character: directory for model outputs (default: "models")
#' @param iterations Integer: MCMC iterations
#' @param chains Integer: number of chains
#' @return Integer: system command exit status
run_rba_model <- function(
    data,
    model_name,
    base_dir = here::here(),
    dist_y = "student",
    data_dir = "data_table",
    output_dir = "models",
    iterations = 5000,
    chains = 4
) {
  # Construct full paths
  full_output_dir <- file.path(base_dir, output_dir)
  full_data_dir <- file.path(base_dir, data_dir)
  
  # Ensure directories exist
  dir.create(full_output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(full_data_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Write data table
  data_file <- file.path(full_data_dir, glue::glue("{model_name}.tsv"))
  
  write.table(
    data,
    file = data_file,
    sep = "\t",
    quote = FALSE,
    row.names = FALSE,
    col.names = TRUE,
    na = "NA"
  )
  
  # Construct and run RBA command
  cmd <- paste(
    "RBA",
    glue::glue("-prefix {output_dir}/{model_name}"),
    glue::glue("-chains {chains}"),
    glue::glue("-iterations {iterations}"),
    "-model 1",
    "-EOI 'Intercept'",
    "-ridgePlot 10 8",
    glue::glue("-distY '{dist_y}'"),
    "-dataTable",
    data_file
  )
  
  cat(glue::glue("Running RBA model: {model_name}\n"))
  cat(glue::glue("Data file: {data_file}\n"))
  cat(glue::glue("Output dir: {full_output_dir}\n\n"))
  status <- system(cmd)
  
  # Move ridge plot if it exists
  ridge_file <- "Intercept_ridge.png"
  if (file.exists(ridge_file)) {
    fs::file_move(
      ridge_file,
      glue::glue("{output_dir}/{model_name}_intercept_ridge.png")
    )
  }
  
  invisible(status)
}

#' Extract and visualize RBA results
#'
#' @param model_name Character: name of the RBA model
#' @param output_dir Character: directory containing model outputs
#' @param atlas_df Data frame: brain atlas for visualization
#' @param plot_title Character: title for brain plot
#' @param filltype Character: color palette for brain plot
#' @param limits Numeric vector: color scale limits (optional)
#' @param significance_threshold Numeric: P+ threshold for significance (default: 0.975)
#' @return List with: results_df, significant_rois, brain_plot
extract_and_plot_rba <- function(
    model_name,
    output_dir = "models",
    base_dir = here::here(),
    atlas_df = NULL,
    plot_title = NULL,
    filltype = "RdBu",
    limits = NULL,
    significance_threshold = 0.975
) {
  # Construct full path
  full_output_dir <- file.path(base_dir, output_dir)
  model_file <- file.path(full_output_dir, glue::glue("{model_name}.RData"))
  
  # Check if model file exists
  if (!file.exists(model_file)) {
    stop(glue::glue("Model file not found: {model_file}"))
  }
  
  # Load model results
  e <- new.env()
  load(model_file, envir = e)
  
  # Extract intercept effects
  intercept_effects <- extract_region_effects(
    fit = e$fm,
    region_var = "ROI",
    parameter = "Intercept",
    digits = 4
  )
  
  # Create results dataframe
  results_df <- data.frame(
    label = rownames(intercept_effects),
    mean_effect = intercept_effects$mean,
    sd = intercept_effects$SD,
    p_plus = intercept_effects$`P+`,
    lower95 = intercept_effects$`2.5%`,
    upper95 = intercept_effects$`97.5%`
  )
  
  # Filter significant ROIs
  significant_rois <- results_df %>%
    dplyr::filter(
      p_plus > significance_threshold | 
        p_plus < (1 - significance_threshold)
    ) %>%
    arrange(desc(abs(mean_effect)))
  
  # Create brain plot if atlas provided
  brain_plot <- NULL
  if (!is.null(atlas_df)) {
    brain_plot <- my_brain_plot(
      df = significant_rois,
      atlas_df = atlas_df,
      parameter = mean_effect,
      filltype = filltype,
      legend = TRUE,
      limits = limits
    )
    
    if (!is.null(plot_title)) {
      brain_plot <- brain_plot + ggtitle(plot_title)
    }
  }
  
  # Return results
  list(
    results_df = results_df,
    significant_rois = significant_rois,
    brain_plot = brain_plot
  )
}


write_df_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  # deterministic ordering to avoid noisy diffs across rebuilds
  df <- df |> dplyr::arrange(subject, roi, P, T, S)
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(df, tmp)
  file.rename(tmp, path)
  path  # return the file path so targets can track it
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
