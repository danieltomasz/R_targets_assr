library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("readr", "dplyr", "brms", "ggplot2", "cmdstanr"),
  workspace_on_error = TRUE
)

targets::tar_source()  # sources all R/*.R and tracks them for invalidation


list(
  tar_target(itpc_csv, "data/Destrieux_final_itpc.csv", format = "file"),
  tar_target(spec_csv, "data/specparam_all.csv", format = "file"),

  # Join and prepare one tidy table for modeling
  tar_target(analysis_df, make_analysis_df(itpc_csv, spec_csv)),
  # save intermediate df
  tar_target(
    analysis_df_csv,
    {
      dir.create("derived", recursive = TRUE, showWarnings = FALSE)
      path <- file.path("derived", "analysis_df.csv")
      readr::write_csv(analysis_df, path)
      path  # <- return the path so targets can track it
    },
    format = "file",
    cue = tar_cue(mode = "thorough")  # rebuild when inputs change
  ),

  # Pre-stimulation, prestim condition  
  tar_target(
    fit_spectral_pre_stim,
    fit_brms_spectral(
      analysis_df %>% filter(P == "pre", S == "stim"),
      model_suffix = "pre_stim"
    ),
    cue = tar_cue(mode = "thorough")
  ),
  
  # Brain visualization for spectral models
  tar_target(
    brain_plot_spectral_pre_stim,
    create_brain_plot(
      fit = fit_spectral_pre_stim,
      parameter = "Intercept",
      filltype = "Blues",
      filter_significant = TRUE,
      title = "Spectral Exponent: Pre-Stimulation (Significant Regions)",
      filename = "brain_spectral_pre_stim",
      width = 10,
      height = 6
    ),
    format = "file"
  ),
  

  
  # Summary forest plot for spectral model
  tar_target(
    summary_plot_spectral_pre_stim,
    create_effects_summary_plot(
      fit = fit_spectral_pre_stim,
      parameter = "Intercept",
      filename = "summary_spectral_pre_stim"
    ),
    format = "file"
  ),
  
  # One hierarchical model (fast-ish settings to begin with)
  tar_target(
    fit_itpc_exponent,
    fit_brms_itpc_exponent(analysis_df),
    cue = tar_cue(mode = "thorough") # refit only when inputs change
  ),

  # A quick diagnostic plot saved to disk so the vignette can just include it
  targets::tar_target(ppcheck_png, save_pp_check(fit_itpc_exponent), format = "file"),

  # Your chapter-as-vignette: renders with tar_read() calls inside the QMD
  tarchetypes::tar_quarto(vignette_render, path = "vignettes/01_simple_brms.qmd", quiet = FALSE)
)
