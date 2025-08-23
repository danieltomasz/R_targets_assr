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
