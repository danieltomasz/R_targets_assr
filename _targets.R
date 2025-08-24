library(targets)
library(tarchetypes)

# Packages ----

tar_option_set(
  packages = c("readr", "dplyr", "brms", "ggplot2", "cmdstanr"),
  workspace_on_error = TRUE
)

targets::tar_source() # sources all R/*.R and tracks them for invalidation

# Data Preparation ----
data_targets <- list(
  # raw inputs tracked as files
  tar_target(itpc_csv, "data/Destrieux_final_itpc.csv", format = "file"),
  tar_target(spec_csv, "data/specparam_all.csv", format = "file"),

  # join + tidy (in memory)
  tar_target(analysis_df, make_analysis_df(itpc_csv, spec_csv)),

  # save intermediates (files returned; targets tracks them)
  tar_target(
    analysis_df_csv,
    write_df_csv(analysis_df, "derived/analysis_df.csv"),
    format = "file"
  ),

  # restrict to S=stim and set baselines (in memory)
  tar_target(
    df_stim,
    analysis_df |>
      dplyr::filter(S == "stim") |>
      dplyr::mutate(
        P = forcats::fct_relevel(P, "pre", "post"),
        T = forcats::fct_relevel(T, "sham", "real")
      )
  ),

  # save S=stim subset too
  tar_target(
    df_stim_csv,
    write_df_csv(df_stim, "derived/df_stim.csv"),
    format = "file"
  )
)


# Test ITPC ----
model_itpc <- list(
  # 1) Fit multilevel Student-t model with P, T, and P×T; random effects by roi and subject
  # Fit + save; return the on-disk file path
  tar_target(
    model_pt_file,
    {f <- itpc ~ 1 + P*T + (1 + P*T | roi) + (1 + P*T | subject)
      fit_brms_generic(
        df = df_stim,
        formula = f,
        file_stem = "model_itpc_stim_pt",
        family   = brms::student(),
        # example: prior = brms::set_prior("student_t(3, 0, 0.1)", class = "b")
      )
      here::here("fits", "model_itpc_stim_pt.rds")
    },
    format = "file"
  ),
  
  # Load when needed downstream
  tar_target(
    model_pt_stim,
    readRDS(model_pt_file),
    format = "rds"
  ), 
  # 3) Pre-specified, interpretable contrasts (all within S=stim)
  #    b_Ppost         = post - pre (at T=sham)
  #    b_Treal         = real - sham (at P=pre)
  #    b_Ppost:Treal   = difference-in-differences (interaction)
  tar_target(
    pt_hypothesis,
    {
      hyp <- c(
        "ΔP_sham" = "Ppost = 0",
        "ΔT_pre" = "Treal = 0",
        "ΔP_real" = "Ppost + Ppost:Treal = 0",
        "ΔT_post" = "Treal + Ppost:Treal = 0",
        "DID" = "Ppost:Treal = 0"
      )
      res <- brms::hypothesis(model_pt_stim, hyp)
      # attach posterior probability of direction (P+)
      draws <- as.data.frame(res$samples)
      p_plus <- sapply(draws, function(x) mean(x > 0))
      list(hypothesis = res$hypothesis, samples = res$samples, p_plus = p_plus)
    },
    format = "rds"
  ),
  
  # 4) Tidy posterior draws for key fixed effects: use in plots/tables in the vignette
  tar_target(
    pt_draws,
    {
      vars <- c("b_Intercept", "b_Ppost", "b_Treal", "b_Ppost:Treal")
      posterior::as_draws_df(model_pt_stim)[, vars, drop = FALSE]
    },
    format = "rds"
  ),
  
  # 5) (Optional) ROI-wise random slope for the interaction (for brain maps etc.)
  # Comment out if you don't need it.
  tar_target(
    pt_roi_slopes,
    {
      # Extract random effect draws for roi on the P×T interaction
      # brms names: "r_roi[<ROI>,Ppost:Treal]"
      dd <- posterior::as_draws_df(model_pt_stim)
      rn <- grep("^r_roi\\[.*?,Ppost:Treal\\]$", colnames(dd), value = TRUE)
      if (!length(rn)) {
        return(NULL)
      }
      # summarize posterior mean & 95% CrI per ROI
      S <- lapply(rn, function(v) {
        roi <- sub("^r_roi\\[(.*?),Ppost:Treal\\]$", "\\1", v)
        m <- mean(dd[[v]])
        q <- quantile(dd[[v]], c(0.025, 0.975))
        data.frame(roi = roi, mean = m, l95 = q[[1]], u95 = q[[2]])
      })
      do.call(rbind, S)
    },
    format = "rds"
  )
)


# Run quarto vignettes ----
vignette_targets <- list(
  
  # Your chapter-as-vignette: renders with tar_read() calls inside the QMD
  tarchetypes::tar_quarto(vignette_render, path = "vignettes/01_simple_brms.qmd", quiet = FALSE)
)




# IMPORTANT: return ONE flat plan (don’t nest lists)
c(data_targets, model_itpc, vignette_targets)
