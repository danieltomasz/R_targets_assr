# R/models.R

fit_brms_spectral <- function(df, model_suffix = "default") {
  res <- itpc_resources()
  
  brms::brm(
    formula = exponent ~ 1 + (1 | subject) + (1 | roi),
    data = df,
    family = student(),
    chains = res$chains,
    cores = res$cores,
    threads = res$threads,
    iter = 4000,
    warmup = 1000,
    control = list(adapt_delta = 0.95),
    file = here::here("fits", paste0("model_spectral_", model_suffix)),
    file_refit = "on_change"
  )
}

# dont change if not needed to avaid recomputing
fit_brms_itpc_exponent <- function(df) {
  set.seed(2025)
  
  # auto-allocate resources: keep threads ~= physical cores
  n_phys  <- max(1, parallel::detectCores(logical = FALSE))
  # Favor between-chain first; then add modest within-chain threading
  n_chains <- min(4, n_phys)                 # up to 4 chains, but not more than cores
  tpc      <- max(1, floor(n_phys / n_chains))  # threads per chain
  
  df_stim <- dplyr::filter(df, S == "stim")
  
  brms::brm(
    itpc ~ 1 + exponent +
      (1 + exponent | roi) +
      (1 + exponent | subject),
    data     = df_stim,
    family   = brms::student(),
    backend  = "cmdstanr",
    chains   = n_chains,
    cores    = n_chains,                    # chains run in parallel
    threads  = brms::threading(tpc),        # within-chain threads
    iter     = 2000, warmup = 1000,
    control  = list(adapt_delta = 0.95),
    # caching compiled model
    file     = "fits/itpc_exponent",
    file_refit = "on_change"
  )
}

save_pp_check <- function(fit) {
  p <- brms::pp_check(fit, ndraws = 50)
  out <- "figures/fig-ppcheck.png"
  ggplot2::ggsave(out, p, width = 6, height = 4, dpi = 150)
  out
}