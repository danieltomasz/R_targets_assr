# Generic BRMS saver -----------------------------------------------------------
fit_brms_generic <- function(
    df,
    formula,
    file_stem,
    family   = brms::student(),
    iter     = 4000,
    warmup   = 1000,
    control  = list(adapt_delta = 0.95),
    backend  = "cmdstanr",
    ...
) {
  res <- itpc_resources()  # expects $chains, $cores, $threads
  dir.create(here::here("fits"), recursive = TRUE, showWarnings = FALSE)
  
  brms::brm(
    formula = formula,
    data    = df,
    family  = family,
    backend = backend,
    chains  = res$chains,
    cores   = res$cores,
    threads = res$threads,     # keep this consistent with your itpc_resources()
    iter    = iter,
    warmup  = warmup,
    control = control,
    file    = here::here("fits", file_stem),
    file_refit = "on_change",
    ...
  )
}

save_pp_check <- function(fit) {
  p <- brms::pp_check(fit, ndraws = 50)
  out <- "figures/fig-ppcheck.png"
  ggplot2::ggsave(out, p, width = 6, height = 4, dpi = 150)
  out
}