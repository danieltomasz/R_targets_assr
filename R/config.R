#' Allocate Stan/BRMS resources once per call
#' @param max_chains upper bound for parallel chains (default from ITPC_MAX_CHAINS env or 4)
#' @return list with $n_phys, $n_chains, $tpc, and BRMS-ready $chains, $cores, $threads
#' @export
itpc_resources <- function(max_chains = NULL) {
  if (is.null(max_chains)) {
    max_chains <- as.integer(Sys.getenv("ITPC_MAX_CHAINS", "4"))
  }
  n_phys   <- max(1L, parallel::detectCores(logical = FALSE))
  n_chains <- max(1L, min(max_chains, n_phys))
  tpc      <- max(1L, floor(n_phys / n_chains))
  
  list(
    n_phys  = n_phys,
    n_chains = n_chains,
    tpc      = tpc,
    # convenience fields for brms::brm()
    chains   = n_chains,
    cores    = n_chains,
    threads  = brms::threading(tpc)
  )
}