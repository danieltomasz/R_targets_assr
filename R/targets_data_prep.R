# R/data_prep.R


#' Join ITPC and spectral-exponent data for modeling
#'
#' Reads two CSVs and builds a tidy table keyed by `subject`, `roi`, `P`, `T`, `S`.
#'
#' @details
#' **Inclusion/Exclusion logic**
#' - Keeps only ITPC rows where `type` is one of `"prestim_itpc"` or `"stim_itpc"`.
#' - Maps `type` → `S` as: `"prestim_itpc"` → `"prestim"`, `"stim_itpc"` → `"stim"`.
#' - In spectral params, renames `condition` to `S` and requires `S` to match the ITPC side.
#' - Uses an inner join on `c(subject, roi, P, T, S)`; rows missing on either side are dropped.
#'
#' @section Output shape:
#' A tibble with one row per `(subject, roi, P, T, S)` and columns:
#' - `subject` (factor), `roi` (factor)
#' - `P`, `T`
#' - `S` (factor with levels `c("prestim","stim")`)
#' - `itpc` (numeric), `exponent` (numeric)
#'
#' @param itpc_path Path to ITPC CSV with columns `subject, roi, P, T, type, value`.
#' @param spec_path Path to spectral-params CSV with columns `subject, roi, P, T, condition, exponent`.
#' @return A tibble ready for hierarchical modeling (e.g., with **brms**), containing only `prestim`/`stim` conditions and matched keys across sources.
#'
#' @examples
#' \dontrun{
#' df <- make_analysis_df(
#'   "data/Destrieux_final_itpc.csv",
#'   "data/specparam_all.csv"
#' )
#' }
#' @export
make_analysis_df <- function(itpc_path, spec_path) {
  itpc <- readr::read_csv(itpc_path, show_col_types = FALSE) |>
    dplyr::filter(type %in% c("prestim_itpc", "stim_itpc")) |>
    dplyr::mutate(
      S = dplyr::case_when(
        type == "prestim_itpc" ~ "prestim",
        type == "stim_itpc" ~ "stim",
        TRUE ~ type
      ),
      itpc = value
    ) |>
    dplyr::select(subject, roi, P, T, S, itpc) |>
    dplyr::distinct()

  spec <- readr::read_csv(spec_path, show_col_types = FALSE) |>
    dplyr::select(subject, roi, P, T, condition, exponent, offset) |>
    dplyr::rename(S = condition) |>
    dplyr::distinct()

  out <- dplyr::inner_join(itpc, spec, by = c("subject", "roi", "P", "T", "S"))

  # sanity check: no duplicates after join
  post_dups <- out |>
    dplyr::count(subject, roi, P, T, S, name = "n") |>
    dplyr::filter(n > 1)
  if (nrow(post_dups) > 0) {
    stop("Join produced duplicates. Your inputs are still not unique on (subject, roi, P, T, S). Fix upstream.")
  }

  out |>
    dplyr::mutate(
      subject = factor(subject),
      roi     = factor(roi),
      S       = factor(S, levels = c("prestim", "stim"))
    )
}
