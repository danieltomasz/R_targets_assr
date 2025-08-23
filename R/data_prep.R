# R/data_prep.R
make_analysis_df <- function(itpc_path, spec_path) {
  itpc <- readr::read_csv(itpc_path, show_col_types = FALSE) |>
    dplyr::filter(type %in% c("prestim_itpc", "stim_itpc")) |>
    dplyr::mutate(
      S  = dplyr::case_when(type == "prestim_itpc" ~ "prestim",
                            type == "stim_itpc"    ~ "stim",
                            TRUE ~ type),
      itpc = value
    ) |>
    dplyr::select(subject, roi, P, T, S, itpc)
  
  spec <- readr::read_csv(spec_path, show_col_types = FALSE) |>
    dplyr::select(subject, roi, P, T, condition, exponent) |>
    dplyr::rename(S = condition)
  
  dplyr::inner_join(itpc, spec, by = c("subject", "roi", "P", "T", "S")) |>
    dplyr::mutate(
      subject = factor(subject),
      roi     = factor(roi),
      S       = factor(S, levels = c("prestim", "stim"))
    )
}