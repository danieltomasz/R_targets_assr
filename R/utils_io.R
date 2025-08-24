write_df_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  # deterministic ordering to avoid noisy diffs across rebuilds
  df <- df |> dplyr::arrange(subject, roi, P, T, S)
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(df, tmp)
  file.rename(tmp, path)
  path  # return the file path so targets can track it
}