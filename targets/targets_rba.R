suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(processx)
})

# in_long must have columns: subject, roi, S (values: prestim, stim), itpc
build_rba_diff <- function(in_long, out_tsv) {
  df <- readr::read_tsv(in_long, show_col_types = FALSE)
  
  req <- c("subject","roi","S","itpc")
  miss <- setdiff(req, names(df))
  if (length(miss)) stop("Missing columns in input: ", paste(miss, collapse=", "))
  
  if (!all(c("prestim","stim") %in% unique(df$S)))
    stop("S must contain exactly the two levels: 'prestim' and 'stim'.")
  
  wide <- df %>%
    group_by(subject, roi, S) %>%
    summarise(itpc = mean(itpc, na.rm = TRUE), .groups = "drop") %>%
    mutate(S = factor(S, levels = c("prestim","stim"))) %>%
    pivot_wider(names_from = S, values_from = itpc)
  
  out <- wide %>%
    transmute(Subj = subject, ROI = roi, Y = stim - prestim) %>%
    tidyr::drop_na(Y)
  
  dir.create(dirname(out_tsv), showWarnings = FALSE, recursive = TRUE)
  readr::write_tsv(out, out_tsv)
  out_tsv
}

run_rba <- function(data_tsv, prefix, chains = 4, iterations = 2000, distY = "student") {
  rba <- Sys.which("RBA")
  if (rba == "") stop("RBA not found on PATH. Fix PATH or pass absolute path.")
  
  dir.create(dirname(prefix), showWarnings = FALSE, recursive = TRUE)
  
  cmd <- sprintf(
    "%s -prefix %s -chains %d -iterations %d -model 1 -EOI Intercept -distY %s -dataTable %s",
    shQuote(rba), shQuote(prefix), chains, iterations, shQuote(distY), shQuote(data_tsv)
  )
  
  # run and fail fast if RBA errors
  res <- processx::run(command = "bash", args = c("-lc", cmd), error_on_status = TRUE)
  
  # RBA writes <prefix>.txt
  out_txt <- paste0(prefix, ".txt")
  if (!file.exists(out_txt)) stop("RBA completed but output not found: ", out_txt)
  out_txt
}

read_rba <- function(txt_path) {
  read.delim(txt_path, comment.char = "#", check.names = FALSE)
}