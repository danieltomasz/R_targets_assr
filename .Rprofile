# .Rprofile (project root)
if (interactive() && file.exists("DESCRIPTION")) {
  if (requireNamespace("devtools", quietly = TRUE)) {
    try(devtools::load_all(quiet = TRUE), silent = TRUE)  # enables ?help + autocompletion
  }
}

if (interactive()) {
  reload <- function() {
    if (!requireNamespace("devtools", quietly = TRUE)) {
      install.packages("devtools")
    }
    devtools::document()
    devtools::load_all(quiet = TRUE)
    message("Reloaded docs + code into this session.")
  }
}
