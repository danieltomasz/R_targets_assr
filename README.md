# README

Run make equivalent

```R
targets::tar_make()
```

```R
install.packages(c("targets","tarchetypes","brms","readr","dplyr","ggplot2"))
targets::tar_make()                 # builds data, model, pp_check, then renders the vignette
browseURL("vignettes/01_simple_brms.html")
```

## if you using quarto, rememebrt to check if there any `_quarto.yml` a level above, interfering with your project

remember 