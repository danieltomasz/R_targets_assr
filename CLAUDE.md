# CLAUDE.md - AI Assistant Guide for R_targets_assr

## Project Overview

**assrproj** is an R package providing pipeline helpers for ITPC (Inter-Trial Phase Coherence) and spectral exponent modeling in the context of auditory steady-state response (ASSR) research. The project investigates the relationship between neural synchrony (measured via 40 Hz ASSR) and aperiodic activity (1/f slope) using Bayesian hierarchical modeling.

### Key Scientific Context
- **ITPC**: Inter-Trial Phase Coherence measures phase consistency across trials
- **ASSR**: Auditory Steady-State Response at 40 Hz as measure of gamma synchrony
- **Spectral Exponent**: Aperiodic (1/f) slope as biomarker of E/I balance
- **Experimental Design**: Within-subjects tDCS study with factors:
  - `T` (treatment): sham vs real stimulation
  - `P` (period): pre vs post stimulation
  - `S` (sound): prestim (pause) vs stim (40 Hz AM tone)

### Technology Stack
- **Pipeline**: `targets` for reproducible workflows
- **Statistical Modeling**: `brms` (Bayesian multilevel models with Stan)
- **Documentation**: Quarto (`.qmd` vignettes)
- **Dependency Management**: `renv` (lockfile-based)
- **Package Development**: Standard R package structure with roxygen2
- **Neuroimaging Tools**: RBA (Region-Based Analysis) toolbox
- **Build Automation**: Makefile for common development tasks

---

## Repository Structure

```
R_targets_assr/
├── DESCRIPTION           # R package metadata
├── NAMESPACE             # Exported functions (auto-generated)
├── Makefile              # Development task automation
├── _targets.R            # Main pipeline orchestration
├── _quarto.yml           # Quarto configuration
├── renv.lock             # Dependency versions
│
├── R/                    # Package source code
│   ├── Utils.R           # Data prep & RBA utilities
│   ├── utils_viz.R       # Visualization helpers
│   ├── config.R          # Resource allocation (Stan/BRMS)
│   ├── afni.R            # AFNI-related functions
│   └── assrproj-package.R # Package documentation
│
├── targets/              # Pipeline target definitions
│   ├── _targets_itpc.R   # ITPC comparison targets
│   ├── targets_data_prep.R # Data loading/joining
│   ├── models.R          # Model fitting targets
│   ├── targets_rba.R     # RBA analysis targets
│   └── targets_visualizations.R # Plot generation
│
├── data/                 # Raw input data (tracked as files)
│   ├── Destrieux_final_itpc.csv    # ITPC measurements
│   └── specparam_all.csv           # Spectral exponent data
│
├── derived/              # Processed intermediate data
│   ├── analysis_df.csv   # Joined ITPC + exponent data
│   └── df_stim.csv       # Filtered to S=stim condition
│
├── fits/                 # Saved model objects (.rds)
│   ├── model_itpc_stim_pt.rds
│   └── model_exponent_stim_pt.rds
│
├── vignettes/            # Quarto analysis documents
│   └── 01_simple_brms.qmd
│
├── figures/              # Static images for reports
│   └── static/
│
├── docs/                 # Generated pipeline visualizations
│   ├── pipeline.html
│   └── pipeline_tooltips.html
│
├── notebooks/            # Exploratory analysis
│   ├── data_table/       # RBA input tables
│   └── models/           # RBA model outputs
│
└── tests/                # Unit tests (testthat)
    └── testthat.R
```

---

## Development Workflows

### 1. Initial Setup

When first working with this project:

```r
# Restore package dependencies
renv::restore()

# Build package documentation
make doc
# OR: devtools::document()

# Load package functions into session
make load
# OR: devtools::load_all()
```

### 2. Running the Analysis Pipeline

The `targets` pipeline orchestrates all data processing, modeling, and reporting:

```r
# View pipeline structure
targets::tar_manifest(fields = command)

# Visualize dependency graph
targets::tar_visnetwork(targets_only = TRUE)

# Run entire pipeline (only rebuilds outdated targets)
targets::tar_make()

# Run specific target
targets::tar_make(model_pt_stim)

# Load results from completed pipeline
df <- targets::tar_read(analysis_df)
model <- targets::tar_read(model_pt_stim)
```

**Important**: The pipeline automatically tracks dependencies. If you modify:
- A function in `R/`, targets will detect the change and re-run dependent targets
- A data file in `data/`, downstream targets will rebuild
- Pipeline definitions in `targets/`, affected targets will update

### 3. Pipeline Architecture

The main pipeline file `_targets.R` sources:
- All `.R` files in `targets/` directory (pipeline definitions)
- All `.R` files in `R/` directory (package functions)

Current active pipeline configuration (line 239):
```r
c(itpc_compare_targets)
```

**To activate other target groups**, uncomment line 238:
```r
c(data_targets, model_itpc, model_exponent, vignette_targets)
```

### 4. Key Pipeline Stages

#### A. Data Preparation (`data_targets`)
1. **Load raw data**: Track CSV files as targets
2. **Join datasets**: Combine ITPC + spectral exponent via `make_analysis_df()`
3. **Filter conditions**: Subset to `S=stim`, set factor baselines
4. **Save intermediates**: Write derived CSVs for inspection

#### B. ITPC Modeling (`model_itpc`)
1. **Fit hierarchical model**: `itpc ~ 1 + P*T + (1 + P*T | roi) + (1 + P*T | subject)`
2. **Test hypotheses**: Pre-specified contrasts for treatment effects
3. **Extract posteriors**: Fixed effects and ROI-wise random slopes

#### C. Exponent Modeling (`model_exponent`)
- Parallel structure to ITPC modeling
- Model: `exponent ~ 1 + P*T + (1 + P*T | roi) + (1 + P*T | subject)`

#### D. Reporting (`vignette_targets`)
- Render Quarto vignette with `tar_quarto()`
- Vignette reads targets via `targets::tar_read()` calls

### 5. Model Fitting Conventions

**Resource Allocation** (see `R/config.R`):
```r
res <- itpc_resources()  # Detects CPU cores, sets chains/threads
# Use in brms calls:
brm(..., chains = res$chains, cores = res$cores, threads = res$threads)
```

**Saving Models**: Models are saved to `fits/` directory as `.rds` files. The pipeline:
1. Fits and saves model via `fit_brms_generic()`
2. Returns file path as a tracked target (`format = "file"`)
3. Loads model when needed via `readRDS()` (in separate target)

**Hypothesis Testing**:
```r
# Pre-specified contrasts
hyp <- c(
  "ΔP_sham" = "Ppost = 0",              # Post-pre at sham
  "ΔT_pre"  = "Treal = 0",              # Real-sham at pre
  "ΔP_real" = "Ppost + Ppost:Treal = 0", # Post-pre at real
  "ΔT_post" = "Treal + Ppost:Treal = 0", # Real-sham at post
  "DID"     = "Ppost:Treal = 0"         # Interaction (DiD)
)
brms::hypothesis(model, hyp)
```

### 6. RBA (Region-Based Analysis) Workflow

For brain-wide analysis of ROI effects:

```r
# 1. Prepare data (aggregate to subject × ROI level)
rba_data <- prepare_rba_data(df, metric_col = "itpc")

# 2. Run RBA model (external AFNI command)
run_rba_model(rba_data, model_name = "itpc_intercept")

# 3. Extract and visualize results
results <- extract_and_plot_rba(
  model_name = "itpc_intercept",
  atlas_df = atlas,
  significance_threshold = 0.95
)
```

**RBA outputs** (in `notebooks/models/`):
- `<model_name>.RData`: Stan fit object
- `<model_name>_intercept_ridge.png`: Ridge plot of ROI effects

### 7. Visualization Pipeline

**Pipeline Graphs**:
```r
# Generate interactive HTML visualization
library(visNetwork)
net <- targets::tar_network(targets_only = TRUE)
# See README.Rmd for full code to create tooltips and hierarchical layout
htmlwidgets::saveWidget(g, "docs/pipeline.html")
```

**Brain Plots**: Use `my_brain_plot()` function (in `R/utils_viz.R`) to visualize ROI-level effects on brain atlas.

### 8. Common Development Tasks

```bash
# Update documentation
make doc

# Load package for interactive use
make load

# Reload (doc + load)
make reload

# Start R session
make r

# Generate context file for AI assistants
make context
```

---

## Code Conventions & Best Practices

### Function Documentation

All exported functions MUST have roxygen2 documentation:

```r
#' Brief description (one line)
#'
#' Detailed description (optional)
#'
#' @param param_name Description of parameter
#' @return Description of return value
#' @examples
#' \dontrun{
#'   result <- my_function(x)
#' }
#' @export
my_function <- function(param_name) {
  # Implementation
}
```

### Data Handling

**Key function**: `make_analysis_df()` (in `R/Utils.R` or `targets/targets_data_prep.R`)

**Requirements**:
- Inner join on `(subject, roi, P, T, S)` - MUST be unique
- Assert no duplicates post-join
- Convert to factors: `subject`, `roi`, `S` (levels: `c("prestim", "stim")`)
- Set factor baselines: `P = fct_relevel(P, "pre", "post")`, `T = fct_relevel(T, "sham", "real")`

**File I/O**:
- Use `write_df_csv()` helper to ensure deterministic ordering (sorts by keys)
- Always create parent directories: `dir.create(..., recursive = TRUE, showWarnings = FALSE)`
- Return file path for targets to track

### Bayesian Modeling

**Family**: Prefer `brms::student()` over Gaussian for robustness to outliers

**Random Effects Structure**:
- By-ROI slopes: `(1 + P*T | roi)`
- By-subject slopes: `(1 + P*T | subject)`

**Priors**: Document any custom priors in comments or function args

**Model Diagnostics**:
- Check convergence: `summary(model)` → look for Rhat < 1.01
- Posterior predictive checks: Use `pp_check(model)`
- Save diagnostic plots to `figures/`

### Pipeline Targets

**Naming conventions**:
- Data targets: `<name>_csv` for file paths, `<name>` for in-memory objects
- Model targets: `model_<outcome>_<conditions>` (e.g., `model_itpc_stim_pt`)
- Derived targets: `<name>_hypothesis`, `<name>_draws`, `<name>_roi_slopes`

**Target options**:
```r
tar_option_set(
  packages = c("readr", "dplyr", "brms", "ggplot2", "cmdstanr"),
  workspace_on_error = TRUE,        # Save workspace if target fails
  cue = tar_cue(mode = "thorough")  # Rehash functions, not just timestamps
)
```

**Format types**:
- `format = "file"`: For file paths (targets tracks modification time)
- `format = "rds"`: For R objects (auto-serialization)
- Default (no format): In-memory R objects

**Descriptions**: Always add `description = "..."` to targets for documentation

### Style Guide

- **Pipe operator**: Use base R pipe `|>` (not magrittr `%>%`)
- **Assignment**: Use `<-` (not `=` except in function calls)
- **Tidyverse verbs**: Prefer `dplyr::`, `tidyr::`, `readr::` explicitly
- **Conflicts**: Use `library(conflicted)` and declare preferences
- **Line length**: Keep under 80-100 characters where practical
- **Indentation**: 2 spaces (standard R convention)

### Testing

**Framework**: `testthat` (edition 3)

**Location**: `tests/testthat/`

**Running tests**:
```r
devtools::test()
# OR
testthat::test_local()
```

---

## Data Dictionary

### Raw Data Files

#### `data/Destrieux_final_itpc.csv`
- **Columns**: `subject`, `roi`, `P`, `T`, `type`, `value`
- **type**: `"prestim_itpc"` or `"stim_itpc"` (maps to `S` factor)
- **value**: ITPC measurement (0-1, higher = more phase-locked)

#### `data/specparam_all.csv`
- **Columns**: `subject`, `roi`, `P`, `T`, `condition`, `exponent`, `offset`
- **condition**: Renames to `S`, should match ITPC types
- **exponent**: Spectral slope (1/f exponent, typically 0-3)

### Derived Data Files

#### `derived/analysis_df.csv`
- **Source**: `make_analysis_df()` inner join
- **Keys**: `subject` (factor), `roi` (factor), `P`, `T`, `S` (factor)
- **Outcomes**: `itpc`, `exponent`, `offset`

#### `derived/df_stim.csv`
- **Subset**: `S == "stim"` only
- **Factor levels**: `P` = `c("pre", "post")`, `T` = `c("sham", "real")`

---

## Common AI Assistant Tasks

### 1. Adding New Analysis Targets

**Steps**:
1. Define target in appropriate `targets/*.R` file
2. Add target list to return statement in `_targets.R`
3. Document with `description = "..."`
4. Use `tar_manifest()` to verify before running

**Example**:
```r
# In targets/models.R
new_targets <- list(
  tar_target(
    my_analysis,
    analyze_data(df_stim),
    description = "Perform new analysis on stimulation data"
  )
)

# In _targets.R (line 238-239)
c(data_targets, model_itpc, model_exponent, new_targets, vignette_targets)
```

### 2. Modifying Existing Functions

**Remember**:
- Update roxygen2 documentation
- Run `make doc` to regenerate NAMESPACE
- Targets will auto-detect function changes (if `cue = tar_cue(mode = "thorough")`)
- Test changes: `devtools::load_all(); tar_make()`

### 3. Adding New Vignettes

**Steps**:
1. Create `.qmd` file in `vignettes/`
2. Add `tar_quarto()` target to `vignette_targets` list
3. Use `targets::tar_read()` to load pipeline outputs in vignette
4. Render standalone: `quarto render vignettes/my_vignette.qmd`
5. Or via pipeline: `tar_make(vignette_render_my_vignette)`

### 4. Debugging Failed Targets

```r
# View error message
tar_meta(fields = error) |> filter(!is.na(error))

# Load workspace at point of failure (if workspace_on_error = TRUE)
tar_workspace(failed_target_name)
# Now debug interactively

# Manual re-run with debugging
tar_option_set(debug = "problematic_target")
tar_make()
```

### 5. Investigating Data Lineage

```r
# What depends on this target?
tar_deps(analysis_df)

# What does this target depend on?
tar_network() |> filter(to == "model_pt_stim")

# Visualize subgraph
tar_visnetwork(targets_only = TRUE, names = c("analysis_df", "model_pt_stim"))
```

### 6. Performance Optimization

**Stan/BRMS**:
- Adjust `itpc_resources()` for CPU allocation
- Use `ITPC_MAX_CHAINS` environment variable to limit parallelism
- Consider `cmdstanr` backend for faster compilation

**Targets**:
- Use `tar_make_future()` with `future::plan(multisession)` for parallel targets
- Profile slow targets: `tar_meta(fields = c(seconds, bytes))`

---

## Important Notes for AI Assistants

### Do's
- **Always run `tar_manifest()`** before modifying pipeline to understand dependencies
- **Update roxygen documentation** when changing function signatures
- **Add `description` fields** to new targets for clarity
- **Follow tidyverse style** with base pipe `|>`
- **Test data joins** - assert uniqueness on keys to catch duplicates early
- **Save models as files** in `fits/`, not in targets store (for portability)
- **Use informative priors** for brms models when domain knowledge exists
- **Version control derived data** CSVs in `derived/` (small, useful for debugging)

### Don'ts
- **Don't modify `NAMESPACE`** manually (auto-generated by roxygen2)
- **Don't commit `_targets/` directory** (build artifacts, not source)
- **Don't use `%>%`** - project uses base pipe `|>`
- **Don't assume factor order** - explicitly set with `fct_relevel()`
- **Don't skip convergence checks** on brms models before interpreting
- **Don't hard-code paths** - use `here::here()` for portability
- **Don't run RBA without checking** data format (must have `Subj`, `ROI`, `Y` columns)
- **Don't ignore warnings** from `tar_make()` - they often indicate stale dependencies

### When in Doubt
1. Check existing code in `R/` and `targets/` for patterns
2. Review `README.Rmd` for workflow examples
3. Visualize pipeline: `tar_visnetwork()` to understand data flow
4. Consult vignette `vignettes/01_simple_brms.qmd` for analysis examples
5. Run `tar_manifest(fields = command)` to see exact code executed

---

## File Modification Guidelines

### High-Touch Files (modify carefully)
- `_targets.R`: Main pipeline orchestrator - changes affect entire build
- `DESCRIPTION`: Package metadata - affects installation and dependencies
- `renv.lock`: Dependency versions - prefer `renv::snapshot()` over manual edits

### Safe to Modify
- `targets/*.R`: Pipeline definitions - modular, isolated changes
- `R/*.R`: Package functions - targets auto-detects changes
- `vignettes/*.qmd`: Reports - self-contained analysis documents
- `Makefile`: Development shortcuts - add new tasks as needed

### Auto-Generated (do not edit)
- `NAMESPACE`: Generated by `roxygen2::roxygenize()`
- `man/*.Rd`: Generated by roxygen2 from function docs
- `_targets/`: Targets cache and metadata

---

## Environment Variables

- `ITPC_MAX_CHAINS`: Maximum parallel chains for brms (default: 4)
  - Set lower on memory-constrained systems
  - Set higher on HPC clusters

---

## Additional Resources

### Key R Packages Documentation
- [targets manual](https://books.ropensci.org/targets/)
- [brms documentation](https://paul-buerkner.github.io/brms/)
- [Quarto guides](https://quarto.org/docs/guide/)
- [RBA toolbox](https://github.com/afni/afni/blob/master/src/other_builds/R_scripts/RBA.R)

### Relevant Papers
- Chen et al. (2019): RBA methodology for neuroimaging
- Pellegrino et al. (2019): Original ASSR-tDCS experiment

### Project-Specific TODOs
See `TODO.md` for current development priorities:
- Create targets for ITPC change figures

---

**Last Updated**: 2025-11-18
**Repository**: R_targets_assr (ITPC ~ exponent modeling pipeline)
