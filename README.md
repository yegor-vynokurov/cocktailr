cocktailr
================

- [cocktailr](#cocktailr)
  - [Citation](#citation)
  - [Overview](#overview)
  - [Installation](#installation)
  - [Input Data](#input-data)
  - [Default Labeling Workflow](#default-labeling-workflow)
  - [More Details](#more-details)
  - [Function Help](#function-help)

# cocktailr

*Cocktail* clustering for diagnostic species-based vegetation classification.

------------------------------------------------------------------------

## Citation

If you use `cocktailr`, please cite the GitHub repository and the
original Cocktail method papers listed in the package documentation.

## Overview

`cocktailr` provides:

- Cocktail clustering for vegetation data
- helper functions for selecting and inspecting clusters
- synthetic vegetation datasets for testing labeling workflows
- evidence extraction for one cluster
- structured local LLM labeling through Ollama
- evidence-aware validation of model output
- final human-review markdown cards with sidecar metadata

Current MVP workflow recommendation:

- stable building blocks:
  `generate_synthetic_vegetation_data()`, `cluster_evidence()`,
  `validate_cluster_label()`, `render_cluster_review()`,
  `label_clusters()`
- experimental boundary: `llm_label_cluster()` and concrete
  `model + prompt` combinations

------------------------------------------------------------------------

## Installation

Install the development version from GitHub:

``` r
install.packages("remotes")
remotes::install_github("dvynokur/cocktailr")
library(cocktailr)
```

If you are working from a local development checkout and want newly
added functions immediately, use:

``` r
pkgload::load_all("path/to/cocktailr")
```

------------------------------------------------------------------------

## Input Data

`cocktail_cluster()` does **not** take a file path directly. You first
load data into R, then pass the resulting object to
`cocktail_cluster()`.

Supported inputs:

- **wide format**: a matrix or data frame with plots in rows and
  species in columns
- **long format**: a data frame with plot, species, and value columns,
  plus `input_format = "long"`

For exact loading examples from CSV and for custom long-format column
mappings, see [LABELING_STEP_BY_STEP.md](LABELING_STEP_BY_STEP.md).

------------------------------------------------------------------------

## Default Labeling Workflow

The default labeling path is:

1. create or load a dataset
2. run `cocktail_cluster()`
3. run `label_clusters()`
4. inspect the saved markdown review card and the returned summary table

Recommended defaults for labeling:

- `plot_values = "rel_cover"`
- `species_cluster_phi = TRUE`
- `save_vegmatrix = TRUE`
- `model = "gemma4:12b"`
- `variant = "strict_abstention_gate_v1"`
- `workflow_steps = 1`

Minimal end-to-end example:

``` r
library(cocktailr)

syn <- generate_synthetic_vegetation_data(seed = 42)

res <- cocktail_cluster(
  vegmatrix = syn$wide_matrix,
  progress = FALSE,
  plot_values = "rel_cover",
  species_cluster_phi = TRUE,
  save_vegmatrix = TRUE
)

run <- label_clusters(
  x = res,
  clusters = "c_1",
  model = "gemma4:12b",
  variant = "strict_abstention_gate_v1",
  workflow_steps = 1,
  timeout_sec = 600,
  num_predict = 600
)

run$summary
run$results$c_1$review$file
```

Note: `syn` is a list returned by
`generate_synthetic_vegetation_data()`. Pass `syn$wide_matrix` or
`syn$long_table` to `cocktail_cluster()`, not `syn` itself.

If you omit `clusters`, `label_clusters()` processes up to the first 10
clusters selected by score.

If you are working from a local source checkout, relative review-card
paths such as `temp/reports/cluster_reviews/` are resolved against the
`cocktailr` package root automatically, even if R was started one level
higher. The same source-root resolution is now used for relative
`log_dir` paths such as `temp/llm_logs/`.

The default saved artifact is a compact markdown review card. If
dataset provenance is known, `render_cluster_review()` places the card
in a dataset-specific subfolder under `temp/reports/cluster_reviews/`;
otherwise it falls back to a timestamped folder. Use `full = TRUE` if
you also want the expanded card plus the `.meta.json` sidecar.
The compact card also records the model and prompt file paths used for
that answer, which helps compare runs across models or prompt variants.
Intermediate raw model logs through `log_dir` are optional and are not
part of the default workflow.

The `temp/` workspace is created on demand. It is safe to delete it,
and a fresh clone of the repository works without any pre-existing
`temp/` folder.

For a first real run on unknown local hardware, `timeout_sec = 600` and
`num_predict = 600` are usually safer than the package defaults. More
detail is in [LABELING_STEP_BY_STEP.md](LABELING_STEP_BY_STEP.md).
If num_predict = 600 causes an error like EOF then increase to 1200 or more

If you want the low-level manual chain
`cluster_evidence() -> llm_label_cluster() -> validate_cluster_label() -> render_cluster_review()`,
see [LABELING_STEP_BY_STEP.md](LABELING_STEP_BY_STEP.md).

------------------------------------------------------------------------

## More Details

Use these documents depending on what you need:

- [LABELING_STEP_BY_STEP.md](LABELING_STEP_BY_STEP.md)
  Full English step-by-step procedure for dataset loading, clustering,
  labeling, validation, and saved review cards.
- [llm_operation.md](llm_operation.md)
  Local Ollama setup, model recommendations, and troubleshooting.

------------------------------------------------------------------------

## Function Help

Useful help pages:

- `?cocktail_cluster`
- `?clusters_at_cut`
- `?select_clusters`
- `?generate_synthetic_vegetation_data`
- `?cluster_evidence`
- `?label_clusters`
- `?llm_label_cluster`
- `?validate_cluster_label`
- `?render_cluster_review`
