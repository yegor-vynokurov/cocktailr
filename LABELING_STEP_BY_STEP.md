# Labeling Step by Step

This document describes the recommended end-to-end labeling workflow in
English:

1. initialize the package
2. generate or load a dataset
3. run Cocktail clustering
4. choose cluster IDs
5. build evidence
6. run one-step local LLM labeling
7. validate the result
8. save the final human-review markdown card to disk


### quick flow run example: 
you run step by step and will get the markdow cards with labels of the clusters in the end. 
model = "gemma4:12b" it is a model, and read llm_operations.md or README.md for install details
output is in temp/reports/cluster_reviews folder. 
The quick flow run example will explain top 10 most high score clusters. 
For other variants read further.
qwen3.5:9b-q4_K_M
```r
pkgload::load_all("D:/documents/coctrailr/cocktailr")
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
  model = "gemma4:12b",
  variant = "strict_abstention_gate_v1",
  workflow_steps = 1,
  timeout_sec = 600,
  num_predict = 1200
)
```

The current default recommendation is:

- one-step workflow: `workflow_steps = 1`
- model: `gemma4:12b`
- prompt variant: `strict_abstention_gate_v1`
- safer first-run overrides on unknown local hardware:
  `timeout_sec = 600`, `num_predict = 600` (or 1200 if we have EOF error)
- final saved artifact: compact markdown review card under
  dataset-aware subfolders of `temp/reports/cluster_reviews/`
- no `log_dir` by default

## 0. Before You Start

You need:

- `cocktailr` installed or loaded from a local development checkout
- a local Ollama installation
- at least one local model available in Ollama

Working-directory recommendation:

- open `cocktailr/cocktailr.Rproj` when possible
- review-card paths like `temp/reports/cluster_reviews/` are now
  resolved against the local `cocktailr` source root automatically when
  that checkout can be detected
- the same source-root resolution is now used for relative log paths
  like `temp/llm_logs/`
- the `temp/` folder itself is not required to exist in advance;
  review-card and log directories are created automatically on demand
- deleting `temp/` is safe; a fresh clone without that folder will
  recreate it when you first save a review card or enable LLM logging

For Ollama setup, model installation, and troubleshooting, see
[llm_operation.md](llm_operation.md).

If you work from a local source checkout and want the newest functions
in the current R session:

```r
pkgload::load_all("path/to/cocktailr")
```
e.g. pkgload::load_all("D:/documents/coctrailr/cocktailr")

Otherwise:

```r
library(cocktailr)
```

## 1. Understand What `cocktail_cluster()` Accepts

`cocktail_cluster()` does **not** read a file path directly.

You must first load your dataset into R, then pass the resulting object
to `cocktail_cluster()`.

Supported input types:

- **wide format**
  A matrix or data frame with plots in rows and species in columns.
- **long format**
  A data frame with one row per plot-species record plus a numeric value
  column. In this case you must use `input_format = "long"`.

## 2. Option A: Generate a Synthetic Dataset

This is the easiest way to test the full pipeline.

```r
library(cocktailr)

syn <- generate_synthetic_vegetation_data(seed = 42)
```

Useful returned objects:

- `syn$wide_matrix`
  Ready for `cocktail_cluster()` as wide input
- `syn$long_table`
  Ready for `cocktail_cluster(..., input_format = "long")`
- `syn$plot_truth`
  Synthetic ground truth for plots
- `syn$species_truth`
  Synthetic ground truth for species
- `syn$community_profiles`
  Human-readable community descriptions

Important:

- `syn` itself is a **list**
- do **not** pass `vegmatrix = syn` to `cocktail_cluster()`
- use `syn$wide_matrix` for wide input
- use `syn$long_table` for long input

Correct synthetic-data path:

```r
res <- cocktail_cluster(
  vegmatrix = syn$wide_matrix,
  progress = FALSE,
  plot_values = "rel_cover",
  species_cluster_phi = TRUE,
  save_vegmatrix = TRUE
)
```


## 3. Option B: Load the Shipped Example Dataset

The package ships with CSV exports in `inst/extdata`.

### 3.1 Load the shipped wide CSV

```r
extdir <- system.file("extdata", package = "cocktailr")

veg_wide <- read.csv(
  file.path(extdir, "synthetic_vegetation_wide.csv"),
  row.names = 1,
  check.names = FALSE
)

vm <- as.matrix(veg_wide)
storage.mode(vm) <- "numeric"
```

Use `vm` as the input to `cocktail_cluster()`. If you want review cards
to remember where the data came from, later pass
`dataset_path = file.path(extdir, "synthetic_vegetation_wide.csv")` to
`cocktail_cluster()`.

### 3.2 Load the shipped long CSV

```r
extdir <- system.file("extdata", package = "cocktailr")

veg_long <- read.csv(
  file.path(extdir, "synthetic_vegetation_long.csv"),
  check.names = FALSE
)
```

Use `veg_long` with `input_format = "long"`.
If you want review cards to remember the file origin, later pass
`dataset_path = file.path(extdir, "synthetic_vegetation_long.csv")` to
`cocktail_cluster()`.

## 4. Option C: Load Your Own Wide Dataset

If your own vegetation table is already in wide format and stored as
CSV, the usual pattern is:

```r
veg_wide <- read.csv(
  "my_vegetation_wide.csv",
  row.names = 1,
  check.names = FALSE
)

vm <- as.matrix(veg_wide)
storage.mode(vm) <- "numeric"
```

When you cluster your own CSV-backed data, it is useful to also pass
the same file path as `dataset_path = "my_vegetation_wide.csv"` to
`cocktail_cluster()`. Then saved review cards can be grouped by dataset
automatically.

Requirements:

- rows = plots
- columns = species
- cell values = numeric cover or abundance values

If your plot IDs are not in the first CSV column, adjust the import
first so that the final object is still a plots x species matrix or data
frame.

## 5. Option D: Load Your Own Long Dataset

If your vegetation table is stored as plot-species-value rows:

```r
veg_long <- read.csv(
  "my_vegetation_long.csv",
  check.names = FALSE
)
```

The default expected column names are:

- `plot`
- `species`
- `value`

If your table already uses those names, you can pass it directly as long
input. If not, either rename the columns first or pass a custom mapping.

Example with custom column names:

```r
res <- cocktail_cluster(
  vegmatrix = veg_long,
  input_format = "long",
  long = list(
    plot = "PlotObservationID",
    species = "Harmonized_name_wfo",
    value = "Relative_cover"
  ),
  progress = FALSE,
  plot_values = "rel_cover",
  species_cluster_phi = TRUE,
  save_vegmatrix = TRUE,
  dataset_path = "my_vegetation_long.csv"
)
```

## 6. Run Cocktail Clustering

For labeling workflows, the recommended defaults are:

- `plot_values = "rel_cover"`
- `species_cluster_phi = TRUE`
- `save_vegmatrix = TRUE`
- `progress = FALSE`

If you generated data with `generate_synthetic_vegetation_data()`, the
most common next step is:

```r
res <- cocktail_cluster(
  vegmatrix = syn$wide_matrix,
  progress = FALSE,
  plot_values = "rel_cover",
  species_cluster_phi = TRUE,
  save_vegmatrix = TRUE
)
```

Do not use `vegmatrix = syn`, because `syn` is a list returned by the
generator, not a matrix or data frame.

### 6.1 Wide input

```r
res <- cocktail_cluster(
  vegmatrix = vm,
  progress = FALSE,
  plot_values = "rel_cover",
  species_cluster_phi = TRUE,
  save_vegmatrix = TRUE,
  dataset_path = "my_vegetation_wide.csv"
)
```

### 6.2 Long input

```r
res <- cocktail_cluster(
  vegmatrix = veg_long,
  input_format = "long",
  progress = FALSE,
  plot_values = "rel_cover",
  species_cluster_phi = TRUE,
  save_vegmatrix = TRUE,
  dataset_path = "my_vegetation_long.csv"
)
```

Why these defaults matter:

- `plot_values = "rel_cover"` makes plot support values more useful for
  later evidence interpretation
- `species_cluster_phi = TRUE` keeps species-cluster phi evidence for
  later labeling
- `save_vegmatrix = TRUE` keeps the aligned vegetation matrix inside the
  Cocktail object for downstream functions

## 7. Choose Cluster IDs to Label

The simplest default is:

```r
cluster_ids <- clusters_at_cut(res, phi_cut = 0.25)
cluster_ids
```

This returns cluster labels such as:

```r
[1] "c_1" "c_4" "c_9"
```

If you want a score-based alternative instead:

```r
cluster_ids <- select_clusters(
  x = res,
  min_phi = 0.20,
  min_k = 1,
  min_score = 0.3,
  mode = "strict",
  return = "labels"
)
```

For the rest of this document, we use one chosen cluster:

```r
cluster_id <- cluster_ids[1]
```

## 8. Build Evidence for One Cluster

```r
ev <- cluster_evidence(res, cluster = cluster_id)
print(ev)
```

Important parts to inspect:

- `ev$meta$cluster_id`
- `ev$context$cluster_metrics`
- `ev$summaries$species_topological`
- `ev$summaries$species_phi`
- `ev$summaries$plots_prototype`
- `ev$limitations`

This is the evidence bundle that will be passed to the model.

## 9. Run One-Step Labeling (Default)

The current default recommendation is one-step labeling:

```r
ans <- llm_label_cluster(
  evidence = ev,
  model = "gemma4:12b",
  variant = "strict_abstention_gate_v1",
  workflow_steps = 1,
  timeout_sec = 600,
  num_predict = 1200
)
```

What this does:

- uses the evidence bundle from `cluster_evidence()`
- calls the local Ollama model
- asks for structured JSON output
- does **not** save intermediate raw logs by default

Why these extra arguments are recommended for the first real run:

- the package-level defaults are stricter: `timeout_sec = 120` and
  `num_predict = 1200`
- on slower local hardware, the model may need more than 120 seconds to
  finish one structured response
- reducing `num_predict` while increasing `timeout_sec` is often a safer
  starting point than using the built-in defaults immediately

Short troubleshooting note:

- if you get an error like
  `Timeout was reached [localhost]: Operation timed out ... with 0 bytes received`,
  the model usually did not finish the full response before the HTTP
  timeout
- this is usually a local Ollama performance or model-loading issue,
  not a `cluster_evidence()` issue
- first try: `timeout_sec = 600` and `num_predict = 600` (or num_predict = 1200 if we have EOF error)

If you only want to inspect the request before calling the model:

```r
req <- llm_label_cluster(
  evidence = ev,
  model = "gemma4:12b",
  variant = "strict_abstention_gate_v1",
  workflow_steps = 1,
  dry_run = TRUE
)

names(req)
req$request$model
```

## 10. Read the Labeling Output

The most important output fields are:

```r
ans$output$status
ans$output$canonical_label
ans$output$display_label
ans$output$interpretation_summary
```

Meaning:

- `status`
  Either `labeled` or `abstain`
- `canonical_label`
  Machine-friendly snake_case label
- `display_label`
  Human-readable label for reports and review
- `interpretation_summary`
  Short natural-language explanation

The human-readable label is usually:

```r
ans$output$display_label
```

## 11. Validate the Output

```r
val <- validate_cluster_label(ans, ev)
print(val)
```

Important fields:

```r
val$validation_status
val$needs_human_review
val$evidence_coverage
val$issues
```

This step checks:

- required fields
- evidence ID integrity
- separation between data-backed claims and external knowledge
- unsupported ecological overreach

## 12. Save the Final Review Card to Disk

The recommended root location for final labeling artifacts is:

```r
temp/reports/cluster_reviews/
```

This is a generated-report location, not a stable package data
location. `render_cluster_review()` can create dataset-aware subfolders
there automatically.

Example:

```r
review <- render_cluster_review(
  x = ans,
  evidence = ev,
  validation = val,
  review_dir = file.path("temp", "reports", "cluster_reviews")
)

review$file
getwd()
```

Default saved file:

- `<cluster_id>_review.md`
  Compact human-review markdown card

The default compact card also records:

- the model used for generation
- the prompt file paths used for that answer

If dataset provenance is known:

- synthetic data from `generate_synthetic_vegetation_data()` already
  carry dataset metadata automatically
- real datasets can be grouped by file if you pass
  `dataset_path = ...` to `cocktail_cluster()`
- if no dataset can be identified, `render_cluster_review()` falls back
  to a timestamped folder

If you want the expanded card plus sidecar metadata JSON:

```r
review_full <- render_cluster_review(
  x = ans,
  evidence = ev,
  validation = val,
  review_dir = file.path("temp", "reports", "cluster_reviews"),
  full = TRUE
)

review_full$file
review_full$metadata_file
```

The markdown card is the main final artifact of the default workflow.

Important:

- the folder name is `cluster_reviews` (plural), not `cluster_review`
- if a local `cocktailr` source checkout is detected, relative
  `review_dir` values are resolved against that package root
- if no source checkout can be detected, relative `review_dir` still
  falls back to the current `getwd()`
- the most reliable way to find the written file is to inspect
  `review$file`

## 13. Run the Same Workflow for Multiple Clusters

Recommended high-level shortcut:

```r
run <- label_clusters(
  x = res,
  model = "gemma4:12b",
  variant = "strict_abstention_gate_v1",
  workflow_steps = 1,
  timeout_sec = 600,
  num_predict = 1200,
  review_dir = file.path("temp", "reports", "cluster_reviews")
)

run$summary
```

or other example with number of clusters: 
```r
run <- label_clusters(
  x = res,
  clusters = c("c_12", "c_35"),
  model = "gemma4:12b",
  variant = "strict_abstention_gate_v1",
  workflow_steps = 1,
  timeout_sec = 600,
  num_predict = 1200
)

run$summary
run$results$c_12$review$file
```

```r
run <- label_clusters(
  x = res,
  model = "qwen3.5:9b-q4_K_M",
  variant = "strict_abstention_gate_v1",
  workflow_steps = 2,
  timeout_sec = 600,
  num_predict = 1200,
  review_dir = file.path("temp", "reports", "cluster_reviews")
)
```

Notes:

- if you omit `clusters`, `label_clusters()` processes up to the first
  10 clusters selected by score
- by default `verbose = TRUE`, so the function prints short progress
  messages such as LLM start, retry/repair, EOF-triggered `num_predict`
  increase, and saved review paths
- use `verbose = FALSE` for quiet batch runs
- for each cluster it builds evidence, runs the LLM, validates the
  output, tries one validator-guided repair pass if needed, and saves a
  review card
- if no valid structured result is obtained after the bounded retry
  budget, it still writes a placeholder markdown card with the cluster,
  model, prompt provenance, and failure reason

other example:

```r
run <- label_clusters(
  x = res,
  clusters = c("c_12", "c_26"),
  model = "gemma4:12b",
  variant = "strict_abstention_gate_v1",
  workflow_steps = 1,
  timeout_sec = 600,
  num_predict = 1200
)

run$summary
run$results$c_12$review$file
```

Manual low-level loop:

```r
cluster_ids <- clusters_at_cut(res, phi_cut = 0.25)

for (cluster_id in cluster_ids) {
  ev <- cluster_evidence(res, cluster = cluster_id)

  ans <- llm_label_cluster(
    evidence = ev,
    model = "gemma4:12b",
    variant = "strict_abstention_gate_v1",
    workflow_steps = 1,
    timeout_sec = 600,
    num_predict = 1200
  )

  val <- validate_cluster_label(ans, ev)

  review <- render_cluster_review(
    x = ans,
    evidence = ev,
    validation = val,
    review_dir = file.path("temp", "reports", "cluster_reviews")
  )

  review$file
}
```

## 14. Advanced Options

These are **not** the default path, but you may need them later:

- different prompt variants:
  `concise_label_v1`, `conservative_interpretation_v1`,
  `abstain_first_v1`, `strict_abstention_gate_v1`
- two-step workflow:
  `workflow_steps = 2`
- raw run logging for debugging:
  `log_dir = ...`

For the current project default, keep:

- `workflow_steps = 1`
- no `log_dir`
- final saved review card only

## 15. Common Problems

### `could not find function "llm_label_cluster"`

You are probably using an old loaded package state.

Use:

```r
pkgload::load_all("path/to/cocktailr")
```

or reinstall the local package, then reload it.

### `vegmatrix must be a matrix or data.frame`

You probably passed:

- a file path string
- a list
- or another unsupported object

Load the dataset into R first with `read.csv()` or build a matrix/data
frame explicitly.

### `Timeout was reached ... with 0 bytes received`

This usually means:

- Ollama accepted the request
- the local model did not finish the full structured response within the
  current timeout window
- the HTTP request expired before any final response was returned

The most practical first fix is:

```r
ans <- llm_label_cluster(
  evidence = ev,
  model = "gemma4:12b",
  variant = "strict_abstention_gate_v1",
  workflow_steps = 1,
  timeout_sec = 600,
  num_predict = 1200
)
```

Also check:

- whether this was the first run after model load or after a long idle
  period
- whether `ollama ps` shows heavy CPU offload
- whether a smaller model answers faster on the same machine

### The long-format table is not recognized correctly

Make sure you used:

```r
input_format = "long"
```

and, if needed:

```r
long = list(plot = "...", species = "...", value = "...")
```

### The model is not available

Install it first in Ollama, for example:

```powershell
ollama pull gemma4:12b
```

## Related Documents

- [README.md](README.md)
  Short project overview and the default workflow only
- [llm_operation.md](llm_operation.md)
  Ollama setup, model selection, and troubleshooting
- [temp/README.md](temp/README.md)
  Policy for temporary generated artifacts and experimental assets
