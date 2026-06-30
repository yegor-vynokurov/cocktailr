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

## Quick Flow Example

This is the shortest recommended end-to-end run.

It:

- loads the current development version of the package
- generates a synthetic dataset
- runs Cocktail clustering
- labels the top score-ranked clusters with the current default
  `gemma4:12b` + `label_primary_v1` + `workflow_steps = 1`
- saves compact markdown review cards under
  `temp/reports/cluster_reviews/`

For Ollama installation and model setup, see
[llm_operation.md](llm_operation.md).

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
  variant = "label_primary_v1",
  workflow_steps = 1,
  timeout_sec = 600,
  num_predict = 2400,
  labels_for_imgs = TRUE
)
```

If `labels_for_imgs = TRUE`, `label_clusters()` also saves
`cluster_label_registry.csv` next to the review cards. You can then let
`cocktail_plot()` pick it up automatically with `label_registry = "auto"`.
The same saved registry can also relabel a base-R `hclust` object with
`label_hclust_leaves(hc, label_registry = "auto", x = res)`.
If you want the full `cluster_phi_dist() -> hclust() -> relabel -> plot`
workflow in one call, use `cluster_hclust_plot()`.

The current default recommendation is:

- one-step workflow: `workflow_steps = 1`
- model: `gemma4:12b`
- prompt variant: `label_primary_v1`
- safer first-run overrides on unknown local hardware:
  `timeout_sec = 600`, `num_predict = 2400`
- if the model tends to turn the label itself into a paragraph, try the
  staged mode: `workflow_steps = 3`
- if a weaker model needs a narrower decision space, try
  `label_mode = "constrained"`
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
  variant = "label_primary_v1",
  workflow_steps = 1,
  timeout_sec = 600,
  num_predict = 2400
)
```

What this does:

- uses the evidence bundle from `cluster_evidence()`
- calls the local Ollama model
- asks for structured JSON output
- does **not** save intermediate raw logs by default

Why these extra arguments are recommended for the first real run:

- the package-level defaults are stricter: `timeout_sec = 120` and
  `num_predict = 2400`
- on slower local hardware, the model may need more than 120 seconds to
  finish one structured response
- keeping `num_predict = 2400` while increasing `timeout_sec` is often a
  safer starting point than falling back to the short default timeout

Short troubleshooting note:

- if you get an error like
  `Timeout was reached [localhost]: Operation timed out ... with 0 bytes received`,
  the model usually did not finish the full response before the HTTP
  timeout
- this is usually a local Ollama performance or model-loading issue,
  not a `cluster_evidence()` issue
- first try: `timeout_sec = 600` and `num_predict = 2400`
- if structured output still hits EOF, `label_clusters()` now retries
  automatically at `4800` and then `9600`

## 9A. Run Three-Step Labeling (Optional)

If the model tends to blur the short label together with the explanation,
you can switch to the staged workflow:

```r
ans <- llm_label_cluster(
  evidence = ev,
  model = "gemma4:12b",
  variant = "label_primary_v1",
  workflow_steps = 3,
  label_mode = "dynamic",
  timeout_sec = 600,
  num_predict = 2400
)
```

What changes in this mode:

- stage A writes a freeform draft analysis without the final JSON schema
- stage B runs a short-label selection cascade:
  `label_primary_v1 -> label_soft_v1 -> label_broad_v1`
- stage C writes the final structured explanation while keeping the selected
  label fixed

Use this when:

- a weaker model keeps returning paragraph-like labels
- you want more open-ended reasoning before the final label is chosen
- you want the explanation pass to justify an already accepted label instead
  of renegotiating it
- `label_mode = "dynamic"` is useful here when you want Stage B to prefer
  short labels already proposed by Stage A instead of inventing a fresh one

If you only want to inspect the request before calling the model:

```r
req <- llm_label_cluster(
  evidence = ev,
  model = "gemma4:12b",
  variant = "label_primary_v1",
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
- label-shape constraints such as `display_label <= 80` characters,
  `display_label <= 6` words, forbidden punctuation, and
  `canonical_label <= 64` characters

If validation fails only because the label fields violate these format
limits, `label_clusters()` now uses a lightweight repair pass. That repair
reuses the parsed JSON plus validator feedback and does not resend the full
evidence bundle.

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
  variant = "label_primary_v1",
  workflow_steps = 1,
  timeout_sec = 600,
  num_predict = 2400,
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
  variant = "label_primary_v1",
  workflow_steps = 1,
  timeout_sec = 600,
  num_predict = 2400
)

run$summary
run$results$c_12$review$file
```

```r
run <- label_clusters(
  x = res,
  model = "qwen3.5:9b-q4_K_M",
  variant = "label_primary_v1",
  workflow_steps = 2,
  timeout_sec = 600,
  num_predict = 2400,
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
  variant = "label_primary_v1",
  workflow_steps = 1,
  timeout_sec = 600,
  num_predict = 2400
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
    variant = "label_primary_v1",
    workflow_steps = 1,
    timeout_sec = 600,
    num_predict = 2400
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

## 13A. Optional speculative fallback after a non-accepted strict result

The default workflow keeps:

```r
speculative_fallback_mode = "off"
```

If you want cautious orientation labels for clusters that either

- end in the placeholder / no-valid-label branch, or
- produce a strict valid `abstain`,

enable:

```r
run_spec <- label_clusters(
  x = res,
  clusters = c("c_12", "c_26"),
  model = "gemma4:12b",
  variant = "label_primary_v1",
  workflow_steps = 1,
  speculative_fallback_mode = "after_nonaccepted",
  timeout_sec = 600,
  num_predict = 2400,
  labels_for_imgs = TRUE
)

run_spec$summary[, c("cluster", "run_status", "label_tier", "review_status")]
```

What this mode does:

- accepted strict labels stay ordinary accepted labels
- if the strict pass abstains or would otherwise fall into the placeholder
  branch, `label_clusters()` starts a soft-label ladder automatically
- the current soft-label ladder uses:
  `phi4-mini:latest` + `ollama_options = list(num_ctx = 8192)` +
  `num_predict = 2400`
- the ladder tries `label_soft_v1` first
- if the soft rung still abstains, it escalates to the more label-forcing
  `label_broad_v1`
- successful fallback labels are marked as speculative and still require human review
- the final review card and registry also record:
  `label_origin`, `species_entropy_band`, `species_entropy_text`,
  `chaoticity_score`, and `chaoticity_label`

How the main controls are split:

- `speculative_fallback_mode`
  Controls whether the soft-label ladder is disabled (`"off"`), only used
  after the placeholder/rejection path (`"after_rejection"`), or also used
  after a valid strict abstain (`"after_nonaccepted"`).
- `model`
  Controls the strict first pass and, by default, the speculative fallback
  ladder too. If you start on one model, the fallback stays on that same
  model unless you explicitly override it with
  `options(cocktailr.speculative_fallback_model = "...")`. The soft ladder
  itself was developed and tuned primarily on `phi4-mini:latest`; this does
  not change the main project recommendation: for ordinary strict labeling,
  keep `gemma4:12b` as the baseline and treat smaller `phi4` models as
  experimental.
- `variant`
  Controls the strict first-pass prompt only. For the current recommended
  strict path, keep `variant = "label_primary_v1"`.
- `timeout_sec`
  Controls the request timeout for both the strict pass and the fallback
  ladder.
- `num_predict`
  Controls the strict pass. The fallback ladder uses its own larger internal
  default, currently `2400`.
- `prompt_budget_chars`
  Controls the character budget for the final system + user prompt messages.
  Default `10000`. If the evidence bundle is too large, lower-priority
  evidence blocks are trimmed first. The full JSON schema is still enforced
  separately through the structured-output `format` field.
- `labels_for_imgs`
  If `TRUE`, the resulting speculative or accepted labels are also exported
  into `cluster_label_registry.csv` for plotting helpers.

If you need to override the internal soft-ladder defaults, the current
workflow also supports R options:

```r
options(cocktailr.speculative_fallback_model = "phi4-mini:latest")
options(cocktailr.speculative_fallback_num_predict = 2400)
options(cocktailr.speculative_fallback_ollama_options = list(num_ctx = 8192))
```

Those overrides are optional. If you do nothing, the soft ladder inherits the
current `model` and any explicitly supplied `ollama_options`; otherwise it
falls back to:

- `ollama_options = list(num_ctx = 8192)`
- `num_predict = 2400`
- `label_soft_v1` first, then `label_broad_v1`

Advanced note:

- `label_mode = "constrained"` now uses the packaged coarse vocabulary
  directly
- if you want to test a dataset-specific label list, set:

```r
options(
  cocktailr.cluster_label_vocabulary_path =
    "path/to/your/custom_vocabulary.json"
)
```

Typical display semantics:

- accepted: `c_12: Mixed Deciduous Woodland`
- speculative: `c_27: Woodland-transition assemblage*`
- plot footnote: `* tentative / speculative label; strict validation did not accept a stable evidence-backed label`

If you also saved a plotting registry:

```r
cocktail_plot(
  x = res,
  clusters = run_spec$summary$cluster,
  label_clusters = TRUE,
  label_registry = "auto"
)
```

The dendrogram still keeps stable numeric IDs on the plot itself. The starred
human-readable label is shown in the legend / caption layer or in relabeled
`hclust` leaves.

## 13B. Optional semantic indicator enrichment

You can also enrich the evidence bundle before the LLM call with
indicator-derived ecological axes from the external EIVE/Tichy tables.

Example:

```r
run_sem <- label_clusters(
  x = res,
  clusters = c("c_12", "c_26"),
  model = "gemma4:12b",
  variant = "label_primary_v1",
  workflow_steps = 1,
  semantic_layer = TRUE,
  semantic_root = "D:/documents/coctrailr/cocktailr",
  timeout_sec = 600,
  num_predict = 2400
)

run_sem$summary[, c(
  "cluster",
  "semantic_layer_used",
  "semantic_layer_status",
  "semantic_layer_error"
)]
```

What this does:

- keeps the ordinary `cluster_evidence()` facts intact
- adds a semantic indicator profile with ecological axes such as light,
  moisture, reaction, nutrients, temperature, and salinity
- injects those semantic summaries into the final evidence text that is
  sent to the model

Current semantic-layer arguments on `label_clusters()`:

- `semantic_layer`
  Turns the auxiliary enrichment on or off. Default is `FALSE`.
- `semantic_root`
  Optional project-root override used to find `data-raw/external/` and
  `cache/semantic_layer/`.
- `semantic_min_phi`
  Optional species-filter threshold forwarded to the semantic scorer.
- `semantic_bootstrap`
  Bootstrap size for the semantic axis summaries. Default is `200`.
- `semantic_force_species`
  Rebuild the species lookup cache for the requested species.
- `semantic_force_reference`
  Re-read the source workbooks and rebuild the combined reference cache.

Practical notes:

- the semantic layer is auxiliary ecological context, not formal habitat
  proof
- it expects the EIVE/Tichy source workbooks under `data-raw/external/`
  and uses `readxl` when available, otherwise the packaged `xml2`
  fallback reader
- if enrichment fails, the workflow does not stop; it records
  `semantic_layer_status = "failed"` and continues with the plain
  evidence bundle
- the current local smoke run with `phi4-mini:latest` showed that the
  semantic layer was ingested successfully on several synthetic datasets,
  but the small model still tended to abstain
- for ordinary cautious runs, `gemma4:12b` remains the main recommended
  baseline even when semantic enrichment is enabled

## 14. Advanced Options

These are **not** the default path, but you may need them later:

- different prompt variants:
  `label_primary_v1`, `label_soft_v1`, `label_broad_v1`
- older versioned prompt IDs are still accepted as compatibility aliases,
  but they now resolve to the public three-prompt set
- two-step workflow:
  `workflow_steps = 2`
- staged draft -> label -> explanation workflow:
  `workflow_steps = 3`
- label-space modes:
  `label_mode = "open"`, `"constrained"`, `"dynamic"`
- speculative fallback ladder after strict abstain or rejection:
  `speculative_fallback_mode = "after_nonaccepted"`
- raw run logging for debugging:
  `log_dir = ...`

Migration note:

- do not manually browse the old `v1-v9` prompt ladder anymore
- the supported public choices are now only
  `label_primary_v1`, `label_soft_v1`, `label_broad_v1`
- legacy IDs still work as compatibility aliases
- retired prompt texts are archived locally under
  `temp/prompt_archive/cluster_labeling/`

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
  variant = "label_primary_v1",
  workflow_steps = 1,
  timeout_sec = 600,
  num_predict = 2400
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
