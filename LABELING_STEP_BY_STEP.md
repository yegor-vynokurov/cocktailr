# Labeling Step by Step

This document describes the current cluster-labeling workflow in
`cocktailr`.

The public pipeline is now one fixed route:

1. build deterministic cluster evidence
2. optionally run a short brainstorm pass
3. run a lightweight label-selection step
4. run a plain-text explanation step
5. assemble the final output in code

There is no longer a recommended one-step vs three-step choice. The
pipeline is always staged; `use_brainstorm` only decides whether the
first draft-analysis step runs.

## Quick Flow

This is the shortest recommended end-to-end run:

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
  timeout_sec = 600,
  num_predict = 2400,
  use_brainstorm = TRUE,
  labels_for_imgs = TRUE
)
```

What this does:

- builds evidence for the selected clusters
- runs the fixed local LLM pipeline
- writes review cards under `temp/reports/cluster_reviews/`
- saves `cluster_label_registry.csv` next to those review cards when
  `labels_for_imgs = TRUE`

## What the Model Actually Has To Do

The model is deliberately kept on a short leash.

Step 1, brainstorm:

- optional
- plain text only
- asks for a few interpretations, conflicts, candidate labels, and
  things not to overclaim

Step 2, selection:

- always lightweight JSON
- the model only decides between:
  `canonical_label`, `display_label`, `label_summary`,
  `abstain_reason`
- the model does not fill `status`
- the model does not build the final report structure

Step 3, explanation:

- plain text only
- explains the already fixed selection result

The final assembled object is built in code. If the model abstains on
all three selection rungs, the internal result stays an honest
abstention; downstream review and registry artifacts can still expose a
separate public fallback such as `Chaotic Cluster`.

## 1. Load Or Generate Data

`cocktail_cluster()` does not read a file path directly. First load data
into R, then pass the matrix or data frame.

### Synthetic data

```r
syn <- generate_synthetic_vegetation_data(seed = 42)
```

Use:

- `syn$wide_matrix` for wide input
- `syn$long_table` for long input

### Wide CSV

```r
veg_wide <- read.csv(
  "my_vegetation_wide.csv",
  row.names = 1,
  check.names = FALSE
)

vm <- as.matrix(veg_wide)
storage.mode(vm) <- "numeric"
```

### Long CSV

```r
veg_long <- read.csv(
  "my_vegetation_long.csv",
  check.names = FALSE
)
```

Expected default columns:

- `plot`
- `species`
- `value`

If your names differ, pass a custom `long = list(...)` mapping to
`cocktail_cluster()`.

## 2. Run Cocktail Clustering

Recommended defaults for labeling workflows:

```r
res <- cocktail_cluster(
  vegmatrix = syn$wide_matrix,
  progress = FALSE,
  plot_values = "rel_cover",
  species_cluster_phi = TRUE,
  save_vegmatrix = TRUE
)
```

Why these matter:

- `plot_values = "rel_cover"` keeps plot support values informative
- `species_cluster_phi = TRUE` preserves phi-ranked species evidence
- `save_vegmatrix = TRUE` keeps aligned vegetation data for later
  evidence summaries

## 3. Choose Clusters To Label

Simple score-based selection:

```r
cluster_ids <- select_clusters(
  x = res,
  min_phi = 0.20,
  min_k = 1,
  min_score = 0.3,
  mode = "strict",
  return = "labels"
)

cluster_id <- cluster_ids[1]
```

## 4. Build Evidence

```r
ev <- cluster_evidence(
  res,
  cluster = cluster_id,
  top_n_phi = 10,
  n_prototype_plots = 5,
  n_borderline_plots = 5
)
```

Useful fields to inspect:

- `ev$meta$cluster_id`
- `ev$summaries$species_topological`
- `ev$summaries$species_phi`
- `ev$summaries$plots_prototype`
- `ev$limitations`

## 5. Optional User-Added Data

You can attach lightweight extra material to the evidence bundle with
`user_added_data`.

Supported inputs:

- an in-memory R object
- one file
- one directory scanned non-recursively

Supported file extensions:

- `.txt`
- `.json`
- `.yaml`
- `.yml`

Examples:

```r
ev <- cluster_evidence(
  res,
  cluster = cluster_id,
  user_added_data = "notes/site_notes.txt"
)
```

```r
ev <- cluster_evidence(
  res,
  cluster = cluster_id,
  user_added_data = "notes/cluster_context/"
)
```

Rules:

- files are included as raw text only
- supported files in a directory are loaded in stable filename order
- unsupported extensions produce a warning and are skipped
- missing directories produce a warning and the workflow continues
- empty or unsupported-only directories also produce a warning and the
  workflow continues
- the combined payload is truncated deterministically at 1000 characters

In prompts, this block appears as `User-added data:`. The code does not
try to interpret it for the model.

## 6. Run One Cluster Through The Fixed Pipeline

```r
ans <- llm_label_cluster(
  evidence = ev,
  model = "gemma4:12b",
  variant = "label_primary_v1",
  timeout_sec = 600,
  num_predict = 2400,
  use_brainstorm = TRUE
)
```

The default selection ladder is:

- `label_primary_v1`
- `label_soft_v1`
- `label_broad_v1`

If the model returns malformed or incomplete lightweight selection JSON,
the pipeline gives that rung one local repair cycle before moving on.

If you want the model to skip the draft-analysis step:

```r
ans <- llm_label_cluster(
  evidence = ev,
  model = "gemma4:12b",
  variant = "label_primary_v1",
  use_brainstorm = FALSE,
  timeout_sec = 600,
  num_predict = 2400
)
```

## 7. Run Batch Labeling

For end-to-end work, `label_clusters()` is usually the better entry
point:

```r
run <- label_clusters(
  x = res,
  clusters = cluster_ids,
  model = "gemma4:12b",
  variant = "label_primary_v1",
  timeout_sec = 600,
  num_predict = 2400,
  use_brainstorm = TRUE,
  labels_for_imgs = TRUE,
  review_dir = file.path("temp", "reports", "cluster_reviews")
)
```

Useful optional switches:

- `use_brainstorm = FALSE`
  Skip the draft-analysis step and go directly to selection plus
  explanation.
- `label_mode = "constrained"`
  Inject the packaged coarse vocabulary into the selection prompt.
- `user_added_data = ...`
  Attach raw user material to each cluster evidence bundle.
- `semantic_layer = TRUE`
  Add semantic enrichment when you have the optional semantic resources
  prepared.

## 8. Constrained Label Mode

Use this when you want the model to choose from a controlled vocabulary
instead of inventing a fresh label.

```r
run <- label_clusters(
  x = res,
  clusters = cluster_ids,
  model = "phi4-mini:latest",
  variant = "label_primary_v1",
  label_mode = "constrained",
  timeout_sec = 600,
  num_predict = 2400
)
```

The selection prompt will include:

- `Constrained label mode is active.`
- `Allowed labels:`

You can override the packaged vocabulary with:

```r
options(cocktailr.cluster_label_vocabulary_path = "path/to/custom_vocab.json")
```

## 9. Inspect The Result

For a single-cluster call:

```r
ans$output$status
ans$output$canonical_label
ans$output$display_label
ans$output$label_summary
ans$output$abstain_reason
ans$output$explanation
```

For a batch run:

```r
run$summary
run$results[[1]]$review$file
run$label_registry_file
```

What to look at:

- `status`
  `labeled` or `abstain`
- `canonical_label`
  machine-friendly snake_case label
- `display_label`
  human-readable label
- `label_summary`
  short explanation of what the label means
- `abstain_reason`
  why the model declined to name the cluster
- `explanation`
  compact final explanation assembled after the selection step

## 10. Review Cards And Plotting Registry

`label_clusters()` writes compact markdown review cards. When
`labels_for_imgs = TRUE`, it also saves `cluster_label_registry.csv`.

Typical places to inspect:

- `run$summary$review_file`
- `run$label_registry_file`

That registry can be reused in plotting helpers such as:

- `cocktail_plot(..., label_registry = "auto")`
- `label_hclust_leaves(..., label_registry = "auto")`
- `cluster_hclust_plot(..., label_registry = "auto")`

## 11. Honest Abstain vs Public Fallback

If all three selection rungs abstain:

- the internal final output stays `status = "abstain"`
- `canonical_label`, `display_label`, and `label_summary` stay `NULL`
- `abstain_reason` and `explanation` stay populated

Downstream artifacts may also expose separate public fallback fields:

- `public_canonical_label`
- `public_display_label`
- `public_label_source`

This is intentional. It keeps the model output honest while still giving
plots and summary tables a usable public label.

## 12. Deprecated Knobs

These older switches are no longer part of the recommended workflow:

- `workflow_steps`
  Still accepted as a compatibility argument, but the active pipeline is
  always fixed to three stages.
- `label_mode = "dynamic"`
  Deprecated. Draft-derived candidate labels are now injected into
  selection prompts automatically, so the runtime treats this like
  `open`.
- speculative strict-vs-soft branching as the main public workflow
  No longer the recommended user path.

## 13. Dry Runs

If you only want to inspect the assembled prompts and request payloads:

```r
req <- llm_label_cluster(
  evidence = ev,
  model = "gemma4:12b",
  variant = "label_primary_v1",
  dry_run = TRUE
)

names(req)
req$workflow$label$variants[[1]]$request
```

This is useful when you want to verify:

- the fixed pipeline stages
- constrained vocabulary injection
- `User-added data:` prompt content
- prompt budget trimming
