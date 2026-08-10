# Labeling Step by Step

This is the official end-to-end guide for running cluster labeling in
`cocktailr` on a real dataset with a local Ollama model such as
`phi4-mini:latest`.

The goal is practical, not experimental:

1. prepare one vegetation dataset
2. run Cocktail clustering
3. choose clusters to label
4. run a smoke test on one cluster
5. run full batch labeling
6. inspect review cards, summaries, logs, and plots

This guide is written for users who are not already familiar with LLM
settings. It uses one clear path and keeps optional knobs to a minimum.

## What This Guide Assumes

- You have a local `cocktailr` source checkout.
- You can run PowerShell and R on the same machine.
- Ollama is installed locally.
- The model you want to use is available in `ollama list`, or you can
  download it with `ollama pull`.

The shell examples below use PowerShell on Windows. If your local paths
are different, change `root` and the dataset path once at the top.

## The Few Settings You Actually Need To Care About

For a first real run, only these settings normally matter:

- `MODEL_NAME`
  The Ollama model name, for example `"phi4-mini:latest"`.
- `OLLAMA_OPTIONS$num_ctx`
  The Ollama context window. Start with `8192L` if the model supports
  it; use `4096L` if you need a smaller run.
- `PROMPT_BUDGET_CHARS`
  How much evidence text is allowed into the assembled prompt.
- `NUM_PREDICT`
  How much output the model may generate.
- `TIMEOUT_SEC`
  How long one LLM call may take before timing out.

For a first run, do not change these:

- `variant = "label_primary_v1"`
- `label_mode = "open"`
- `use_brainstorm = TRUE`
- `semantic_layer = TRUE`
- `life_form_layer = "complex"`
- `structured_prompt = TRUE`
- `represented_species = 5L`
- `workflow_steps`
- `internal_prompt_version`
- `speculative_fallback_mode`

The staged public workflow already knows how to use the main prompt
ladder. You do not need to manually chain `label_soft_v1` or
`label_broad_v1`.

Leave `short_label_with_llm = FALSE` for normal runs. If you turn it on,
the extra shortening-repair prompt is available only in internal prompt
bundles that include it, such as the packaged `v2` bundle.

This guide recommends the current bounded ordinary-labeling profile rather
than the raw function defaults:

- `semantic_layer = TRUE`
- `life_form_layer = "complex"`
- `structured_prompt = TRUE`
- `represented_species = 5L`

The function API still keeps broader defaults for backward compatibility,
but this guide now opts into the current synopsis path on purpose.

This guide uses `semantic_layer = TRUE` by default because ecological
indicator evidence usually improves cluster interpretation when the
optional semantic resources are available. The run still continues without
semantic enrichment if those resources cannot be built; check
`summary$semantic_layer_status` after the run.

This guide uses `life_form_layer = "complex"` by default because the
current term-light synopsis path converts the richer species-first
life-form overlay into a compact structural summary without exposing the
raw specialist terms directly to the model. After the run, check
`summary$life_form_layer_status` and `summary$life_form_layer_mode`.

This guide uses `represented_species = 5L` as the practical starting
point, not as a universal truth. In the bounded Phase C ladder, `5`
preserved a species list in the prompt while staying shorter than `7` and
much shorter than `12`, but the resulting label quality still depended
strongly on the dataset and the model. Treat `5L` as the default starting
configuration for a new dataset, then vary it experimentally and inspect
the label quality before you freeze a project workflow.

Use these alternatives deliberately:

- `represented_species = 0L` when the cluster summary is already strong
  and the species rows seem to distract the model or overfill the prompt.
- `represented_species = 7L` when five species feel too sparse for a
  dataset with many diagnostic co-dominants or transitions.
- `represented_species = 12L` only as an explicit large-context
  comparison; it carries the most species detail but also the largest
  prompt growth and, in the bounded ladder, did not produce a stability
  advantage.
- `life_form_layer = "simple"` when you want a lighter coarse structural
  hint but do not need the newer species-first overlay path.
- `life_form_layer = NULL` for ablation, legacy comparison, or when you
  intentionally want to measure semantic-only behavior.
- `semantic_layer = FALSE` when the external semantic inputs are missing or
  when you intentionally want to isolate the contribution of life-form
  evidence.
- `structured_prompt = FALSE` only for legacy-path comparison runs; this
  guide does not recommend it as the ordinary default anymore.

## Where The Life-Form Prompt Data Comes From

In the current recommended ordinary-labeling path, the life-form summary is
not invented by the LLM. It is assembled programmatically from:

- the external life-form workbook
  `data-raw/external/life_forms/Life_form.xlsx`
- the packaged base life-form dictionary
  `inst/extdata/life_form_dictionary_v1.csv`
- the packaged overlay interpretation dictionary
  `inst/extdata/life_form_overlay_dictionary_v1.csv`

There is also a third packaged dictionary file:

- `inst/extdata/life_form_glossary_dictionary_v1.csv`

That glossary dictionary stores short definitions and source links for raw
life-form terms such as `Therophyte` or `Phanerophyte`. It is useful for
term-definition flows and prompt-inspection work, but the current
recommended term-light synopsis path does not depend on pushing those raw
specialist terms directly into the model prompt.

Use this mental model:

- your current dataset supplies the cluster species list and species ranks
- the workbook supplies the reference life-form assignments
- `life_form_dictionary_v1.csv` tells the code which workbook flags belong
  to which named life-form labels
- `life_form_overlay_dictionary_v1.csv` converts derived metrics such as
  dominance share, mixing, coverage gaps, and compression into short bucket
  phrases that are later turned into the compact life-form summary

Short provenance notes:

- The workbook-backed life-form assignments are external reference data
  reused across datasets; in code they are treated as the source layer for
  species-to-life-form matching rather than as something learned from the
  current run.
- The packaged base dictionary
  `inst/extdata/life_form_dictionary_v1.csv`
  is a project mapping layer for the workbook columns. It defines the raw
  flags the code recognizes, their human-readable labels, and their
  priority order.
- The packaged overlay dictionary
  `inst/extdata/life_form_overlay_dictionary_v1.csv`
  is an internal interpretation table created for this project. Its metric
  buckets are based on the delivered overlay metrics and were tuned on the
  bounded real-data forest-steppe experiments used in `CHG-0017` and
  `CHG-0018`; it is not an external taxonomic standard.
- The glossary dictionary
  `inst/extdata/life_form_glossary_dictionary_v1.csv`
  is literature/web-reference-backed. In the current packaged file, the
  definitions cite Simple English Wikipedia or Wiktionary for `Tree` and
  `Shrub`, and the Raunkiaer life-form article on Wikipedia for
  `Chamaephyte`, `Geophyte`, `Hemicryptophyte`, `Phanerophyte`,
  `Therophyte`, and related terms.

The plain-language structural cues used in the current term-light synopsis
rewrite were additionally grounded in the bounded `T019a` design notes
under:

- `temp/reports/forest_steppe_single_model_labeling/chg0018_compact_synopsis_builder_v1_t019a_pre_phase_c_life_form_summary_rerun_live_2026_08_10/analysis/synopsis_design_decisions.md`

Those notes distinguish literature-backed ecological cues from internal
heuristic wording for assignment clarity.

## What Gets Written Automatically vs What This Guide Saves Explicitly

`label_clusters()` automatically writes:

- markdown review cards under `review_dir`
- `cluster_label_registry.csv` next to those review cards when
  `labels_for_imgs = TRUE`

This guide also saves a few convenience files explicitly so that the run
is easy to reopen later:

- `..._run.rds`
- `..._summary.csv`
- `..._label_registry_copy.csv`
- plot PNG files

## Final Folder Layout

This guide keeps every artifact for one run under one timestamped
folder:

```text
temp/reports/forest_steppe_single_model_labeling/<timestamp>/
  input/
  clustering/
  runs/
    <MODEL_SLUG>/
      smoke/
      full/
  plots/
  evaluation/
  session/
```

The most useful outputs at the end are:

- `clustering/selected_clusters.csv`
- `runs/<MODEL_SLUG>/full/<MODEL_SLUG>_run.rds`
- `runs/<MODEL_SLUG>/full/<MODEL_SLUG>_summary.csv`
- `runs/<MODEL_SLUG>/full/cluster_reviews/...`
- `runs/<MODEL_SLUG>/full/llm_logs/...` if `debug = TRUE`
- `runs/<MODEL_SLUG>/full/<MODEL_SLUG>_label_registry_copy.csv`
- `plots/<MODEL_SLUG>_cluster_hclust_labels.png`
- `plots/<MODEL_SLUG>_full_cocktail_plot_page01.png`
- `plots/<MODEL_SLUG>_full_cocktail_plot_page02.png` and more if needed

Important: in this guide we keep `review_dir` inside the run folder, so
for plotting we pass `run$label_registry` explicitly. Do not rely on
`label_registry = "auto"` here.

## 1. One-Time Model Check In PowerShell

1) install the Ollama https://ollama.com/download/ locally

Open PowerShell and confirm that Ollama is available:

```powershell
ollama --version
ollama list
ollama pull phi4-mini:latest
```
first command check the version of the ollama on your pc
second command show list of the available models on your pc. If it is first downloading, the list will be empty. 
third command initiates the downloading the chosen model to the your pc. Model phi4-mini:latest is approximatelly 2.5 Gb. 

If you want another model, pull that model instead and later change
`MODEL_NAME` in R.

## 2. Step 1 In PowerShell: Create A Run Folder

```powershell
Set-Location D:\documents\coctrailr\cocktailr

$stamp = Get-Date -Format "yyyy-MM-dd_HHmmss"

$BASE_REPORT_DIR = "temp/reports/forest_steppe_single_model_labeling"
$REPORT_ROOT = "$BASE_REPORT_DIR/$stamp"
$env:REPORT_ROOT = $REPORT_ROOT

$REPORT_ROOT_FILE = "$BASE_REPORT_DIR/_ACTIVE_REPORT_ROOT.txt"

New-Item -ItemType Directory -Force -Path $BASE_REPORT_DIR | Out-Null
$REPORT_ROOT | Out-File $REPORT_ROOT_FILE -Encoding utf8

New-Item -ItemType Directory -Force -Path `
  "$REPORT_ROOT/input", `
  "$REPORT_ROOT/clustering", `
  "$REPORT_ROOT/runs", `
  "$REPORT_ROOT/plots", `
  "$REPORT_ROOT/evaluation", `
  "$REPORT_ROOT/session" | Out-Null

ollama list | Out-File "$REPORT_ROOT/session/ollama_list.txt" -Encoding utf8
git rev-parse HEAD | Out-File "$REPORT_ROOT/session/git_commit.txt" -Encoding utf8
git status --short | Out-File "$REPORT_ROOT/session/git_status.txt" -Encoding utf8

Write-Host "Active REPORT_ROOT: $REPORT_ROOT"
Write-Host "Marker file: $REPORT_ROOT_FILE"
```

Why this step matters:

- every run gets its own folder
- later R steps can rediscover the active run folder
- environment details are saved up front

## 3. Step 2 In R: Load The Package And Fix The Main Settings

Open `cocktailr.Rproj` in RStudio, or start R in the project root.

```r
root <- normalizePath(
  "D:/documents/coctrailr/cocktailr",
  winslash = "/",
  mustWork = TRUE
)

report_root_file <- file.path(
  root,
  "temp",
  "reports",
  "forest_steppe_single_model_labeling",
  "_ACTIVE_REPORT_ROOT.txt"
)

report_root_rel <- Sys.getenv("REPORT_ROOT", unset = "")

if (!nzchar(report_root_rel) && file.exists(report_root_file)) {
  report_root_rel <- trimws(readLines(
    report_root_file,
    warn = FALSE,
    encoding = "UTF-8"
  )[1])
}

if (!nzchar(report_root_rel)) {
  stop(
    "REPORT_ROOT is not set and _ACTIVE_REPORT_ROOT.txt was not found. Run the PowerShell step first.",
    call. = FALSE
  )
}

Sys.setenv(REPORT_ROOT = report_root_rel)

report_root <- normalizePath(
  file.path(root, report_root_rel),
  winslash = "/",
  mustWork = FALSE
)

dir.create(file.path(report_root, "session"), recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop(
    "Please install the 'pkgload' package once with install.packages(\"pkgload\").",
    call. = FALSE
  )
}

pkgload::load_all(path = root, quiet = TRUE)

writeLines(
  capture.output(sessionInfo()),
  file.path(report_root, "session", "R_sessionInfo.txt")
)

MODEL_NAME <- "phi4-mini:latest"
MODEL_SLUG <- gsub(":", "_", MODEL_NAME)
MODEL_SLUG <- gsub("[^A-Za-z0-9_-]+", "_", MODEL_SLUG)
MODEL_SLUG <- sub("^_+", "", MODEL_SLUG)
MODEL_SLUG <- sub("_+$", "", MODEL_SLUG)

OLLAMA_OPTIONS <- list(
  num_ctx = 8192L
)

PROMPT_BUDGET_CHARS <- 10000L
NUM_PREDICT <- 2400L
TIMEOUT_SEC <- 600L

MODEL_NAME
MODEL_SLUG
report_root
```

If you installed `cocktailr` as a regular R package instead of working
from a source checkout, you can replace the `pkgload::load_all(...)`
line with:

```r
library(cocktailr)
```

If package loading fails because of a missing R dependency, install that
dependency once and rerun this step.

## 4. Step 3: Prepare The Real Dataset

This example uses the forest-steppe species table:

```text
D:/documents/coctrailr/cocktailr/data-raw/external/forest_steppe_chytry_2021/Chytry-Krystof_forest-steppe-v1_2021-05-24_SPE.csv
```

Expected input columns:

- `PLOT_ID`
- `TAXON`
- `LAYER`
- `COVER`

The LLM labeling workflow does not need the original site metadata here.
We only prepare a long table with:

- anonymized plot IDs
- species names
- numeric cover values

```r
dir.create(file.path(report_root, "input"), recursive = TRUE, showWarnings = FALSE)

spe_path <- file.path(
  root,
  "data-raw",
  "external",
  "forest_steppe_chytry_2021",
  "Chytry-Krystof_forest-steppe-v1_2021-05-24_SPE.csv"
)

if (!file.exists(spe_path)) {
  stop("SPE.csv was not found at: ", spe_path, call. = FALSE)
}

spe_raw <- utils::read.csv(
  spe_path,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

required_cols <- c("PLOT_ID", "TAXON", "LAYER", "COVER")
missing_cols <- setdiff(required_cols, names(spe_raw))

if (length(missing_cols)) {
  stop(
    "SPE.csv is missing required columns: ",
    paste(missing_cols, collapse = ", "),
    call. = FALSE
  )
}

cover_map <- c(
  "r" = 0.1,
  "+" = 0.5,
  "1" = 2.5,
  "m" = 4.0,
  "a" = 8.75,
  "b" = 18.75,
  "3" = 37.5,
  "4" = 62.5,
  "5" = 87.5
)

unknown_cover <- setdiff(unique(spe_raw$COVER), names(cover_map))

if (length(unknown_cover)) {
  stop(
    "Unknown COVER code(s): ",
    paste(unknown_cover, collapse = ", "),
    call. = FALSE
  )
}

plot_ids <- sort(unique(spe_raw$PLOT_ID))

plot_lookup <- data.frame(
  PLOT_ID = plot_ids,
  plot = sprintf("plot_%03d", seq_along(plot_ids)),
  stringsAsFactors = FALSE
)

spe_sanitized <- merge(
  spe_raw[, c("PLOT_ID", "TAXON", "COVER")],
  plot_lookup,
  by = "PLOT_ID",
  all.x = TRUE,
  sort = FALSE
)

spe_sanitized$species <- spe_sanitized$TAXON
spe_sanitized$value <- unname(cover_map[spe_sanitized$COVER])

veg_long <- aggregate(
  value ~ plot + species,
  data = spe_sanitized[, c("plot", "species", "value")],
  FUN = sum
)

veg_long$value <- pmin(veg_long$value, 100)
veg_long <- veg_long[order(veg_long$plot, veg_long$species), ]
rownames(veg_long) <- NULL

prep_path <- file.path(report_root, "input", "veg_long_numeric_eval_v1.csv")

utils::write.csv(
  veg_long,
  prep_path,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

utils::write.csv(
  plot_lookup,
  file.path(report_root, "input", "plot_id_lookup_private.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

utils::write.csv(
  data.frame(
    raw_code = names(cover_map),
    numeric_value = unname(cover_map),
    stringsAsFactors = FALSE
  ),
  file.path(report_root, "input", "cover_conversion_table.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

head(veg_long)
```

Files written here:

- `input/veg_long_numeric_eval_v1.csv`
- `input/plot_id_lookup_private.csv`
- `input/cover_conversion_table.csv`

## 5. Step 4: Run Cocktail Clustering

You do not need to convert the long table to wide format manually.
`cocktail_cluster()` can read long input directly.

```r
dir.create(file.path(report_root, "clustering"), recursive = TRUE, showWarnings = FALSE)

res <- cocktail_cluster(
  vegmatrix = veg_long,
  input_format = "long",
  long = list(plot = "plot", species = "species", value = "value"),
  progress = FALSE,
  plot_values = "rel_cover",
  species_cluster_phi = TRUE,
  save_vegmatrix = TRUE,
  dataset_path = prep_path,
  dataset_label = "real_eval_long_numeric_v1",
  dataset_type = "real"
)

res_path <- file.path(report_root, "clustering", "res_real_eval_long_numeric_v1.rds")
saveRDS(res, res_path)

res_path
```

Why these settings matter:

- `input_format = "long"` uses the prepared long table directly
- `plot_values = "rel_cover"` keeps cover-aware plot summaries
- `species_cluster_phi = TRUE` keeps phi-based species evidence
- `save_vegmatrix = TRUE` keeps the aligned vegetation matrix for later
  evidence summaries

## 6. Step 5: Choose Which Clusters To Label

Start with the default strict selection rule:

```r
selection_table <- select_clusters(
  x = res,
  min_phi = 0.2,
  min_k = 1L,
  min_score = 1,
  mode = "strict",
  return = "table"
)

selected_clusters <- selection_table$cluster

utils::write.csv(
  selection_table,
  file.path(report_root, "clustering", "selected_clusters_table.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

utils::write.csv(
  data.frame(cluster = selected_clusters, stringsAsFactors = FALSE),
  file.path(report_root, "clustering", "selected_clusters.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

selection_table
length(selected_clusters)
```

What to look at:

- `selected_clusters.csv`
  The simple reusable cluster list.
- `selected_clusters_table.csv`
  The same selection with `h`, `k`, `m`, and `score`.

If you already know which clusters you want, you can replace
`selected_clusters` later with something like:

```r
selected_clusters <- c("c_12", "c_27", "c_35")
```

Optional baseline plot before labeling:

```r
dir.create(file.path(report_root, "plots"), recursive = TRUE, showWarnings = FALSE)

cluster_hclust_plot(
  x = res,
  clusters = selected_clusters,
  label_leaves = FALSE,
  file = file.path(report_root, "plots", "baseline_cluster_hclust_ids.png")
)
```

## 7. Step 6: Optional Dry Run On One Cluster

This step is useful when you want to inspect the assembled request
before making a real Ollama call.

```r
dry_cluster <- selected_clusters[[1]]

ev <- cluster_evidence(
  x = res,
  cluster = dry_cluster,
  top_n_phi = 10L,
  n_prototype_plots = 5L,
  n_borderline_plots = 5L
)

dry <- llm_label_cluster(
  evidence = ev,
  model = MODEL_NAME,
  variant = "label_primary_v1",
  label_mode = "open",
  use_brainstorm = TRUE,
  timeout_sec = TIMEOUT_SEC,
  num_predict = NUM_PREDICT,
  prompt_budget_chars = PROMPT_BUDGET_CHARS,
  ollama_options = OLLAMA_OPTIONS,
  dry_run = TRUE
)

saveRDS(
  dry,
  file.path(report_root, "session", paste0(MODEL_SLUG, "_dry_run.rds"))
)

names(dry)
dry$request$model
dry$request$options
```

Dry run is optional, but it is often the fastest way to confirm that:

- the model name is correct
- `num_ctx` is being passed
- the prompt is assembled successfully

Dry run does not write review cards and does not contact Ollama.
The `semantic_layer` and `life_form_layer` switches belong to
`label_clusters()`, which builds and optionally enriches evidence before
calling the model. In this direct `llm_label_cluster()` dry run, you are
passing the evidence object yourself.

## 8. Step 7: Smoke Test On One Cluster (optional)

Do this before a full batch run.

```r
model_root <- file.path(report_root, "runs", MODEL_SLUG)
smoke_root <- file.path(model_root, "smoke")

dir.create(smoke_root, recursive = TRUE, showWarnings = FALSE)

smoke_cluster <- selected_clusters[[1]]

smoke_run <- label_clusters(
  x = res,
  clusters = smoke_cluster,
  model = MODEL_NAME,
  variant = "label_primary_v1",
  label_mode = "open",
  use_brainstorm = TRUE,
  semantic_layer = TRUE,
  life_form_layer = "complex",
  structured_prompt = TRUE,
  represented_species = 5L,
  timeout_sec = TIMEOUT_SEC,
  num_predict = NUM_PREDICT,
  prompt_budget_chars = PROMPT_BUDGET_CHARS,
  ollama_options = OLLAMA_OPTIONS,
  review_dir = file.path(smoke_root, "cluster_reviews"),
  log_dir = file.path(smoke_root, "llm_logs"),
  debug = TRUE,
  labels_for_imgs = TRUE,
  verbose = TRUE
)

saveRDS(
  smoke_run,
  file.path(smoke_root, paste0(MODEL_SLUG, "_smoke_run.rds"))
)

utils::write.csv(
  smoke_run$summary,
  file.path(smoke_root, paste0(MODEL_SLUG, "_smoke_summary.csv")),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

smoke_run$summary
```

Good signs in `smoke_run$summary`:

- `run_status` is not a hard failure
- `validation_status` is valid
- `review_file` points to an existing `.md` file
- `failure_reason` is empty or `NA`

An `output_status = "abstain"` can still be acceptable for a weaker
model. It is not automatically a pipeline failure.

Where to look after the smoke test:

- `runs/<MODEL_SLUG>/smoke/<MODEL_SLUG>_smoke_run.rds`
- `runs/<MODEL_SLUG>/smoke/<MODEL_SLUG>_smoke_summary.csv`
- `runs/<MODEL_SLUG>/smoke/cluster_reviews/...`
- `runs/<MODEL_SLUG>/smoke/llm_logs/...` because `debug = TRUE`

## 9. Step 8: Run Full Labeling

If you want a smaller first batch, replace `selected_clusters` with
something like `head(selected_clusters, 10)` here.

```r
res <- readRDS(
  file.path(report_root, "clustering", "res_real_eval_long_numeric_v1.rds")
)

selected_clusters <- utils::read.csv(
  file.path(report_root, "clustering", "selected_clusters.csv"),
  stringsAsFactors = FALSE
)[["cluster"]]

model_root <- file.path(report_root, "runs", MODEL_SLUG)
full_root <- file.path(model_root, "full")

dir.create(full_root, recursive = TRUE, showWarnings = FALSE)

run <- label_clusters(
  x = res,
  clusters = selected_clusters,
  model = MODEL_NAME,
  variant = "label_primary_v1",
  label_mode = "open",
  use_brainstorm = TRUE,
  semantic_layer = TRUE,
  life_form_layer = "complex",
  structured_prompt = TRUE,
  represented_species = 5L,
  timeout_sec = TIMEOUT_SEC,
  num_predict = NUM_PREDICT,
  prompt_budget_chars = PROMPT_BUDGET_CHARS,
  ollama_options = OLLAMA_OPTIONS,
  review_dir = file.path(full_root, "cluster_reviews"),
  log_dir = file.path(full_root, "llm_logs"),
  debug = TRUE,
  labels_for_imgs = TRUE,
  verbose = TRUE
)

saveRDS(
  run,
  file.path(full_root, paste0(MODEL_SLUG, "_run.rds"))
)

summary_tbl <- run$summary
summary_tbl$model_slug <- MODEL_SLUG
summary_tbl$model_name <- MODEL_NAME

utils::write.csv(
  summary_tbl,
  file.path(full_root, paste0(MODEL_SLUG, "_summary.csv")),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

if (inherits(run$label_registry, "cluster_label_registry")) {
  registry_tbl <- as.data.frame(run$label_registry, stringsAsFactors = FALSE)
  registry_tbl$model_slug <- MODEL_SLUG
  registry_tbl$model_name <- MODEL_NAME

  utils::write.csv(
    registry_tbl,
    file.path(full_root, paste0(MODEL_SLUG, "_label_registry_copy.csv")),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

summary_tbl
```

Where to look after the full run:

- `runs/<MODEL_SLUG>/full/<MODEL_SLUG>_run.rds`
- `runs/<MODEL_SLUG>/full/<MODEL_SLUG>_summary.csv`
- `runs/<MODEL_SLUG>/full/cluster_reviews/...`
- `runs/<MODEL_SLUG>/full/llm_logs/...`
- `runs/<MODEL_SLUG>/full/<MODEL_SLUG>_label_registry_copy.csv`

Also note:

- `run$label_registry_file` points to the automatically written
  `cluster_label_registry.csv`
- that registry file lives next to the review cards, not in the root of
  `full_root`
- inside `cluster_reviews/` and `llm_logs/`, expect a dataset-specific
  subfolder derived from `dataset_label`; in this guide that slug is
  `real_eval_long_numeric_v1`
- the recommended ordinary-labeling profile in this guide is
  `semantic_layer = TRUE`,
  `life_form_layer = "complex"`,
  `structured_prompt = TRUE`,
  `represented_species = 5L`
- if label quality looks too generic or too unstable on your dataset, the
  first parameter to vary experimentally is usually `represented_species`,
  followed by whether `life_form_layer` stays on

## 10. Optional: Category/Subcategory Labeling

If you want each cluster to receive both a broad vegetation category and a
shorter within-category subcategory, keep the same evidence-first workflow and
turn on subcategorization. The best prompt-only setting from the current
experiments is `internal_prompt_version = "v8a"`.

```r
subcat_root <- file.path(model_root, "full_subcategorization_v8a")
dir.create(subcat_root, recursive = TRUE, showWarnings = FALSE)

run_subcat <- label_clusters(
  x = res,
  clusters = selected_clusters,
  model = MODEL_NAME,
  variant = "label_primary_v1",
  label_mode = "open",
  use_brainstorm = TRUE,
  semantic_layer = TRUE,
  short_label_with_llm = FALSE,
  use_subcategorization = TRUE,
  internal_prompt_version = "v8a",
  timeout_sec = TIMEOUT_SEC,
  num_predict = NUM_PREDICT,
  prompt_budget_chars = PROMPT_BUDGET_CHARS,
  ollama_options = OLLAMA_OPTIONS,
  review_dir = file.path(subcat_root, "cluster_reviews"),
  log_dir = file.path(subcat_root, "llm_logs"),
  debug = TRUE,
  labels_for_imgs = TRUE,
  verbose = TRUE
)

run_subcat$summary[, intersect(
  c("cluster", "display_label", "category_label", "subcategory_labels"),
  names(run_subcat$summary)
)]
```

Interpret these extra fields this way:

- `category_label` is the broad vegetation group name
- `subcategory_labels` is the shorter distinction inside that broad group
- `display_label` remains the main human-readable cluster label

## 11. Optional Experiment: Two Brainstorm Drafts

There is also a larger experimental flow that asks the model for two separate
brainstorm drafts before the downstream label, category, subcategory, and
summary prompts. It is callable and useful for comparison runs, but it did not
beat `v8a` in the current pilot.

Use it only together with subcategorization and the isolated `v9` prompt
bundle:

```r
double_root <- file.path(model_root, "full_double_brainstorm_v9")
dir.create(double_root, recursive = TRUE, showWarnings = FALSE)

run_double <- label_clusters(
  x = res,
  clusters = selected_clusters,
  model = MODEL_NAME,
  variant = "label_primary_v1",
  label_mode = "open",
  use_brainstorm = TRUE,
  semantic_layer = TRUE,
  short_label_with_llm = FALSE,
  use_subcategorization = TRUE,
  use_double_brainstorm = TRUE,
  internal_prompt_version = "v9",
  timeout_sec = TIMEOUT_SEC,
  num_predict = NUM_PREDICT,
  prompt_budget_chars = PROMPT_BUDGET_CHARS,
  ollama_options = OLLAMA_OPTIONS,
  review_dir = file.path(double_root, "cluster_reviews"),
  log_dir = file.path(double_root, "llm_logs"),
  debug = TRUE,
  labels_for_imgs = TRUE,
  verbose = TRUE
)

run_double$summary[, intersect(
  c(
    "cluster",
    "display_label",
    "category_label",
    "subcategory_labels",
    "subcategorization_strategy",
    "double_brainstorm_enabled"
  ),
  names(run_double$summary)
)]
```

## 12. Step 9: Inspect The Results

Read the saved summary table:

```r
summary_path <- file.path(
  report_root,
  "runs",
  MODEL_SLUG,
  "full",
  paste0(MODEL_SLUG, "_summary.csv")
)

summary_tbl <- utils::read.csv(
  summary_path,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

summary_tbl[, intersect(
  c(
    "cluster",
    "run_status",
    "output_status",
    "validation_status",
    "display_label",
    "canonical_label",
    "failure_reason",
    "review_file"
  ),
  names(summary_tbl)
)]
```

Interpret the saved label fields as follows:

- `display_label` is the full stored human label
- `canonical_label` is the short/projected programmatic label
- plot-shortened previews live in `cluster_label_registry.csv` as
  `plot_label_short`, `legend_label`, and `hclust_label_compact`

Open the first review card:

```r
first_review <- summary_tbl$review_file[!is.na(summary_tbl$review_file)][1]
first_review
file.exists(first_review)

if (file.exists(first_review)) {
  file.edit(first_review)
}
```

List all review cards:

```r
review_files <- list.files(
  file.path(report_root, "runs", MODEL_SLUG, "full", "cluster_reviews"),
  pattern = "_review\\.md$",
  recursive = TRUE,
  full.names = TRUE
)

review_files
```

If `debug = TRUE`, list log files:

```r
log_files <- list.files(
  file.path(report_root, "runs", MODEL_SLUG, "full", "llm_logs"),
  recursive = TRUE,
  full.names = TRUE
)

log_files
```

## 13. Step 10: Render The Labeled Plots

If your R session has restarted, reload the saved objects first:

```r
res <- readRDS(
  file.path(report_root, "clustering", "res_real_eval_long_numeric_v1.rds")
)

selected_clusters <- utils::read.csv(
  file.path(report_root, "clustering", "selected_clusters.csv"),
  stringsAsFactors = FALSE
)[["cluster"]]

run <- readRDS(
  file.path(
    report_root,
    "runs",
    MODEL_SLUG,
    "full",
    paste0(MODEL_SLUG, "_run.rds")
  )
)
```

Now render the plots:

```r
dir.create(file.path(report_root, "plots"), recursive = TRUE, showWarnings = FALSE)

cluster_hclust_plot(
  x = res,
  clusters = selected_clusters,
  label_registry = run$label_registry,
  file = file.path(report_root, "plots", paste0(MODEL_SLUG, "_cluster_hclust_labels.png"))
)

cocktail_plot(
  x = res,
  clusters = selected_clusters,
  label_clusters = TRUE,
  label_registry = run$label_registry,
  file = file.path(report_root, "plots", paste0(MODEL_SLUG, "_full_cocktail_plot.png"))
)

list.files(file.path(report_root, "plots"), full.names = TRUE)
```

Important plotting note:

- `cluster_hclust_plot()` writes one PNG file
- `cocktail_plot()` may write more than one PNG page
- if you ask for
  `.../phi4-mini_latest_full_cocktail_plot.png`, you should expect files
  such as:
  - `..._full_cocktail_plot_page01.png`
  - `..._full_cocktail_plot_page02.png`

If you want to inspect the saved registry as a plain table:

```r
registry_copy_path <- file.path(
  report_root,
  "runs",
  MODEL_SLUG,
  "full",
  paste0(MODEL_SLUG, "_label_registry_copy.csv")
)

registry_tbl <- utils::read.csv(
  registry_copy_path,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

registry_tbl
```

## 14. What To Check First When Something Looks Wrong

### Package load step fails

Check that you are pointing at the correct project root and that
`pkgload` is installed:

```r
root
file.exists(file.path(root, "DESCRIPTION"))
```

If you are using a source checkout, prefer:

```r
pkgload::load_all(path = root, quiet = TRUE)
```

### Ollama does not respond

In PowerShell:

```powershell
ollama list
ollama ps
```

If needed:

```powershell
ollama stop phi4-mini:latest
```

Then rerun the smoke test.

### The model is too slow or times out

Try smaller settings:

```r
OLLAMA_OPTIONS <- list(num_ctx = 4096L)
PROMPT_BUDGET_CHARS <- 6000L
NUM_PREDICT <- 1200L
TIMEOUT_SEC <- 900L
```

### You get many abstentions

Inspect:

- `summary_tbl$output_status`
- `summary_tbl$failure_reason`
- review cards
- `llm_logs/`

Possible causes:

- the model is too small for the task
- the context window is too small
- the prompt budget is too small
- the cluster is genuinely mixed and hard to name

### You have review cards but no labeled plots

Check:

```r
inherits(run$label_registry, "cluster_label_registry")
run$label_registry_file
```

Then call plotting helpers with:

```r
label_registry = run$label_registry
```

Do not rely on `label_registry = "auto"` in this guide, because the
review artifacts live inside the run folder, not under the default
auto-discovery root.

## 15. Minimal Process Map

```text
PowerShell step
  -> create REPORT_ROOT

R setup step
  -> load package
  -> fix model and LLM settings

Data step
  -> raw SPE.csv
  -> anonymized numeric long table

Clustering step
  -> cocktail_cluster()
  -> Cocktail object res

Selection step
  -> selected_clusters.csv

Optional dry run
  -> inspect assembled request without contacting Ollama

Smoke test
  -> one-cluster review card
  -> one-cluster summary
  -> logs

Full run
  -> review cards
  -> summary CSV
  -> label registry
  -> logs

Plotting step
  -> labeled cluster hclust PNG
  -> labeled cocktail plot PNG pages
```

## 16. The Main Files Most Users Actually Need

If you only want the final practical outputs, start here:

- `runs/<MODEL_SLUG>/full/<MODEL_SLUG>_summary.csv`
- `runs/<MODEL_SLUG>/full/cluster_reviews/...`
- `runs/<MODEL_SLUG>/full/<MODEL_SLUG>_label_registry_copy.csv`
- `plots/<MODEL_SLUG>_cluster_hclust_labels.png`
- `plots/<MODEL_SLUG>_full_cocktail_plot_page01.png`
