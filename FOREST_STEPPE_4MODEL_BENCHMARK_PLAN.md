# План воспроизводимого эксперимента: новый LLM-лейблинг на реальном forest-steppe датасете

## Статус исходных данных

- [x] Получить датасет для анализа:
  `D:\documents\coctrailr\cocktailr\data-raw\external\forest_steppe_chytry_2021\Chytry-Krystof_forest-steppe-v1_2021-05-24_SPE.csv`
- [x] Получить датасеты для обогащения evidence:
  `D:\documents\coctrailr\cocktailr\data-raw\external\eive_1_0\EIVE_Paper_1.0_SM_08.xlsx`
  `D:\documents\coctrailr\cocktailr\data-raw\external\ellenberg_tichy_2023\Indicator.values-tables-2022-11-07-Zenodo.v2.xlsx`
- [ ] Позже дописать отдельное приложение с provenance этих файлов и полным описанием их предварительной подготовки.

## Цель

Провести один честный, воспроизводимый benchmark нового универсального
лейблинг-пайплайна на реальном long-format датасете и сравнить 4 локально
доступные Ollama-модели:

- `phi4-mini-4k:latest`
- `phi4-mini:latest`
- `qwen3.5:9b-q4_K_M`
- `gemma4:12b`

`embeddinggemma:latest` в этот benchmark не входит, потому что это embedding-модель,
а не генеративная LLM для structured labeling.

## Что именно тестируем

Тестируем не "голую модель", а новый рабочий каркас:

- `variant = "label_primary_v1"`
- fixed staged pipeline: brainstorm -> selection ladder -> explanation
- `use_brainstorm = TRUE`
- `label_mode = "open"`
- `semantic_layer = TRUE`
- `prompt_budget_chars = 10000`
- `num_predict = 2400`
- no speculative fallback branch: if the run starts on one model, all stages finish on that same model
- автоматический EOF retry ladder `2400 -> 4800 -> 9600`
- repair для длинных / плохих label-полей
- `labels_for_imgs = TRUE`

Идея benchmark-а: проверить, насколько этот универсальный пайплайн устойчив на
сильной, средней и слабой модели, включая урезанную
`phi4-mini-4k:latest`.

## Почему здесь особенно важен контроль утечки данных

В папке с "живым" датасетом есть не только vegetation table, но и таблицы,
которые выглядят как источник gold-разметки и контекстных подсказок:

- `ENV.csv` содержит поле `HABITAT`
- `SITES.csv` содержит `SITE_ID`, `LOCALITY`, `BEDROCK` и другие метаданные

На текущей проверке `ENV.csv$HABITAT` действительно выглядит как post-hoc gold для
сравнения. Там есть коды:

- `ST` = 51
- `UE` = 49
- `UF` = 44
- `LF` = 35
- `LE` = 34
- `NA` = 5
- `NF` = 3
- `-` = 2
- `RL` = 1

Эти поля нельзя давать модели ни напрямую, ни косвенно.

## Жёсткие anti-leakage правила

Во время clustering и labeling используем только vegetation input, построенный из
`SPE.csv`.

Нельзя пускать в prompt / evidence:

- `HABITAT`
- `SITE_ID`
- `LOCALITY`
- `BEDROCK`
- координаты
- исходное имя файла `forest-steppe`
- исходные `PLOT_ID`

Поэтому рабочий input будет обезличен:

- имя файла будет нейтральным
- `PLOT_ID` будут заменены на `plot_001`, `plot_002`, ...
- `ENV.csv` и `SITES.csv` будут использоваться только после завершения всех LLM run-ов
- в `cocktail_cluster()` не будем передавать исходный `dataset_path`
- `dataset_label` зададим нейтрально

Отдельная проверка после первого smoke run:

- просканировать prompt/request logs на слова
  `HABITAT`, `SITE_ID`, `LOCALITY`, `BEDROCK`, `forest-steppe`, `Chytry`,
  `Kršlenica`, `Medovarce`

Если хотя бы одно из этих слов попадает в request payload, benchmark нужно
остановить и починить leakage до full run.

## Зафиксированные решения по входным данным

### 1. Работаем с long format

Базовый файл:

- `..._SPE.csv`

Структура:

- `PLOT_ID`
- `TAXON`
- `LAYER`
- `COVER`

### 2. Не используем raw `COVER` как есть

В `SPE.csv` встречаются cover-коды:

- `r`
- `+`
- `1`
- `m`
- `a`
- `b`
- `3`
- `4`
- `5`

Если передать их в `cocktail_cluster(..., plot_values = "rel_cover")`
без преобразования, пакет свалится в binary fallback, потому что это
нечисловые значения.

Поэтому перед benchmark-ом фиксируем одну явную numeric conversion
для Braun-Blanquet-подобной шкалы:

| raw code | interpretation | numeric value |
| --- | --- | ---: |
| `r` | trace | `0.1` |
| `+` | present, very low cover | `0.5` |
| `1` | `<5%` | `2.5` |
| `m` | `2m` | `4.0` |
| `a` | `2a` | `8.75` |
| `b` | `2b` | `18.75` |
| `3` | `25-50%` midpoint | `37.5` |
| `4` | `50-75%` midpoint | `62.5` |
| `5` | `75-100%` midpoint | `87.5` |

### 3. Дубликаты plot-species агрегируем до запуска Cocktail

Так как long-format содержит слой `LAYER`, одна и та же species может
встречаться в одном plot несколько раз. Для benchmark-а заранее
агрегируем до:

- `plot`
- `species`
- `value`

Правило:

- суммировать все cover values внутри `plot + species`
- обрезать сверху до `100`

Это даёт прозрачный и воспроизводимый numeric input.

## Зафиксированная конфигурация моделей

| slug | model | `ollama_options` | комментарий |
| --- | --- | --- | --- |
| `phi4-mini-4k_latest` | `phi4-mini-4k:latest` | `list(num_ctx = 4096)` | урезанное контекстное окно |
| `phi4-mini_latest` | `phi4-mini:latest` | `list(num_ctx = 8192)` | обычная phi4-mini |
| `qwen3.5_9b-q4_K_M` | `qwen3.5:9b-q4_K_M` | `list(num_ctx = 8192)` | средний baseline |
| `gemma4_12b` | `gemma4:12b` | `list(num_ctx = 8192)` | основной baseline |

## Что обязательно сохраняем

В отдельную папку эксперимента под `temp/reports/`:

- обезличенный input
- lookup между исходными `PLOT_ID` и `plot_001...`
- объект `res`
- зафиксированный список выбранных clusters
- run object для каждой модели
- summary CSV для каждой модели
- review cards
- raw LLM logs
- `cluster_label_registry.csv`
- картинки с лейблами
- post-hoc evaluation tables
- session / provenance файлы

## Предлагаемая структура папок

```text
temp/reports/forest_steppe_universal_llm_4models/<timestamp>/
  input/
  clustering/
  runs/
    phi4-mini-4k_latest/
    phi4-mini_latest/
    qwen3.5_9b-q4_K_M/
    gemma4_12b/
  plots/
  evaluation/
  session/
```

## Пошаговый протокол

## 1. Зафиксировать окружение и доступные модели

PowerShell:

```powershell
Set-Location D:\documents\coctrailr\cocktailr

$stamp = Get-Date -Format "yyyy-MM-dd_HHmmss"
$REPORT_ROOT = "temp/reports/forest_steppe_universal_llm_4models/$stamp"
$env:REPORT_ROOT = $REPORT_ROOT
$REPORT_ROOT_FILE = "temp/reports/forest_steppe_universal_llm_4models/_ACTIVE_REPORT_ROOT.txt"

New-Item -ItemType Directory -Force -Path "temp/reports/forest_steppe_universal_llm_4models" | Out-Null
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

Override this with the new single-source-of-truth flow:

- step 1 writes the active run path to
  `temp/reports/forest_steppe_universal_llm_4models/_ACTIVE_REPORT_ROOT.txt`
- all R and PowerShell snippets below should resolve `REPORT_ROOT` from that
  file when the environment variable is missing
- do not silently fall back to `manual`; fail loudly instead
- the older `manual` fallback note above is now obsolete
- do not type `<stamp>` literally in R; either leave `REPORT_ROOT` unset and
  let the snippets read `_ACTIVE_REPORT_ROOT.txt`, or set a real value such as
  `temp/reports/forest_steppe_universal_llm_4models/2026-06-30_134305`

R:

```r
root <- normalizePath("D:/documents/coctrailr/cocktailr", winslash = "/", mustWork = TRUE)
report_root_file <- file.path(
  root, "temp", "reports", "forest_steppe_universal_llm_4models",
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
    "REPORT_ROOT is not set and _ACTIVE_REPORT_ROOT.txt was not found. Run step 1 first.",
    call. = FALSE
  )
}
Sys.setenv(REPORT_ROOT = report_root_rel)
report_root <- file.path(root, report_root_rel)
dir.create(file.path(report_root, "session"), recursive = TRUE, showWarnings = FALSE)
writeLines(capture.output(sessionInfo()), file.path(report_root, "session", "R_sessionInfo.txt"))
```

## 2. Подготовить обезличенный vegetation input

R:

```r
root <- normalizePath("D:/documents/coctrailr/cocktailr", winslash = "/", mustWork = TRUE)
report_root_file <- file.path(
  root, "temp", "reports", "forest_steppe_universal_llm_4models",
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
    "REPORT_ROOT is not set and _ACTIVE_REPORT_ROOT.txt was not found. Run step 1 first.",
    call. = FALSE
  )
}
Sys.setenv(REPORT_ROOT = report_root_rel)
report_root <- normalizePath(file.path(root, report_root_rel), winslash = "/", mustWork = FALSE)
dir.create(file.path(report_root, "input"), recursive = TRUE, showWarnings = FALSE)

spe_path <- file.path(
  root, "data-raw", "external", "forest_steppe_chytry_2021",
  "Chytry-Krystof_forest-steppe-v1_2021-05-24_SPE.csv"
)

spe_raw <- utils::read.csv(
  spe_path,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

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

stopifnot(all(unique(spe_raw$COVER) %in% names(cover_map)))

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

spe_sanitized$value <- unname(cover_map[spe_sanitized$COVER])

veg_long <- aggregate(
  value ~ plot + TAXON,
  data = spe_sanitized,
  FUN = sum
)

veg_long$value <- pmin(veg_long$value, 100)
names(veg_long)[names(veg_long) == "TAXON"] <- "species"
veg_long <- veg_long[, c("plot", "species", "value")]

utils::write.csv(
  veg_long,
  file.path(report_root, "input", "veg_long_numeric_eval_v1.csv"),
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
```

## 3. Построить Cocktail object и зафиксировать набор clusters

R:

```r
root <- normalizePath("D:/documents/coctrailr/cocktailr", winslash = "/", mustWork = TRUE)
report_root_file <- file.path(
  root, "temp", "reports", "forest_steppe_universal_llm_4models",
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
    "REPORT_ROOT is not set and _ACTIVE_REPORT_ROOT.txt was not found. Run step 1 first.",
    call. = FALSE
  )
}
Sys.setenv(REPORT_ROOT = report_root_rel)
report_root <- normalizePath(file.path(root, report_root_rel), winslash = "/", mustWork = FALSE)
dir.create(file.path(report_root, "clustering"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(report_root, "plots"), recursive = TRUE, showWarnings = FALSE)

pkgload::load_all(root, quiet = TRUE)

veg_long <- utils::read.csv(
  file.path(report_root, "input", "veg_long_numeric_eval_v1.csv"),
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

res <- cocktail_cluster(
  vegmatrix = veg_long,
  input_format = "long",
  long = list(
    plot = "plot",
    species = "species",
    value = "value"
  ),
  progress = FALSE,
  plot_values = "rel_cover",
  species_cluster_phi = TRUE,
  save_vegmatrix = TRUE,
  dataset_label = "real_eval_long_numeric_v1"
)

saveRDS(
  res,
  file.path(report_root, "clustering", "res_forest_steppe_real_eval.rds")
)

selected_clusters <- select_clusters(
  x = res,
  min_phi = 0.20,
  min_k = 1,
  min_score = 0.30,
  mode = "strict",
  return = "labels"
)

utils::write.csv(
  data.frame(cluster = selected_clusters, stringsAsFactors = FALSE),
  file.path(report_root, "clustering", "selected_clusters.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cluster_hclust_plot(
  x = res,
  clusters = selected_clusters,
  label_leaves = FALSE,
  file = file.path(report_root, "plots", "baseline_cluster_hclust_ids.png")
)
```

Смысл этого шага:

- один раз зафиксировать `res`
- один раз зафиксировать `selected_clusters`
- использовать один и тот же набор clusters для всех 4 моделей

## 4. Сделать smoke test до полного прогона

Сначала прогоняем один cluster:

- на `gemma4:12b`
- на `phi4-mini-4k:latest`

Цель smoke test:

- проверить, что semantic layer реально строится
- проверить, что fixed staged pipeline жив и все стадии идут на той же модели
- проверить, что prompt budget укладывается
- проверить, что leakage scan пустой
- проверить, что картинки и registry сохраняются как надо

R:

```r
root <- normalizePath("D:/documents/coctrailr/cocktailr", winslash = "/", mustWork = TRUE)
report_root_file <- file.path(
  root, "temp", "reports", "forest_steppe_universal_llm_4models",
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
    "REPORT_ROOT is not set and _ACTIVE_REPORT_ROOT.txt was not found. Run step 1 first.",
    call. = FALSE
  )
}
Sys.setenv(REPORT_ROOT = report_root_rel)
report_root <- normalizePath(file.path(root, report_root_rel), winslash = "/", mustWork = FALSE)
dir.create(file.path(report_root, "plots"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(report_root, "evaluation"), recursive = TRUE, showWarnings = FALSE)
pkgload::load_all(root, quiet = TRUE)

res <- readRDS(file.path(report_root, "clustering", "res_forest_steppe_real_eval.rds"))
selected_clusters <- utils::read.csv(
  file.path(report_root, "clustering", "selected_clusters.csv"),
  stringsAsFactors = FALSE
)$cluster

smoke_cluster <- selected_clusters[[1]]

smoke_specs <- list(
  list(model = "gemma4:12b", slug = "gemma4_12b", ollama_options = list(num_ctx = 8192)),
  list(model = "phi4-mini-4k:latest", slug = "phi4-mini-4k_latest", ollama_options = list(num_ctx = 4096))
)

for (spec in smoke_specs) {
  run_dir <- file.path(report_root, "runs", spec$slug, "smoke")
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

  run <- label_clusters(
    x = res,
    clusters = smoke_cluster,
    model = spec$model,
    variant = "label_primary_v1",
    use_brainstorm = TRUE,
    label_mode = "open",
    semantic_layer = TRUE,
    semantic_root = root,
    timeout_sec = 600,
    num_predict = 2400,
    prompt_budget_chars = 10000L,
    ollama_options = spec$ollama_options,
    labels_for_imgs = TRUE,
    review_dir = file.path(run_dir, "cluster_reviews"),
    log_dir = file.path(run_dir, "llm_logs"),
    verbose = TRUE,
    full = FALSE
  )

  saveRDS(run, file.path(run_dir, paste0(spec$slug, "_smoke_run.rds")))

  utils::write.csv(
    run$summary,
    file.path(run_dir, paste0(spec$slug, "_smoke_summary.csv")),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}
```

PowerShell leakage scan:

Use this single canonical command. It refreshes `REPORT_ROOT` from
`_ACTIVE_REPORT_ROOT.txt`, scans only the smoke `llm_logs` folders for the two
smoke-tested models, writes exactly to
`$REPORT_ROOT/session/smoke_leakage_scan.txt`, and prints that path in the
console.

Run this one block:

```powershell
$REPORT_ROOT = (Get-Content "temp/reports/forest_steppe_universal_llm_4models/_ACTIVE_REPORT_ROOT.txt" -Encoding utf8 | Select-Object -First 1).Trim()
$env:REPORT_ROOT = $REPORT_ROOT

$pattern = "HABITAT|SITE_ID|LOCALITY|BEDROCK|forest-steppe|Chytry|Kršlenica|Medovarce"
$scan_out = Join-Path $REPORT_ROOT "session/smoke_leakage_scan.txt"
$scan_dirs = @(
  (Join-Path $REPORT_ROOT "runs/gemma4_12b/smoke/llm_logs"),
  (Join-Path $REPORT_ROOT "runs/phi4-mini-4k_latest/smoke/llm_logs")
)

New-Item -ItemType Directory -Force -Path (Split-Path $scan_out -Parent) | Out-Null

$scan_hits = Get-ChildItem $scan_dirs -Recurse -File |
  Select-String -Pattern $pattern -Encoding UTF8 -CaseSensitive

$scan_lines = @(
  $scan_hits | ForEach-Object {
    "{0}:{1}:{2}" -f $_.Path, $_.LineNumber, $_.Line.TrimEnd()
  }
)

Set-Content -Path $scan_out -Value $scan_lines -Encoding UTF8
Write-Host "Saved smoke leakage scan to: $scan_out"

if ($scan_lines.Count -eq 0) {
  Write-Host "No smoke leakage hits found."
} else {
  $scan_lines
}
```

How to inspect smoke outputs:

- Open `$REPORT_ROOT/session/smoke_leakage_scan.txt`.
  Good: the file exists and is empty.
  Bad: the file contains hits. Investigate them before starting the full benchmark.
- Open `$REPORT_ROOT/runs/gemma4_12b/smoke/gemma4_12b_smoke_summary.csv`.
  Look at `semantic_layer_status`, `run_status`, `failure_reason`, and `review_file`.
  Good: `semantic_layer_status` is not `failed`, `run_status` is `success` or `speculative`, and `failure_reason` is `NA`.
  Bad: `semantic_layer_status == failed`, `run_status == placeholder`, or `failure_reason` is filled.
- Open `$REPORT_ROOT/runs/phi4-mini-4k_latest/smoke/phi4-mini-4k_latest_smoke_summary.csv`.
  Use the same checks.
- Open the review card path from the `review_file` column in each smoke summary.
  For the current dataset this will typically be `$REPORT_ROOT/runs/<model_slug>/smoke/cluster_reviews/real_eval_long_numeric_v1/c_475_review.md`.
  Good: the review card exists and looks like a normal label review.
  Bad: the file is missing, empty, or only records a placeholder/failure.
- Open `$REPORT_ROOT/runs/<model_slug>/smoke/cluster_reviews/real_eval_long_numeric_v1/cluster_label_registry.csv`.
  Good: the file exists and contains the smoke cluster row.
  Bad: the file is missing.

Критерий перехода к full benchmark:

- `semantic_layer_status` не равен `failed`
- leakage scan пустой
- есть валидные review cards
- есть `cluster_label_registry.csv`

## 5. Полный прогон на 4 моделях

R:

```r
root <- normalizePath("D:/documents/coctrailr/cocktailr", winslash = "/", mustWork = TRUE)
report_root_file <- file.path(
  root, "temp", "reports", "forest_steppe_universal_llm_4models",
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
    "REPORT_ROOT is not set and _ACTIVE_REPORT_ROOT.txt was not found. Run step 1 first.",
    call. = FALSE
  )
}
Sys.setenv(REPORT_ROOT = report_root_rel)
report_root <- normalizePath(file.path(root, report_root_rel), winslash = "/", mustWork = FALSE)
pkgload::load_all(root, quiet = TRUE)

res <- readRDS(file.path(report_root, "clustering", "res_forest_steppe_real_eval.rds"))
selected_clusters <- utils::read.csv(
  file.path(report_root, "clustering", "selected_clusters.csv"),
  stringsAsFactors = FALSE
)$cluster

model_grid <- list(
  list(model = "phi4-mini-4k:latest", slug = "phi4-mini-4k_latest", ollama_options = list(num_ctx = 2048)),
  list(model = "phi4-mini:latest", slug = "phi4-mini_latest", ollama_options = list(num_ctx = 4096)),
  list(model = "qwen3.5:9b-q4_K_M", slug = "qwen3.5_9b-q4_K_M", ollama_options = list(num_ctx = 4096)),
  list(model = "gemma4:12b", slug = "gemma4_12b", ollama_options = list(num_ctx = 4096))
)

all_rows <- list()

for (spec in model_grid) {
  model_root <- file.path(report_root, "runs", spec$slug)
  dir.create(model_root, recursive = TRUE, showWarnings = FALSE)

  run <- label_clusters(
    x = res,
    clusters = selected_clusters,
    model = spec$model,
    variant = "label_primary_v1",
    use_brainstorm = TRUE,
    label_mode = "open",
    semantic_layer = TRUE,
    semantic_root = root,
    timeout_sec = 600,
    num_predict = 2400,
    prompt_budget_chars = 10000L,
    ollama_options = spec$ollama_options,
    labels_for_imgs = TRUE,
    review_dir = file.path(model_root, "cluster_reviews"),
    log_dir = file.path(model_root, "llm_logs"),
    verbose = TRUE,
    full = FALSE
  )

  saveRDS(
    run,
    file.path(model_root, paste0(spec$slug, "_run.rds"))
  )

  summary_tbl <- run$summary
  summary_tbl$model_slug <- spec$slug
  summary_tbl$model_name <- spec$model

  utils::write.csv(
    summary_tbl,
    file.path(model_root, paste0(spec$slug, "_summary.csv")),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  if (inherits(run$label_registry, "cluster_label_registry")) {
    registry_tbl <- as.data.frame(run$label_registry, stringsAsFactors = FALSE)
    registry_tbl$model_slug <- spec$slug
    registry_tbl$model_name <- spec$model

    utils::write.csv(
      registry_tbl,
      file.path(model_root, paste0(spec$slug, "_label_registry_copy.csv")),
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
  }

  cluster_hclust_plot(
    x = res,
    clusters = selected_clusters,
    label_registry = run$label_registry,
    label_field = "plot_label_short",
    file = file.path(report_root, "plots", paste0(spec$slug, "_cluster_hclust_labels.png"))
  )

  cocktail_plot(
    x = res,
    clusters = selected_clusters,
    label_clusters = TRUE,
    label_registry = run$label_registry,
    file = file.path(report_root, "plots", paste0(spec$slug, "_full_cocktail_plot.png"))
  )

  all_rows[[length(all_rows) + 1L]] <- summary_tbl
}

summary_all <- do.call(rbind, all_rows)
utils::write.csv(
  summary_all,
  file.path(report_root, "evaluation", "summary_4models.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
```
