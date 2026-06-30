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
- `workflow_steps = 3`
- `label_mode = "dynamic"`
- `speculative_fallback_mode = "after_nonaccepted"`
- `semantic_layer = TRUE`
- `prompt_budget_chars = 10000`
- `num_predict = 2400`
- speculative fallback принудительно остаётся на той же модели, которая сейчас тестируется
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
```

R:

```r
root <- normalizePath("D:/documents/coctrailr/cocktailr", winslash = "/", mustWork = TRUE)
report_root_rel <- Sys.getenv(
  "REPORT_ROOT",
  unset = "temp/reports/forest_steppe_universal_llm_4models/manual"
)
report_root <- file.path(root, report_root_rel)
dir.create(file.path(report_root, "session"), recursive = TRUE, showWarnings = FALSE)
writeLines(capture.output(sessionInfo()), file.path(report_root, "session", "R_sessionInfo.txt"))
```

## 2. Подготовить обезличенный vegetation input

R:

```r
root <- normalizePath("D:/documents/coctrailr/cocktailr", winslash = "/", mustWork = TRUE)
report_root_rel <- Sys.getenv(
  "REPORT_ROOT",
  unset = "temp/reports/forest_steppe_universal_llm_4models/manual"
)
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
report_root_rel <- Sys.getenv(
  "REPORT_ROOT",
  unset = "temp/reports/forest_steppe_universal_llm_4models/manual"
)
report_root <- normalizePath(file.path(root, report_root_rel), winslash = "/", mustWork = FALSE)

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
- проверить, что `workflow_steps = 3` и `label_mode = "dynamic"` живы
- проверить, что prompt budget укладывается
- проверить, что leakage scan пустой
- проверить, что картинки и registry сохраняются как надо

R:

```r
root <- normalizePath("D:/documents/coctrailr/cocktailr", winslash = "/", mustWork = TRUE)
report_root_rel <- Sys.getenv(
  "REPORT_ROOT",
  unset = "temp/reports/forest_steppe_universal_llm_4models/manual"
)
report_root <- normalizePath(file.path(root, report_root_rel), winslash = "/", mustWork = FALSE)
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

  options(
    cocktailr.speculative_fallback_model = spec$model,
    cocktailr.speculative_fallback_num_predict = 2400L,
    cocktailr.speculative_fallback_ollama_options = spec$ollama_options,
    cocktailr.speculative_fallback_variants = c("label_soft_v1", "label_broad_v1")
  )

  run <- label_clusters(
    x = res,
    clusters = smoke_cluster,
    model = spec$model,
    variant = "label_primary_v1",
    workflow_steps = 3L,
    label_mode = "dynamic",
    speculative_fallback_mode = "after_nonaccepted",
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

```powershell
rg -n -S "HABITAT|SITE_ID|LOCALITY|BEDROCK|forest-steppe|Chytry|Kršlenica|Medovarce" `
  "$REPORT_ROOT/runs/gemma4_12b/smoke/llm_logs" `
  "$REPORT_ROOT/runs/phi4-mini-4k_latest/smoke/llm_logs" |
  Out-File "$REPORT_ROOT/session/smoke_leakage_scan.txt" -Encoding utf8
```

Критерий перехода к full benchmark:

- `semantic_layer_status` не равен `failed`
- leakage scan пустой
- есть валидные review cards
- есть `cluster_label_registry.csv`

## 5. Полный прогон на 4 моделях

R:

```r
root <- normalizePath("D:/documents/coctrailr/cocktailr", winslash = "/", mustWork = TRUE)
report_root_rel <- Sys.getenv(
  "REPORT_ROOT",
  unset = "temp/reports/forest_steppe_universal_llm_4models/manual"
)
report_root <- normalizePath(file.path(root, report_root_rel), winslash = "/", mustWork = FALSE)
pkgload::load_all(root, quiet = TRUE)

res <- readRDS(file.path(report_root, "clustering", "res_forest_steppe_real_eval.rds"))
selected_clusters <- utils::read.csv(
  file.path(report_root, "clustering", "selected_clusters.csv"),
  stringsAsFactors = FALSE
)$cluster

model_grid <- list(
  list(model = "phi4-mini-4k:latest", slug = "phi4-mini-4k_latest", ollama_options = list(num_ctx = 4096)),
  list(model = "phi4-mini:latest", slug = "phi4-mini_latest", ollama_options = list(num_ctx = 8192)),
  list(model = "qwen3.5:9b-q4_K_M", slug = "qwen3.5_9b-q4_K_M", ollama_options = list(num_ctx = 8192)),
  list(model = "gemma4:12b", slug = "gemma4_12b", ollama_options = list(num_ctx = 8192))
)

all_rows <- list()

for (spec in model_grid) {
  model_root <- file.path(report_root, "runs", spec$slug)
  dir.create(model_root, recursive = TRUE, showWarnings = FALSE)

  options(
    cocktailr.speculative_fallback_model = spec$model,
    cocktailr.speculative_fallback_num_predict = 2400L,
    cocktailr.speculative_fallback_ollama_options = spec$ollama_options,
    cocktailr.speculative_fallback_variants = c("label_soft_v1", "label_broad_v1")
  )

  run <- label_clusters(
    x = res,
    clusters = selected_clusters,
    model = spec$model,
    variant = "label_primary_v1",
    workflow_steps = 3L,
    label_mode = "dynamic",
    speculative_fallback_mode = "after_nonaccepted",
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

## 6. Отдельно сохранить leakage audit после full run

PowerShell:

```powershell
rg -n -S "HABITAT|SITE_ID|LOCALITY|BEDROCK|forest-steppe|Chytry|Kršlenica|Medovarce" `
  "$REPORT_ROOT/runs" |
  Out-File "$REPORT_ROOT/evaluation/full_leakage_scan.txt" -Encoding utf8
```

Если scan не пустой, в финальный отчёт нужно явно вынести:

- где именно встретилось подозрительное слово
- было ли это в request payload, response payload или только в review metadata

## 7. Только после завершения LLM run-ов подключить gold из `ENV.csv`

Смысл шага:

- на этапе LLM inference gold не трогаем
- на этапе evaluation можно безопасно читать `ENV.csv`

R:

```r
root <- normalizePath("D:/documents/coctrailr/cocktailr", winslash = "/", mustWork = TRUE)
report_root_rel <- Sys.getenv(
  "REPORT_ROOT",
  unset = "temp/reports/forest_steppe_universal_llm_4models/manual"
)
report_root <- normalizePath(file.path(root, report_root_rel), winslash = "/", mustWork = FALSE)

res <- readRDS(file.path(report_root, "clustering", "res_forest_steppe_real_eval.rds"))
selected_clusters <- utils::read.csv(
  file.path(report_root, "clustering", "selected_clusters.csv"),
  stringsAsFactors = FALSE
)$cluster
plot_lookup <- utils::read.csv(
  file.path(report_root, "input", "plot_id_lookup_private.csv"),
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

env_raw <- utils::read.csv(
  file.path(
    root, "data-raw", "external", "forest_steppe_chytry_2021",
    "Chytry-Krystof_forest-steppe-v1_2021-05-24_ENV.csv"
  ),
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

gold_plot <- merge(
  plot_lookup,
  env_raw[, c("PLOT_ID", "HABITAT")],
  by = "PLOT_ID",
  all.x = TRUE,
  sort = FALSE
)

names(gold_plot)[names(gold_plot) == "HABITAT"] <- "gold_habitat"

pc <- as.matrix(res$Plot.cluster)

membership_rows <- do.call(
  rbind,
  lapply(selected_clusters, function(cl) {
    w <- pc[, cl]
    idx <- which(w > 0)
    data.frame(
      plot = rownames(pc)[idx],
      cluster = cl,
      membership = as.numeric(w[idx]),
      stringsAsFactors = FALSE
    )
  })
)

members_gold <- merge(
  membership_rows,
  gold_plot[, c("plot", "gold_habitat")],
  by = "plot",
  all.x = TRUE,
  sort = FALSE
)

cluster_gold_profile <- aggregate(
  membership ~ cluster + gold_habitat,
  data = members_gold,
  FUN = sum
)

cluster_totals <- aggregate(
  membership ~ cluster,
  data = members_gold,
  FUN = sum
)
names(cluster_totals)[2] <- "cluster_total_membership"

cluster_gold_profile <- merge(
  cluster_gold_profile,
  cluster_totals,
  by = "cluster",
  all.x = TRUE
)

cluster_gold_profile$share <- with(
  cluster_gold_profile,
  membership / cluster_total_membership
)

cluster_gold_majority <- do.call(
  rbind,
  lapply(split(cluster_gold_profile, cluster_gold_profile$cluster), function(df) {
    df <- df[order(-df$share, -df$membership), , drop = FALSE]
    data.frame(
      cluster = df$cluster[[1]],
      majority_gold_habitat = df$gold_habitat[[1]],
      majority_share = df$share[[1]],
      stringsAsFactors = FALSE
    )
  })
)

utils::write.csv(
  cluster_gold_profile,
  file.path(report_root, "evaluation", "cluster_gold_profile.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

utils::write.csv(
  cluster_gold_majority,
  file.path(report_root, "evaluation", "cluster_gold_majority.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
```

Почему именно так:

- `HABITAT` задан на уровне plot
- label выдаётся на уровне cluster
- поэтому нужен cluster-level gold profile, а не просто один raw code из одной записи

## 8. Подготовить модель-сравнение с gold, но не фальсифицировать "accuracy"

Здесь важная оговорка:

- LLM выдаёт свободные текстовые label-ы
- `ENV.csv$HABITAT` хранит коды
- прямое string-match сравнение будет некорректным

Поэтому сравнение делим на два слоя.

### Слой A. Полностью автоматический

Считать для каждой модели:

- сколько clusters получили accepted label
- сколько ушли в abstain
- сколько ушли в placeholder
- сколько потребовали repair
- сколько потребовали EOF escalation
- средний `num_predict_used`
- сколько enrichment реально отработал

R:

```r
root <- normalizePath("D:/documents/coctrailr/cocktailr", winslash = "/", mustWork = TRUE)
report_root_rel <- Sys.getenv(
  "REPORT_ROOT",
  unset = "temp/reports/forest_steppe_universal_llm_4models/manual"
)
report_root <- normalizePath(file.path(root, report_root_rel), winslash = "/", mustWork = FALSE)

summary_all <- utils::read.csv(
  file.path(report_root, "evaluation", "summary_4models.csv"),
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

count_eval <- aggregate(
  cbind(
    n_clusters = rep(1L, nrow(summary_all)),
    n_success = as.integer(summary_all$run_status %in% c("success", "speculative")),
    n_accepted = as.integer(summary_all$output_status == "labeled" & summary_all$validation_status == "valid"),
    n_abstain = as.integer(summary_all$output_status == "abstain"),
    n_placeholder = as.integer(summary_all$run_status == "placeholder"),
    n_repair = as.integer(summary_all$repair_used),
    n_semantic_ok = as.integer(summary_all$semantic_layer_status == "enriched")
  ),
  by = list(model_slug = summary_all$model_slug),
  FUN = sum,
  na.rm = TRUE
)

mean_np <- aggregate(
  num_predict_used ~ model_slug,
  data = summary_all,
  FUN = function(x) mean(as.numeric(x), na.rm = TRUE)
)

names(mean_np)[names(mean_np) == "num_predict_used"] <- "mean_num_predict_used"

auto_eval <- merge(count_eval, mean_np, by = "model_slug", all = TRUE)

utils::write.csv(
  auto_eval,
  file.path(report_root, "evaluation", "model_auto_eval.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
```

### Слой B. Post-hoc ecological alignment

Нужно сделать отдельную таблицу для ручного или полу-ручного мэппинга:

- `model_slug`
- `cluster`
- `display_label`
- `canonical_label`
- `majority_gold_habitat`
- `majority_share`
- `mapped_gold_code`
- `mapping_confidence`
- `mapping_note`

R:

```r
root <- normalizePath("D:/documents/coctrailr/cocktailr", winslash = "/", mustWork = TRUE)
report_root_rel <- Sys.getenv(
  "REPORT_ROOT",
  unset = "temp/reports/forest_steppe_universal_llm_4models/manual"
)
report_root <- normalizePath(file.path(root, report_root_rel), winslash = "/", mustWork = FALSE)

summary_all <- utils::read.csv(
  file.path(report_root, "evaluation", "summary_4models.csv"),
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

cluster_gold_majority <- utils::read.csv(
  file.path(report_root, "evaluation", "cluster_gold_majority.csv"),
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

registry_files <- list.files(
  file.path(report_root, "runs"),
  pattern = "_label_registry_copy\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

registry_all <- do.call(
  rbind,
  lapply(registry_files, function(path) {
    utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  })
)

label_eval_queue <- merge(
  registry_all,
  cluster_gold_majority,
  by = "cluster",
  all.x = TRUE,
  sort = FALSE
)

for (col in c("mapped_gold_code", "mapping_confidence", "mapping_note")) {
  label_eval_queue[[col]] <- NA_character_
}

utils::write.csv(
  label_eval_queue,
  file.path(report_root, "evaluation", "label_eval_queue.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
```

Это честнее, чем объявлять "accuracy", пока мы не зафиксировали словарь между
свободными ecological label-ами и кодами `HABITAT`.

## 9. Что должно получиться на выходе

Минимальный комплект артефактов:

- `input/veg_long_numeric_eval_v1.csv`
- `input/plot_id_lookup_private.csv`
- `input/cover_conversion_table.csv`
- `clustering/res_forest_steppe_real_eval.rds`
- `clustering/selected_clusters.csv`
- `runs/<model_slug>/<model_slug>_run.rds`
- `runs/<model_slug>/<model_slug>_summary.csv`
- `runs/<model_slug>/cluster_reviews/...`
- `runs/<model_slug>/llm_logs/...`
- `plots/<model_slug>_cluster_hclust_labels.png`
- `plots/<model_slug>_full_cocktail_plot.png`
- `evaluation/summary_4models.csv`
- `evaluation/cluster_gold_profile.csv`
- `evaluation/cluster_gold_majority.csv`
- `evaluation/model_auto_eval.csv`
- `evaluation/label_eval_queue.csv`
- `evaluation/full_leakage_scan.txt`
- `session/ollama_list.txt`
- `session/git_commit.txt`
- `session/git_status.txt`
- `session/R_sessionInfo.txt`

## 10. Как я буду интерпретировать результат после прогона

Сначала сравню модели по устойчивости пайплайна:

- какая модель чаще даёт valid structured output
- какая модель чаще уходит в abstain
- какая чаще требует repair
- какая чаще упирается в EOF escalation
- как часто реально помогает semantic layer

Потом сравню их по ecological полезности:

- насколько labels вообще читаемы и коротки
- насколько они похожи на dominant `HABITAT` profile cluster-а
- где модель улавливает transition / ecotone / woodland / grassland signal
- где модель начинает hallucinate formal habitat claims без достаточного evidence

Главный практический вопрос после benchmark-а:

- даёт ли новый универсальный пайплайн на слабых моделях "хоть что-то годное"
  без ручного тюнинга под каждую отдельную LLM

Если да, следующий шаг уже не в планировании, а в самом прогоне и разборе
конкретных результатов.
