# Labeling Step by Step RU: один реальный датасет, одна локальная LLM

Версия документа: рабочий русский черновик для отладки.

Этот документ описывает воспроизводимый путь от vegetation table до подписанных
кластеров в `cocktailr` для одной фиксированной модели:

```text
phi4-mini:latest
```

Цель документа не сравнить модели, а дать специалисту пошаговую инструкцию:

1. подготовить или загрузить vegetation input
2. выполнить Cocktail-кластеризацию
3. выбрать кластеры для лейблинга
4. вызвать локальную LLM через Ollama
5. получить review cards, summary tables, prompt logs и графики с labels

Промпты здесь не клонируем и не редактируем. Этот документ только показывает,
где посмотреть, какие prompt/request payloads реально ушли в модель.

---

## 0. Что делает pipeline на верхнем уровне

Весь путь состоит из двух больших частей.

### Часть A. Обычная кластеризация

```text
vegetation table
  -> cocktail_cluster()
  -> Cocktail object `res`
  -> selected_clusters
```

На этом этапе LLM не вызывается.

### Часть B. LLM-лейблинг уже найденных кластеров

```text
res + selected_clusters
  -> label_clusters()
  -> review cards
  -> summary CSV
  -> cluster_label_registry.csv
  -> plots with labels
```

Именно `label_clusters()` является главным пользовательским входом в LLM-слой.
Внутри он строит evidence для каждого кластера, запускает staged LLM pipeline,
валидирует ответ, сохраняет review card и, если включено `labels_for_imgs = TRUE`,
готовит registry для подписи графиков.

Текущий staged pipeline:

```text
cluster evidence
  -> optional brainstorm
  -> label decision ladder
  -> explanation
  -> final object assembled in code
```

---

## 1. Что должно быть установлено

### 1.1. R и пакет

Документ предполагает, что локальный checkout пакета лежит здесь:

```text
D:/documents/coctrailr/cocktailr
```

Если путь другой, замените `root` во всех R-блоках.

### 1.2. Ollama и модель
Чтобы загрузить модель к себе на компьютер: 
В PowerShell:

```powershell
ollama --version
ollama list
ollama pull phi4-mini:latest
```

Если у вас модель называется иначе, например `phi4-mini` без `:latest`,
посмотрите точное имя через:

```powershell
ollama list
```

и замените в R:

```r
MODEL_NAME <- "phi4-mini:latest"
```

на своё имя.

---

## 2. Какой реальный датасет используется в примере

В примере используется файл species table из forest-steppe dataset:

```text
D:/documents/coctrailr/cocktailr/data-raw/external/forest_steppe_chytry_2021/Chytry-Krystof_forest-steppe-v1_2021-05-24_SPE.csv
```

В нём есть колонки:

```text
PLOT_ID
TAXON
LAYER
COVER
```

Для LLM-лейблинга в этом документе используем только `SPE.csv`.

Не используем на этапе LLM inference:

```text
ENV.csv
SITES.csv
HABITAT
SITE_ID
LOCALITY
BEDROCK
координаты
исходные PLOT_ID
исходное имя файла forest-steppe
```

Причина: эти поля могут быть gold-разметкой или сильной контекстной подсказкой.
Если дать их модели, получится утечка данных. Модель должна смотреть только на
состав видов и derived evidence.

---

## 3. Как устроена предобработка примера

Исходный `SPE.csv` не передаётся в Cocktail напрямую.

Перед кластеризацией делаем три вещи.

### 3.1. Обезличиваем plot IDs

Исходные `PLOT_ID` заменяем на:

```text
plot_001
plot_002
plot_003
...
```

Lookup между старым и новым ID сохраняем отдельно:

```text
input/plot_id_lookup_private.csv
```

Этот lookup нужен только человеку. В prompt/request logs он попадать не должен.

### 3.2. Переводим Braun-Blanquet-like cover codes в числа

В исходном `COVER` встречаются коды:

| raw code | meaning | numeric value |
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

Почему это нужно: для `plot_values = "rel_cover"` значения должны быть
числовыми. Если оставить raw strings, pipeline может потерять информацию о cover
и перейти к более грубой binary-логике.

### 3.3. Агрегируем дубликаты plot-species

Поскольку в таблице есть `LAYER`, один и тот же вид может встречаться в одном
plot несколько раз. Для Cocktail input заранее приводим данные к виду:

```text
plot
species
value
```

Правило:

```text
value = sum cover values внутри plot + species
value сверху обрезается до 100
```

Итоговый prepared input сохраняем в:

```text
input/veg_long_numeric_eval_v1.csv
```

---

## 4. Структура папок результата

Каждый запуск создаёт отдельную timestamp-папку:

```text
temp/reports/forest_steppe_single_model_labeling/<timestamp>/
  input/
  clustering/
  runs/
    phi4-mini_latest/
      smoke/
      full/
  plots/
  evaluation/
  session/
```

Главные файлы на выходе:

```text
input/veg_long_numeric_eval_v1.csv
input/plot_id_lookup_private.csv
input/cover_conversion_table.csv

clustering/res_forest_steppe_real_eval.rds
clustering/selected_clusters.csv

runs/phi4-mini_latest/smoke/...
runs/phi4-mini_latest/full/phi4-mini_latest_run.rds
runs/phi4-mini_latest/full/phi4-mini_latest_summary.csv
runs/phi4-mini_latest/full/cluster_reviews/...
runs/phi4-mini_latest/full/llm_logs/...
runs/phi4-mini_latest/full/phi4-mini_latest_label_registry_copy.csv

plots/baseline_cluster_hclust_ids.png
plots/phi4-mini_latest_cluster_hclust_labels.png
plots/phi4-mini_latest_full_cocktail_plot.png

evaluation/full_leakage_scan.txt
session/ollama_list.txt
session/git_commit.txt
session/git_status.txt
session/R_sessionInfo.txt
```

Чтобы изменить место сохранения, измените `$BASE_REPORT_DIR` в шаге 5.
В этом документе используется папка внутри проекта, чтобы относительные пути
работали одинаково в PowerShell и R.

---

## 5. Шаг 1 в PowerShell: создать папку запуска и зафиксировать окружение

Откройте PowerShell.

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

Если Git-команды выдают ошибку, это не мешает labeling, но лучше сохранить
сообщение об ошибке вручную в `session/`, чтобы потом было понятно, из какой
версии кода выполнялся запуск.

---

## 6. Шаг 2 в R: загрузить пакет и зафиксировать настройки

Откройте RStudio из проекта `cocktailr.Rproj` или запустите R в корне проекта.

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
    "REPORT_ROOT is not set and _ACTIVE_REPORT_ROOT.txt was not found. Run PowerShell step 1 first.",
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

pkgload::load_all(root, quiet = TRUE)

writeLines(
  capture.output(sessionInfo()),
  file.path(report_root, "session", "R_sessionInfo.txt")
)

MODEL_NAME <- "phi4-mini:latest"
MODEL_SLUG <- "phi4-mini_latest"

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

### Что можно менять здесь

```r
MODEL_NAME
```

Имя локальной Ollama-модели.

```r
OLLAMA_OPTIONS <- list(num_ctx = 8192L)
```

Размер контекстного окна Ollama. Для более лёгкого запуска можно попробовать:

```r
OLLAMA_OPTIONS <- list(num_ctx = 4096L)
PROMPT_BUDGET_CHARS <- 6000L
NUM_PREDICT <- 1200L
```

```r
PROMPT_BUDGET_CHARS
```

Примерный character budget для prompt evidence. Чем меньше значение, тем короче
evidence попадёт в модель.

```r
NUM_PREDICT
```

Сколько токенов разрешено сгенерировать модели. Если ответы обрываются, можно
увеличить. Если запуск слишком долгий, можно уменьшить.

```r
TIMEOUT_SEC
```

Максимальное время ожидания одного LLM-вызова.

---

## 7. Шаг 3: подготовить forest-steppe input

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

spe_sanitized$value <- unname(cover_map[spe_sanitized$COVER])

veg_long <- aggregate(
  value ~ plot + TAXON,
  data = spe_sanitized,
  FUN = sum
)

veg_long$value <- pmin(veg_long$value, 100)

names(veg_long)[names(veg_long) == "TAXON"] <- "species"

veg_long <- veg_long[, c("plot", "species", "value")]

veg_long <- veg_long[order(veg_long$plot, veg_long$species), ]

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

cat("Prepared rows:", nrow(veg_long), "\n")
cat("Plots:", length(unique(veg_long$plot)), "\n")
cat("Species:", length(unique(veg_long$species)), "\n")
```

Ожидаемый результат для этого датасета: 224 plots и около 572 species.

---

## 8. Альтернативный шаг 3: подать уже предобработанный новый датасет

Этот блок нужен, если у вас уже есть готовый long-format CSV.

Требования к файлу:

```text
plot,species,value
plot_001,Acer campestre,0.5
plot_001,Quercus robur,37.5
...
```

Обязательные правила:

- `plot` - идентификатор описания / relevé / plot
- `species` - имя вида
- `value` - числовое значение cover / abundance / presence
- `value > 0`
- одна строка = один вид в одном plot
- желательно заранее агрегировать дубликаты `plot + species`

Если данные уже предобработаны, `cocktail_cluster()` не требует, чтобы файл
лежал в специальной папке. Но для воспроизводимости лучше скопировать
нормализованный input в папку текущего run-а:

```r
prepared_input_path <- "D:/path/to/my_preprocessed_veg_long.csv"

veg_long <- utils::read.csv(
  prepared_input_path,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

required_cols <- c("plot", "species", "value")
missing_cols <- setdiff(required_cols, names(veg_long))

if (length(missing_cols)) {
  stop(
    "Prepared input is missing required columns: ",
    paste(missing_cols, collapse = ", "),
    call. = FALSE
  )
}

veg_long <- veg_long[, required_cols]
veg_long$value <- as.numeric(veg_long$value)

veg_long <- veg_long[
  !is.na(veg_long$plot) &
    !is.na(veg_long$species) &
    !is.na(veg_long$value) &
    veg_long$value > 0,
]

veg_long <- aggregate(
  value ~ plot + species,
  data = veg_long,
  FUN = sum
)

veg_long$value <- pmin(veg_long$value, 100)

veg_long <- veg_long[order(veg_long$plot, veg_long$species), ]

utils::write.csv(
  veg_long,
  file.path(report_root, "input", "veg_long_numeric_eval_v1.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("Prepared rows:", nrow(veg_long), "\n")
cat("Plots:", length(unique(veg_long$plot)), "\n")
cat("Species:", length(unique(veg_long$species)), "\n")
```

Для нового датасета поменяйте также `dataset_label` в шаге кластеризации.
Например:

```r
DATASET_LABEL <- "my_eval_long_numeric_v1"
```

Не используйте в `dataset_label` слова, которые не должны попасть в prompt logs.

---

## 9. Шаг 4: построить Cocktail object и выбрать кластеры

```r
dir.create(file.path(report_root, "clustering"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(report_root, "plots"), recursive = TRUE, showWarnings = FALSE)

DATASET_LABEL <- "real_eval_long_numeric_v1"

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
  dataset_label = DATASET_LABEL
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

length(selected_clusters)
head(selected_clusters)
```

Что происходит здесь:

- `cocktail_cluster()` строит Cocktail object
- `select_clusters()` выбирает кластеры для labeling по score-фильтрам
- `selected_clusters.csv` фиксирует один набор кластеров, чтобы потом не выбирать
  их заново случайно
- `baseline_cluster_hclust_ids.png` показывает cluster hclust до LLM-labels

### Что можно менять

```r
min_phi = 0.20
```

Порог силы association.

```r
min_k = 1
```

Минимальное число видов в кластере.

```r
min_score = 0.30
```

Порог общего score.

Если кластеров слишком много, увеличьте `min_score` или `min_k`.
Если кластеров слишком мало, уменьшите `min_score`.

---

## 10. Шаг 5: проверить evidence и semantic layer без LLM

Это диагностический шаг. Он не вызывает LLM.

```r
selected_clusters <- utils::read.csv(
  file.path(report_root, "clustering", "selected_clusters.csv"),
  stringsAsFactors = FALSE
)$cluster

res <- readRDS(
  file.path(report_root, "clustering", "res_forest_steppe_real_eval.rds")
)

cluster_id <- selected_clusters[[1]]

ev <- cluster_evidence(
  res,
  cluster = cluster_id,
  top_n_phi = 10,
  n_prototype_plots = 5,
  n_borderline_plots = 5
)

names(ev)
ev$meta$cluster_id
ev$limitations
```

Проверим файлы semantic layer:

```r
semantic_files <- c(
  eive = file.path(
    root,
    "data-raw",
    "external",
    "eive_1_0",
    "EIVE_Paper_1.0_SM_08.xlsx"
  ),
  tichy = file.path(
    root,
    "data-raw",
    "external",
    "ellenberg_tichy_2023",
    "Indicator.values-tables-2022-11-07-Zenodo.v2.xlsx"
  )
)

semantic_files
file.exists(semantic_files)
```

Если функция `score_cluster_semantics()` есть в текущей версии пакета, можно
сделать отдельный тест:

```r
if (exists("score_cluster_semantics")) {
  semantic_test <- score_cluster_semantics(
    x = res,
    clusters = cluster_id,
    root = root,
    bootstrap = 100,
    seed = 42
  )

  semantic_test$wide_profile
  semantic_test$unmatched_species
}
```

Если semantic files отсутствуют, можно временно запускать LLM с:

```r
semantic_layer = FALSE
```

Но для текущего рабочего сценария используем:

```r
semantic_layer = TRUE
```

---

## 11. Шаг 6: dry run для проверки prompt/request без вызова модели

Этот шаг помогает посмотреть, какие payloads будут отправлены в Ollama.

```r
dry <- llm_label_cluster(
  evidence = ev,
  model = MODEL_NAME,
  variant = "label_primary_v1",
  use_brainstorm = TRUE,
  label_mode = "open",
  timeout_sec = TIMEOUT_SEC,
  num_predict = NUM_PREDICT,
  prompt_budget_chars = PROMPT_BUDGET_CHARS,
  ollama_options = OLLAMA_OPTIONS,
  dry_run = TRUE
)

saveRDS(
  dry,
  file.path(report_root, "session", "dry_run_first_cluster.rds")
)

names(dry)
```

Проверить, что `num_ctx` попал в request:

```r
collect_num_ctx <- function(x, path = "root") {
  out <- data.frame(
    path = character(),
    num_ctx = integer(),
    stringsAsFactors = FALSE
  )

  if (is.list(x) && !is.null(x$request$options$num_ctx)) {
    out <- rbind(
      out,
      data.frame(
        path = path,
        num_ctx = as.integer(x$request$options$num_ctx),
        stringsAsFactors = FALSE
      )
    )
  }

  if (is.list(x)) {
    nms <- names(x)
    if (is.null(nms)) {
      nms <- as.character(seq_along(x))
    }

    for (i in seq_along(x)) {
      out <- rbind(
        out,
        collect_num_ctx(
          x[[i]],
          paste0(path, "$", nms[[i]])
        )
      )
    }
  }

  out
}

collect_num_ctx(dry)
```

Где смотреть prompt/request:

```r
dry$workflow
```

Точная структура может отличаться от версии к версии. Главная идея: dry run
позволяет смотреть assembled request до реального запроса к Ollama.

---

## 12. Шаг 7: smoke test на одном кластере

Smoke test нужен, чтобы не запускать сразу десятки кластеров, если проблема в
модели, semantic layer, prompt budget или записи файлов.

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
  use_brainstorm = TRUE,
  label_mode = "open",
  semantic_layer = TRUE,
  semantic_root = root,
  timeout_sec = TIMEOUT_SEC,
  num_predict = NUM_PREDICT,
  prompt_budget_chars = PROMPT_BUDGET_CHARS,
  ollama_options = OLLAMA_OPTIONS,
  labels_for_imgs = TRUE,
  review_dir = file.path(smoke_root, "cluster_reviews"),
  log_dir = file.path(smoke_root, "llm_logs"),
  verbose = TRUE,
  full = FALSE
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

### Что должно быть хорошим признаком

В `smoke_run$summary` смотрим:

```text
run_status
output_status
validation_status
semantic_layer_status
failure_reason
review_file
```

Хорошие признаки:

```text
run_status: success
validation_status: valid
semantic_layer_status: enriched / ok / not failed
failure_reason: NA
review_file: путь к существующему .md
```

Допустимый результат для слабой модели:

```text
output_status: abstain
```

Это не всегда ошибка. Но если все кластеры уходят в `abstain`, качество модели
или prompt budget может быть недостаточным.

Если командная строка выводит что-то типа 

failure_reason
1           <NA>

То это значит, что причины фейла нет, она отсутствует. Это хороший признак. 

---

## 13. Шаг 8 в PowerShell: leakage scan после smoke test

В PowerShell из корня проекта:

```powershell
Set-Location D:\documents\coctrailr\cocktailr

$REPORT_ROOT = (Get-Content "temp/reports/forest_steppe_single_model_labeling/_ACTIVE_REPORT_ROOT.txt" -Encoding utf8 | Select-Object -First 1).Trim()
$env:REPORT_ROOT = $REPORT_ROOT

$pattern = "HABITAT|SITE_ID|LOCALITY|BEDROCK|forest-steppe|Chytry|Kršlenica|Medovarce"
$scan_out = Join-Path $REPORT_ROOT "session/smoke_leakage_scan.txt"
$scan_root = Join-Path $REPORT_ROOT "runs/phi4-mini_latest/smoke/llm_logs"

New-Item -ItemType Directory -Force -Path (Split-Path $scan_out -Parent) | Out-Null

if (Test-Path $scan_root) {
  $scan_hits = Get-ChildItem $scan_root -Recurse -File |
    Select-String -Pattern $pattern -Encoding UTF8 -CaseSensitive

  $scan_lines = @(
    $scan_hits | ForEach-Object {
      "{0}:{1}:{2}" -f $_.Path, $_.LineNumber, $_.Line.TrimEnd()
    }
  )
} else {
  $scan_lines = @("Log folder not found: $scan_root")
}

Set-Content -Path $scan_out -Value $scan_lines -Encoding UTF8
Write-Host "Saved smoke leakage scan to: $scan_out"

if ($scan_lines.Count -eq 0) {
  Write-Host "No smoke leakage hits found."
} else {
  $scan_lines
}
```

Хорошо: файл `session/smoke_leakage_scan.txt` существует и пустой.

Плохо: там есть `HABITAT`, `SITE_ID`, `LOCALITY`, `BEDROCK`, исходные названия
мест или `forest-steppe`. Тогда full run лучше остановить и проверить, откуда
слово попало в prompt/request logs.

---

## 14. Шаг 9: полный labeling выбранных кластеров одной моделью

```r
res <- readRDS(
  file.path(report_root, "clustering", "res_forest_steppe_real_eval.rds")
)

selected_clusters <- utils::read.csv(
  file.path(report_root, "clustering", "selected_clusters.csv"),
  stringsAsFactors = FALSE
)$cluster

model_root <- file.path(report_root, "runs", MODEL_SLUG)
full_root <- file.path(model_root, "full")

dir.create(full_root, recursive = TRUE, showWarnings = FALSE)

run <- label_clusters(
  x = res,
  clusters = selected_clusters,
  model = MODEL_NAME,
  variant = "label_primary_v1",
  use_brainstorm = TRUE,
  label_mode = "open",
  semantic_layer = TRUE,
  semantic_root = root,
  timeout_sec = TIMEOUT_SEC,
  num_predict = NUM_PREDICT,
  prompt_budget_chars = PROMPT_BUDGET_CHARS,
  ollama_options = OLLAMA_OPTIONS,
  labels_for_imgs = TRUE,
  review_dir = file.path(full_root, "cluster_reviews"),
  log_dir = file.path(full_root, "llm_logs"),
  verbose = TRUE,
  full = FALSE
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

---

## 15. Шаг 10: посмотреть результаты labeling

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

names(summary_tbl)
summary_tbl
```

Минимально полезный просмотр:

```r
cols <- intersect(
  c(
    "cluster",
    "run_status",
    "output_status",
    "validation_status",
    "label_tier",
    "display_label",
    "canonical_label",
    "semantic_layer_status",
    "repair_used",
    "iterations_used",
    "num_predict_used",
    "failure_reason",
    "review_file"
  ),
  names(summary_tbl)
)

summary_tbl[, cols]
```

Открыть первую review card:

```r
first_review <- summary_tbl$review_file[!is.na(summary_tbl$review_file)][1]
first_review
file.exists(first_review)

if (file.exists(first_review)) {
  file.edit(first_review)
}
```

Найти все review cards:

```r
review_files <- list.files(
  file.path(report_root, "runs", MODEL_SLUG, "full", "cluster_reviews"),
  pattern = "_review\\.md$",
  recursive = TRUE,
  full.names = TRUE
)

review_files
```

---

## 16. Шаг 11: где смотреть prompt logs и какие промпты были использованы

проще пойти в каталог и посмотреть глазами. Например, путь к данным для кластера c_8:

D:\documents\coctrailr\cocktailr\temp\reports\forest_steppe_single_model_labeling\2026-07-03_145147\runs\phi4-mini_latest\full\cluster_reviews\real_eval_long_numeric_v1\c_8_review_model_logs

только корень проекта будет другим (если вы не меняли пути ранее). 

Главная папка logs:

```r
log_dir <- file.path(report_root, "runs", MODEL_SLUG, "full", "llm_logs")

log_files <- list.files(
  log_dir,
  recursive = TRUE,
  full.names = TRUE
)

log_files
```

В этих файлах надо искать request payloads и stage names. Структура logs зависит
от текущей версии пакета, но обычно именно здесь самый надёжный след того, что
ушло в модель.

Быстрый поиск по логам в R:

```r
hits <- lapply(log_files, function(path) {
  txt <- readLines(path, warn = FALSE, encoding = "UTF-8")
  found <- grep(
    "label_primary|label_soft|label_broad|Task mode|Cluster evidence|Brainstorm",
    txt,
    value = TRUE
  )

  if (length(found)) {
    data.frame(
      file = path,
      line = found,
      stringsAsFactors = FALSE
    )
  } else {
    NULL
  }
})

hits <- do.call(rbind, hits)
hits
```

Быстрый поиск по prompt templates в исходниках проекта через PowerShell:

```powershell
Set-Location D:\documents\coctrailr\cocktailr

Get-ChildItem . -Recurse -File -Include *.md,*.R,*.json |
  Select-String -Pattern "label_primary_v1|label_soft_v1|label_broad_v1|Task mode|label_decision" |
  Select-Object Path, LineNumber, Line
```

Важно: этот документ не меняет промпты. Для воспроизводимости сначала смотрим
logs, а операции с prompt cloning/editing выносим в отдельную инструкцию.

---

## 17. Шаг 12: построить графики с labels

После `label_clusters(..., labels_for_imgs = TRUE)` в объекте `run` есть
`run$label_registry`. Его можно подать в plotting helpers.

Если объект `run` ещё в памяти:

```r
dir.create(file.path(report_root, "plots"), recursive = TRUE, showWarnings = FALSE)

cluster_hclust_plot(
  x = res,
  clusters = selected_clusters,
  label_registry = run$label_registry,
  label_field = "plot_label_short",
  file = file.path(report_root, "plots", paste0(MODEL_SLUG, "_cluster_hclust_labels.png"))
)

cocktail_plot(
  x = res,
  clusters = selected_clusters,
  label_clusters = TRUE,
  label_registry = run$label_registry,
  file = file.path(report_root, "plots", paste0(MODEL_SLUG, "_full_cocktail_plot.png"))
)
```

Если R-сессия уже перезапускалась:

```r
res <- readRDS(
  file.path(report_root, "clustering", "res_forest_steppe_real_eval.rds")
)

selected_clusters <- utils::read.csv(
  file.path(report_root, "clustering", "selected_clusters.csv"),
  stringsAsFactors = FALSE
)$cluster

run <- readRDS(
  file.path(
    report_root,
    "runs",
    MODEL_SLUG,
    "full",
    paste0(MODEL_SLUG, "_run.rds")
  )
)

cluster_hclust_plot(
  x = res,
  clusters = selected_clusters,
  label_registry = run$label_registry,
  label_field = "plot_label_short",
  file = file.path(report_root, "plots", paste0(MODEL_SLUG, "_cluster_hclust_labels.png"))
)

cocktail_plot(
  x = res,
  clusters = selected_clusters,
  label_clusters = TRUE,
  label_registry = run$label_registry,
  file = file.path(report_root, "plots", paste0(MODEL_SLUG, "_full_cocktail_plot.png"))
)
```

Проверка:

```r
list.files(file.path(report_root, "plots"), full.names = TRUE)
```

Если нужно посмотреть registry как таблицу:

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

---

## 18. Шаг 13 в PowerShell: leakage scan после full run

```powershell
Set-Location D:\documents\coctrailr\cocktailr

$REPORT_ROOT = (Get-Content "temp/reports/forest_steppe_single_model_labeling/_ACTIVE_REPORT_ROOT.txt" -Encoding utf8 | Select-Object -First 1).Trim()
$env:REPORT_ROOT = $REPORT_ROOT

$pattern = "HABITAT|SITE_ID|LOCALITY|BEDROCK|forest-steppe|Chytry|Kršlenica|Medovarce"
$scan_out = Join-Path $REPORT_ROOT "evaluation/full_leakage_scan.txt"
$scan_root = Join-Path $REPORT_ROOT "runs/phi4-mini_latest/full/llm_logs"

New-Item -ItemType Directory -Force -Path (Split-Path $scan_out -Parent) | Out-Null

if (Test-Path $scan_root) {
  $scan_hits = Get-ChildItem $scan_root -Recurse -File |
    Select-String -Pattern $pattern -Encoding UTF8 -CaseSensitive

  $scan_lines = @(
    $scan_hits | ForEach-Object {
      "{0}:{1}:{2}" -f $_.Path, $_.LineNumber, $_.Line.TrimEnd()
    }
  )
} else {
  $scan_lines = @("Log folder not found: $scan_root")
}

Set-Content -Path $scan_out -Value $scan_lines -Encoding UTF8
Write-Host "Saved full leakage scan to: $scan_out"

if ($scan_lines.Count -eq 0) {
  Write-Host "No full-run leakage hits found."
} else {
  $scan_lines
}
```

Если scan не пустой, надо посмотреть, это настоящий request payload или только
безопасная metadata. Если подозрительное слово попало в request payload, результат
нельзя считать чистым.

---

## 19. Как интерпретировать результаты

### 19.1. Техническая устойчивость

Смотрим:

```r
table(summary_tbl$run_status, useNA = "ifany")
table(summary_tbl$output_status, useNA = "ifany")
table(summary_tbl$validation_status, useNA = "ifany")
table(summary_tbl$semantic_layer_status, useNA = "ifany")
```

Полезная сводка:

```r
auto_eval <- data.frame(
  n_clusters = nrow(summary_tbl),
  n_success = sum(summary_tbl$run_status %in% c("success", "speculative"), na.rm = TRUE),
  n_labeled = sum(summary_tbl$output_status == "labeled", na.rm = TRUE),
  n_abstain = sum(summary_tbl$output_status == "abstain", na.rm = TRUE),
  n_placeholder = sum(summary_tbl$run_status == "placeholder", na.rm = TRUE),
  n_repair = if ("repair_used" %in% names(summary_tbl)) {
    sum(as.logical(summary_tbl$repair_used), na.rm = TRUE)
  } else {
    NA_integer_
  },
  mean_num_predict_used = if ("num_predict_used" %in% names(summary_tbl)) {
    mean(as.numeric(summary_tbl$num_predict_used), na.rm = TRUE)
  } else {
    NA_real_
  }
)

auto_eval

utils::write.csv(
  auto_eval,
  file.path(report_root, "evaluation", "single_model_auto_eval.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
```

### 19.2. Содержательная полезность

Откройте review cards и смотрите:

```text
- короткий ли label
- не слишком ли общий label
- не заявляет ли модель больше, чем видно из evidence
- не путает ли open grassland / forest edge / woodland
- есть ли объяснение, на какие виды и признаки она опиралась
```

Для `phi4-mini:latest` нормально ожидать больше generic labels и abstain, чем
у более крупных моделей. Это не поломка pipeline. Это свойство слабой локальной
модели.

---

## 20. Параметры, которые пользователь может менять

### `MODEL_NAME`

```r
MODEL_NAME <- "phi4-mini:latest"
```

Меняет модель. Для этого документа модель фиксирована, но технически можно
поставить другое имя из `ollama list`.

### `OLLAMA_OPTIONS$num_ctx`

```r
OLLAMA_OPTIONS <- list(num_ctx = 8192L)
```

Меняет контекстное окно Ollama.

Примеры:

```r
OLLAMA_OPTIONS <- list(num_ctx = 4096L)
OLLAMA_OPTIONS <- list(num_ctx = 6144L)
OLLAMA_OPTIONS <- list(num_ctx = 8192L)
```

При уменьшении `num_ctx` желательно уменьшить и prompt/output budget:

```r
PROMPT_BUDGET_CHARS <- 6000L
NUM_PREDICT <- 1200L
```

### `PROMPT_BUDGET_CHARS`

```r
PROMPT_BUDGET_CHARS <- 10000L
```

Чем больше budget, тем больше evidence может попасть в prompt.
Чем меньше budget, тем меньше риск перегрузить маленькую модель, но тем больше
шанс потерять важные виды или ограничения.

### `NUM_PREDICT`

```r
NUM_PREDICT <- 2400L
```

Чем больше, тем больше места для ответа. Чем меньше, тем быстрее, но выше риск
обрыва structured output.

### `use_brainstorm`

```r
use_brainstorm = TRUE
```

Если `FALSE`, модель пропускает draft-analysis step и сразу идёт к label
decision. Обычно для маленькой модели лучше оставить `TRUE`, потому что
brainstorm помогает собрать направление перед коротким label.

### `semantic_layer`

```r
semantic_layer = TRUE
```

Если `TRUE`, evidence дополняется экологическими indicator-derived summaries
при наличии EIVE/Tichý resources. Для маленькой модели это часто полезно.

### `label_mode`

```r
label_mode = "open"
```

В этом документе используем open labels. `constrained` vocabulary и prompt
editing выносятся в отдельный документ.

---

## 21. Минимальный troubleshooting

### `could not find function "label_clusters"`

Перезагрузите пакет:

```r
pkgload::load_all(
  "D:/documents/coctrailr/cocktailr",
  quiet = TRUE
)
```

### Ollama не отвечает

В PowerShell:

```powershell
ollama list
ollama ps
```

Если модель зависла:

```powershell
ollama stop phi4-mini:latest
```

Потом повторите smoke test.

### Модель слишком долго отвечает

Уменьшите:

```r
OLLAMA_OPTIONS <- list(num_ctx = 4096L)
PROMPT_BUDGET_CHARS <- 6000L
NUM_PREDICT <- 1200L
```

### Очень много `abstain`

Проверьте:

```r
semantic_layer_status
failure_reason
review_file
llm_logs
```

Возможные причины:

```text
- слишком маленькое контекстное окно
- evidence слишком обрезано
- semantic layer не сработал
- модель слишком слабая для экологического labeling
- кластеры действительно смешанные
```

### Review cards есть, но графики без labels

Проверьте:

```r
inherits(run$label_registry, "cluster_label_registry")
run$label_registry
```

И запускайте plotting helpers с:

```r
label_registry = run$label_registry
```

а не без registry.

### Нужно начать новый запуск

Просто заново выполните шаг 5 в PowerShell. Он создаст новый timestamp и
перепишет `_ACTIVE_REPORT_ROOT.txt`. Старые результаты останутся в старой папке.

---

## 22. Короткая карта всего процесса

```text
PowerShell step 1
  -> создаёт REPORT_ROOT

R step 2
  -> загружает пакет и настройки модели

R step 3
  -> SPE.csv -> обезличенный numeric long input

R step 4
  -> cocktail_cluster()
  -> selected_clusters

R step 5
  -> cluster_evidence()
  -> semantic layer diagnostic

R step 6
  -> dry_run
  -> проверить request / num_ctx / prompt payload

R step 7
  -> smoke label_clusters() на 1 cluster

PowerShell step 8
  -> leakage scan smoke logs

R step 9
  -> full label_clusters() на все selected clusters

R step 10-12
  -> summary, review cards, prompt logs, plots

PowerShell step 13
  -> leakage scan full logs
```

Главный результат для обычного пользователя:

```text
runs/phi4-mini_latest/full/phi4-mini_latest_summary.csv
runs/phi4-mini_latest/full/cluster_reviews/...
runs/phi4-mini_latest/full/phi4-mini_latest_label_registry_copy.csv
plots/phi4-mini_latest_cluster_hclust_labels.png
plots/phi4-mini_latest_full_cocktail_plot.png
```
