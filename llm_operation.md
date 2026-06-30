# Работа с локальной LLM через Ollama и `cocktailr`

Этот документ описывает только верхнеуровневый сценарий работы с
локальной LLM в `cocktailr`.

Основной пользовательский вход для labeling сейчас:

- `label_clusters()`

Детальный пошаговый pipeline, ручные low-level вызовы и расширенные
примеры вынесены в
[LABELING_STEP_BY_STEP.md](LABELING_STEP_BY_STEP.md).

## Что делает LLM-слой

LLM-слой не заменяет обычный Cocktail workflow.

Он используется после кластеризации, чтобы:

- предложить человекочитаемое название кластера
- дать короткую интерпретацию
- сохранить review card для человека
- при необходимости подготовить labels для рисунков и `hclust`

Практически это означает, что обычный пользователь чаще всего работает
не с цепочкой
`cluster_evidence() -> llm_label_cluster() -> validate_cluster_label() -> render_cluster_review()`,
а сразу с `label_clusters()`.

## Быстрый старт

### 1. Установите Ollama

Официальная страница:

- <https://ollama.com/download>

Проверьте установку:

```powershell
ollama --version
```

### 2. Загрузите модель

Текущий рекомендуемый baseline:

```powershell
ollama pull gemma4:12b
```

При желании можно проверить, что модель стартует:

```powershell
ollama run gemma4:12b
```

### 3. Запустите labeling из R

Если вы работаете из локального dev-checkout, сначала загрузите свежую
версию пакета:

```r
pkgload::load_all("D:/documents/coctrailr/cocktailr")
```

Минимальный рекомендуемый пример:

```r
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
  num_predict = 2400
)

run$summary
run$summary$review_file
```

## Текущая рекомендуемая комбинация

Для обычного первого запуска сейчас рекомендуется:

- модель: `gemma4:12b`
- prompt variant: `label_primary_v1`
- workflow: `workflow_steps = 1`
- safer first-run settings:
  `timeout_sec = 600`, `num_predict = 2400`

Если строгий проход всё ещё упирается в EOF / truncated JSON, `label_clusters()`
автоматически попробует `num_predict = 4800`, а затем `9600`.

Если модель начинает путать короткий label с длинным объяснением, полезно
переключиться на:

- `workflow_steps = 3`

Этот режим сначала делает свободный draft-analysis, потом отдельно выбирает
короткий label через каскад `primary -> soft -> broad`, и только после этого
генерирует итоговое structured explanation. Для более слабых локальных моделей
это часто устойчивее, чем пытаться получить и label, и explanation за один шаг.

Если весь Stage-B каскад `primary -> soft -> broad` исчерпан, workflow теперь
не тратит ещё один LLM-вызов на explanation pass. Вместо этого он
детерминированно завершает run broad fallback-лейблом `chaotic cluster` и
сразу помечает такой результат как требующий human review.

Дополнительно теперь есть общий переключатель пространства меток:

- `label_mode = "open"`
  Свободное лейблирование, текущий default.
- `label_mode = "constrained"`
  Модель обязана выбирать label из coarse vocabulary. Если не задавать свой
  словарь, используется packaged vocabulary по умолчанию.
- `label_mode = "dynamic"`
  Работает вместе с `workflow_steps = 3`: Stage A сначала предлагает candidate
  labels, а Stage B старается переиспользовать именно их.

Для собственного constrained vocabulary можно задать:

```r
options(
  cocktailr.cluster_label_vocabulary_path =
    "path/to/your/custom_cluster_label_vocabulary.json"
)
```

Важно для миграции prompt-слоя:

- публично поддерживаются только `label_primary_v1`,
  `label_soft_v1`, `label_broad_v1`
- старые versioned prompt IDs всё ещё принимаются как compatibility
  aliases, но больше не являются отдельными рекомендуемыми режимами
- архивные копии retired prompt texts лежат локально в
  `temp/prompt_archive/cluster_labeling/`
- внутренние service prompts вынесены в
  `inst/prompts/internal_cluster_labeling/`

## Дополнительный semantic layer

При желании можно включить дополнительное evidence-enrichment звено:

```r
run_sem <- label_clusters(
  x = res,
  model = "gemma4:12b",
  variant = "label_primary_v1",
  workflow_steps = 1,
  semantic_layer = TRUE,
  timeout_sec = 600,
  num_predict = 2400
)
```

Что это делает:

- перед LLM-вызовом evidence bundle дополняется indicator-derived
  ecological axes из внешних таблиц EIVE/Tichy
- эти semantic axes попадают в prompt как дополнительные ecological hints
- итог `run$summary` показывает, удалось ли enrichment реально собрать:
  `semantic_layer_used`, `semantic_layer_status`,
  `semantic_layer_error`

Важно:

- это вспомогательный слой, а не замена основного Cocktail evidence
- на маленьких моделях типа `phi4-mini` он может добавить полезный
  контекст, но не гарантирует снятие abstain

## Что означает speculative fallback

По умолчанию speculative fallback выключен:

```r
speculative_fallback_mode = "off"
```

Это важно: обычный default workflow пытается получить только нормальный
strict result или abstain.

Если нужен более мягкий режим для рисунков и review queues, можно
включить:

```r
run_spec <- label_clusters(
  x = res,
  model = "gemma4:12b",
  variant = "label_primary_v1",
  workflow_steps = 1,
  speculative_fallback_mode = "after_nonaccepted",
  timeout_sec = 600,
  num_predict = 2400,
  labels_for_imgs = TRUE
)
```

Смысл этого режима:

- accepted strict labels остаются обычными accepted labels
- fallback может запускаться не только после placeholder / no-valid-label,
  но и после валидного strict `abstain`
- успешный fallback label помечается как speculative и требует human review

Это не “второй основной prompt”.

Это отдельный workflow-слой поверх strict baseline.

Если нужен более узкий legacy-режим, при котором fallback включается
только после placeholder-ветки, можно вручную использовать
`speculative_fallback_mode = "after_rejection"`.

Текущая внутренняя fallback-лестница по умолчанию использует сначала
`label_soft_v1`, затем `label_broad_v1`. Более новые
варианты `v6-v9` сейчас считаются экспериментальными prompt assets, а
не пользовательским default.

## Как speculative labels выглядят снаружи

Типичная семантика:

- accepted: `c_12: Mixed Deciduous Woodland`
- speculative: `c_27: Woodland-transition assemblage*`

Звёздочка означает:

- tentative / speculative label
- strict validation не принял стабильный evidence-backed label

На графиках и в legend автоматически используется пояснение:

- `* tentative / speculative label; strict validation did not accept a stable evidence-backed label`

Полезные поля в `run$summary`:

- `run_status`
- `label_tier`
- `review_status`
- `is_speculative`

## Labels для рисунков

Если нужен plotting registry, включите:

```r
run_plot <- label_clusters(
  x = res,
  clusters = c("c_12", "c_26"),
  model = "gemma4:12b",
  variant = "label_primary_v1",
  workflow_steps = 1,
  timeout_sec = 600,
  num_predict = 2400,
  labels_for_imgs = TRUE
)
```

После этого:

- рядом с review cards сохранится `cluster_label_registry.csv`
- `cocktail_plot(..., label_registry = "auto")` сможет подхватить его автоматически
- `label_hclust_leaves(..., label_registry = "auto", x = res)` тоже сможет использовать эти labels

## Как сменить модель

Сначала скачайте модель через Ollama:

```powershell
ollama pull qwen3.5:9b-q4_K_M
```

Потом просто поменяйте аргумент `model =`:

```r
run <- label_clusters(
  x = res,
  model = "qwen3.5:9b-q4_K_M",
  variant = "label_primary_v1",
  workflow_steps = 1,
  timeout_sec = 600,
  num_predict = 2400
)
```

Практический смысл текущих кандидатов:

- `gemma4:12b`
  Основной рекомендуемый baseline.
- `qwen3.5:9b-q4_K_M`
  Полезная secondary alternative для сравнений.
- `phi4-mini`
  Экспериментальный вариант для лёгкого smoke-test запуска на более
  слабой машине. Для устойчивого ecological labeling ему может не
  хватать знаний, поэтому он чаще уходит в generic labels или требует
  speculative ladder.

Если нужен более подробный разбор моделей и запусков, смотрите
[LABELING_STEP_BY_STEP.md](LABELING_STEP_BY_STEP.md).

## Куда сохраняются результаты

По умолчанию финальные review cards сохраняются в:

- `temp/reports/cluster_reviews/`

Если включён `labels_for_imgs = TRUE`, рядом сохраняется:

- `cluster_label_registry.csv`

Папка `temp/` не обязана существовать заранее. Она создаётся
автоматически. Её можно удалить целиком: это не ломает пакет, и новый
clone репозитория без `temp/` работает нормально.

## Короткий troubleshooting

### `could not find function "label_clusters"`

Обычно это означает, что в R-сессии загружена старая версия пакета.

Используйте:

```r
pkgload::load_all("D:/documents/coctrailr/cocktailr")
```

или переустановите локальный пакет.

### Timeout / модель отвечает слишком долго

Для первого реального запуска используйте:

```r
timeout_sec = 600
num_predict = 2400
```

Если ответа всё ещё не хватает, дайте strict-pass дойти до автоматической
лестницы `4800 -> 9600`.

### EOF / truncated JSON

Обычно это значит, что модель начала structured output, но не
договорила его до конца.

Первая практическая реакция:

- оставить `timeout_sec = 600`
- начать с `num_predict = 2400`
- если strict-pass снова обрывается по EOF, дождаться автоматических ретраев на `4800` и `9600`

### Не удаётся подключиться к Ollama

Проверьте:

```powershell
ollama ls
```

Если команда не отвечает, сначала запустите или перезапустите Ollama.

### Модель слишком часто abstain'ится

Это не обязательно ошибка.

Сначала проверьте:

- действительно ли кластер интерпретируемый
- не слишком ли он смешанный
- не стоит ли взять другой cluster ID

Если нужна подробная диагностика по шагам, переходите в
[LABELING_STEP_BY_STEP.md](LABELING_STEP_BY_STEP.md).
