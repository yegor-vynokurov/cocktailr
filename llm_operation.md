# Работа с локальной LLM через Ollama и `cocktailr`

Этот документ описывает только верхнеуровневый сценарий работы с
локальной LLM в `cocktailr`.

Текущий основной пользовательский вход для labeling:

- `label_clusters()`

Детальный manual pipeline, расширенные аргументы и пошаговые примеры
вынесены в
[LABELING_STEP_BY_STEP.md](LABELING_STEP_BY_STEP.md).

## Что делает LLM-слой

На текущем MVP-этапе локальная LLM нужна, чтобы:

- предложить название кластера
- дать короткую интерпретацию
- вернуть структурированный результат, который можно валидировать и
  сохранить как review card

Практически это значит: обычному пользователю не нужно вручную
вызывать цепочку
`cluster_evidence() -> llm_label_cluster() -> validate_cluster_label() -> render_cluster_review()`.
Основной сценарий должен идти через `label_clusters()`.

## Быстрый старт

### 1. Установите Ollama

- Windows: <https://ollama.com/download>
- macOS / Linux: та же официальная страница

Проверьте установку:

```powershell
ollama --version
```

### 2. Загрузите модель

Текущая рекомендуемая стартовая модель:

```powershell
ollama pull gemma4:12b
```

При желании проверьте, что модель запускается:

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
  variant = "strict_abstention_gate_v1",
  workflow_steps = 1,
  timeout_sec = 600,
  num_predict = 600
)

run$summary
run$summary$review_file
```

Если аргумент `clusters` не указан, `label_clusters()` по умолчанию
обрабатывает до 10 score-ranked кластеров.

## Текущая рекомендуемая комбинация

Для обычного первого запуска сейчас рекомендуется:

- модель: `gemma4:12b`
- prompt variant: `strict_abstention_gate_v1`
- workflow: `workflow_steps = 1`
- safer first-run settings:
  `timeout_sec = 600`, `num_predict = 600`

Если при `num_predict = 600` появляется ошибка с `EOF` или обрезанный
JSON-ответ, увеличьте `num_predict` до `1200`.

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
  variant = "strict_abstention_gate_v1",
  workflow_steps = 1,
  timeout_sec = 600,
  num_predict = 600
)
```

Текущий практический смысл:

- `gemma4:12b` — основной рекомендованный baseline
- `qwen3.5:9b-q4_K_M` — полезная вторичная альтернатива для сравнений и
  экспериментов

## Куда сохраняются результаты

По умолчанию финальные review cards сохраняются в:

- `temp/reports/cluster_reviews/`

Raw LLM logging через `log_dir` опционален. Если он включён, логи можно
писать, например, в:

- `temp/llm_logs/`

Важно:

- `temp/` не обязана существовать заранее
- нужные подпапки создаются автоматически
- `temp/` можно удалить целиком, это не ломает пакет
- свежий clone репозитория без `temp/` работает нормально

Если проект запущен из локального source checkout, относительные пути
вроде `temp/reports/cluster_reviews/` и `temp/llm_logs/`
автоматически разрешаются относительно корня пакета `cocktailr`.

## Когда нужен `LABELING_STEP_BY_STEP.md`

Переходите к
[LABELING_STEP_BY_STEP.md](LABELING_STEP_BY_STEP.md),
если вам нужно:

- загрузить реальный CSV или long-format dataset
- выбрать конкретные cluster IDs вручную
- разобрать manual pipeline по шагам
- использовать `llm_label_cluster()` напрямую
- включить `full = TRUE`
- сохранить raw `log_dir`
- детально разбираться с prompt variants
- менять `workflow_steps`

## Короткий Troubleshooting

### `could not find function "label_clusters"`

Обычно это значит, что в сессии загружена старая версия пакета.

Используйте:

```r
pkgload::load_all("D:/documents/coctrailr/cocktailr")
```

или переустановите локальный пакет.

### Timeout / модель отвечает слишком долго

Для первого запуска используйте:

```r
timeout_sec = 600
num_predict = 600
```

Если этого мало, увеличьте `num_predict` до `1200`.

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
- не нужен ли другой cluster ID

Если нужен более глубокий разбор, переходите в
[LABELING_STEP_BY_STEP.md](LABELING_STEP_BY_STEP.md).
