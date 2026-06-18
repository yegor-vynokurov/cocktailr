# Работа с локальной LLM через Ollama и `cocktailr`

Этот документ описывает верхнеуровневый сценарий работы с локальной LLM
в `cocktailr`.

Главная идея сейчас такая:

- Ollama запускает локальную модель
- `cocktailr` обращается к ней из R
- основной пользовательский вход для labeling теперь:
  `label_clusters()`

Подробные пошаговые сценарии, ручной low-level pipeline и расширенные
аргументы вынесены в
[LABELING_STEP_BY_STEP.md](LABELING_STEP_BY_STEP.md).

## Что делает LLM-слой в проекте

На текущем MVP-этапе локальная LLM используется для того, чтобы:

- предложить название кластера
- дать краткую интерпретацию
- вернуть структурированный результат, пригодный для проверки и
  сохранения в markdown card

Рекомендуемый путь сейчас не вручную собирать цепочку из
`cluster_evidence()`, `llm_label_cluster()`,
`validate_cluster_label()` и `render_cluster_review()`,
а вызывать `label_clusters()`, которая делает это сама.

## Быстрый старт

### 1. Установите Ollama

Скачать:

- Windows: <https://ollama.com/download>
- macOS / Linux: там же на официальной странице

Проверьте, что команда доступна:

```powershell
ollama --version
```

### 2. Загрузите модель

Рекомендуемая стартовая модель для текущего workflow:

```powershell
ollama pull gemma4:12b
```

Проверьте, что она запускается:

```powershell
ollama run gemma4:12b
```

### 3. Запустите labeling из R
указывайте ваш путь на вашем локальном компьютере, например D:/documents/coctrailr/cocktailr
```r
pkgload::load_all("path/to/dir")

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
берёт первые top-10 кластеров, ранжированных по score.

## Текущая рекомендуемая комбинация

Для обычного первого запуска сейчас рекомендуется:

- модель: `gemma4:12b`
- prompt variant: `strict_abstention_gate_v1`
- workflow: `workflow_steps = 1`
- safer first-run settings:
  `timeout_sec = 600`, `num_predict = 600`

Если при `num_predict = 600` получаете ошибку с `EOF` или
недогенерацией, увеличьте до `1200`.

## Как сменить модель

Сначала скачайте модель через Ollama:

```powershell
ollama pull qwen3.5:9b-q4_K_M
```

Потом просто поменяйте аргумент `model =` в R:

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

Практический смысл:

- `gemma4:12b` сейчас основной рекомендованный baseline
- `qwen3.5:9b-q4_K_M` можно использовать как альтернативу для
  экспериментов и сравнений. Её вес немного меньше, чем гемма4 12 б.

## Альтернативные модели
Если нужны меньшие или большие модели, то любые, которые есть на сайте оллама, 
можно загрузить и использовать. Главное, чтобы они поддерживали 
структурированный оутпут. 

## Куда сохраняются результаты

По умолчанию финальные review cards идут в:

- `temp/reports/cluster_reviews/`

Если включить raw LLM logging через `log_dir`, логи можно писать,
например, в:

- `temp/llm_logs/`

Важно:

- `temp/` не обязана существовать заранее
- нужные подпапки создаются автоматически
- `temp/` можно удалить целиком, это не ломает пакет
- свежий clone репозитория без `temp/` работает нормально

Если проект запущен из локального source checkout, относительные пути
вроде `temp/reports/cluster_reviews/` и `temp/llm_logs/` автоматически
разрешаются относительно корня пакета `cocktailr`.

## Когда нужен `LABELING_STEP_BY_STEP.md`

Переходите к
[LABELING_STEP_BY_STEP.md](LABELING_STEP_BY_STEP.md),
если вам нужно:

- загрузить реальный CSV или long-format dataset
- выбрать конкретные cluster IDs вручную
- запустить low-level pipeline по шагам
- использовать `llm_label_cluster()` напрямую
- включить `full = TRUE`
- сохранить raw `log_dir`
- разбираться с prompt variants подробнее
- менять `workflow_steps`

## Короткий troubleshooting

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

Если всё равно мало, увеличьте `num_predict` до `1200`.

### Не удаётся подключиться к Ollama

Проверьте:

```powershell
ollama ls
```

Если команда не отвечает, сначала поднимите или перезапустите Ollama.

### Модель слишком часто abstain'ится

Это не обязательно ошибка.

Сначала проверьте:

- действительно ли кластер интерпретируемый
- не слишком ли он смешанный
- не нужен ли другой cluster ID

Если хотите разбираться глубже, смотрите
[LABELING_STEP_BY_STEP.md](LABELING_STEP_BY_STEP.md).

## Что дальше читать

- [README.md](README.md)
  Краткий обзор package workflow
- [LABELING_STEP_BY_STEP.md](LABELING_STEP_BY_STEP.md)
  Полный пошаговый сценарий
- [inst/prompts/cluster_labeling/README.md](inst/prompts/cluster_labeling/README.md)
  Краткие рекомендации по prompt variants
