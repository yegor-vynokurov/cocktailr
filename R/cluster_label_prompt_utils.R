# Internal helpers for packaged prompt and schema assets.
#
# The public LLM API should read like workflow orchestration. Prompt catalog
# lookup, template interpolation, and request assembly live here to keep the
# main labeling file focused on control flow.

.package_asset_path <- function(...) {
  rel <- file.path(...)

  packaged <- system.file(..., package = "cocktailr")
  if (nzchar(packaged)) {
    return(normalizePath(packaged, winslash = "/", mustWork = TRUE))
  }

  ns_path <- tryCatch(
    getNamespaceInfo(asNamespace("cocktailr"), "path"),
    error = function(e) ""
  )

  if (is.character(ns_path) && nzchar(ns_path)) {
    candidates <- unique(c(
      file.path(ns_path, rel),
      file.path(ns_path, "inst", rel)
    ))
    existing <- candidates[file.exists(candidates)]
    if (length(existing)) {
      return(normalizePath(existing[[1L]], winslash = "/", mustWork = TRUE))
    }
  }

  ""
}

.cluster_label_prompt_catalog_path <- function() {
  path <- .package_asset_path("prompts", "cluster_labeling", "catalog.json")

  if (!nzchar(path)) {
    stop(
      "Could not locate the packaged cluster label prompt catalog. ",
      "Reinstall the package or use `pkgload::load_all()` from the package root."
    )
  }

  path
}

.cluster_label_schema_path <- function(
    schema_path = NULL,
    default_schema_name = "cluster_label_output_schema.json"
) {
  if (!is.null(schema_path)) {
    if (!file.exists(schema_path)) {
      stop("`schema_path` does not exist: ", schema_path)
    }
    return(normalizePath(schema_path, winslash = "/", mustWork = TRUE))
  }

  path <- .package_asset_path("schemas", default_schema_name)

  if (!nzchar(path)) {
    stop(
      "Could not locate the packaged schema asset '",
      default_schema_name,
      "'. ",
      "Reinstall the package or use `pkgload::load_all()` from the package root."
    )
  }

  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.cluster_label_gate_schema_path <- function(schema_path = NULL) {
  .cluster_label_schema_path(
    schema_path = schema_path,
    default_schema_name = "cluster_label_gate_schema.json"
  )
}

.read_text_file <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

.replace_fixed_scalar <- function(x, pattern, replacement) {
  gsub(pattern, replacement, x, fixed = TRUE)
}

.interpolate_prompt_template <- function(template, values) {
  out <- template
  for (nm in names(values)) {
    out <- .replace_fixed_scalar(out, nm, values[[nm]])
  }
  out
}

.read_cluster_label_schema <- function(schema_path = NULL) {
  path <- .cluster_label_schema_path(schema_path)
  text <- .read_text_file(path)
  parsed <- jsonlite::fromJSON(text, simplifyVector = FALSE)

  list(
    path = path,
    text = text,
    parsed = parsed,
    required = parsed$required %||% character(0)
  )
}

.read_cluster_label_prompt_catalog <- function() {
  path <- .cluster_label_prompt_catalog_path()
  text <- .read_text_file(path)
  parsed <- jsonlite::fromJSON(text, simplifyVector = FALSE)

  if (!is.list(parsed) ||
      is.null(parsed$system_prompt_path) ||
      is.null(parsed$variants)) {
    stop("The packaged cluster label prompt catalog is malformed.")
  }

  if (!length(parsed$variants) || is.null(names(parsed$variants))) {
    stop("The packaged cluster label prompt catalog must define named variants.")
  }

  list(
    path = path,
    dir = dirname(path),
    text = text,
    parsed = parsed
  )
}

.cluster_label_prompt_asset_path <- function(catalog, rel_path) {
  rel_path <- .arg_scalar_character(rel_path, "rel_path")
  path <- file.path(catalog$dir, rel_path)

  if (!file.exists(path)) {
    stop("Prompt asset referenced by the catalog does not exist: ", rel_path)
  }

  normalizePath(path, winslash = "/", mustWork = TRUE)
}

# The constrained phi4-oriented prompt variants can optionally read a
# user-supplied coarse label vocabulary. Keeping this in prompt assembly,
# rather than inside the LLM workflow file, makes prompt assets easier to test
# and swap without touching the control-flow code.
.cluster_label_vocabulary_override_path <- function() {
  path <- getOption("cocktailr.cluster_label_vocabulary_path", NULL)
  if (is.null(path)) {
    return(NULL)
  }

  .arg_scalar_character(
    path,
    'getOption("cocktailr.cluster_label_vocabulary_path")'
  )
}

.resolve_cluster_label_vocabulary_path <- function(catalog, variant_def) {
  override_path <- .cluster_label_vocabulary_override_path()

  if (!is.null(override_path)) {
    resolved <- .resolve_cocktailr_output_path(override_path)
    if (!file.exists(resolved)) {
      stop(
        "The configured coarse label vocabulary file does not exist: ",
        override_path
      )
    }

    return(normalizePath(resolved, winslash = "/", mustWork = TRUE))
  }

  rel_path <- variant_def$vocabulary_path %||% NULL
  if (is.null(rel_path)) {
    return(NULL)
  }

  .cluster_label_prompt_asset_path(catalog, rel_path)
}

.render_cluster_label_vocabulary_text <- function(vocabulary) {
  labels <- vocabulary$labels %||% NULL
  if (!is.list(labels) || !length(labels)) {
    stop(
      "The coarse label vocabulary must define a non-empty `labels` list."
    )
  }

  lines <- c(
    paste0(
      "Vocabulary name: ",
      .null_default(
        .as_scalar_character(vocabulary$vocabulary_name),
        "coarse_label_vocabulary"
      )
    ),
    paste0(
      "Vocabulary version: ",
      .null_default(
        .as_scalar_character(vocabulary$vocabulary_version),
        "unknown"
      )
    ),
    "Allowed labels:",
    ""
  )

  for (i in seq_along(labels)) {
    label <- labels[[i]]
    canonical <- .as_scalar_character(label$canonical_label)
    display <- .as_scalar_character(label$display_label)
    description <- .as_scalar_character(label$short_description)
    use_when <- .as_scalar_character(label$use_when)

    if (is.na(canonical) || !nzchar(canonical)) {
      stop("Vocabulary label #", i, " is missing `canonical_label`.")
    }
    if (is.na(display) || !nzchar(display)) {
      stop("Vocabulary label `", canonical, "` is missing `display_label`.")
    }
    if (is.na(description) || !nzchar(description)) {
      stop(
        "Vocabulary label `",
        canonical,
        "` is missing `short_description`."
      )
    }

    lines <- c(
      lines,
      paste0("- `", canonical, "` -> `", display, "`: ", description)
    )

    if (!is.na(use_when) && nzchar(use_when)) {
      lines <- c(lines, paste0("  Use when: ", use_when))
    }
  }

  paste(lines, collapse = "\n")
}

.read_cluster_label_vocabulary <- function(catalog, variant_def) {
  path <- .resolve_cluster_label_vocabulary_path(catalog, variant_def)

  if (is.null(path)) {
    return(list(
      path = NULL,
      text = NULL,
      parsed = NULL,
      rendered = ""
    ))
  }

  text <- .read_text_file(path)
  parsed <- jsonlite::fromJSON(text, simplifyVector = FALSE)

  if (!is.list(parsed) || is.null(parsed$labels)) {
    stop("The coarse label vocabulary file is malformed: ", path)
  }

  list(
    path = path,
    text = text,
    parsed = parsed,
    rendered = .render_cluster_label_vocabulary_text(parsed)
  )
}

.build_cluster_label_prompt <- function(
    evidence,
    variant,
    schema_path = NULL,
    temperature = NULL,
    top_p = NULL,
    seed = NULL,
    num_predict = NULL
) {
  catalog <- .read_cluster_label_prompt_catalog()
  catalog_def <- catalog$parsed
  variant_def <- catalog_def$variants[[variant]]

  if (is.null(variant_def)) {
    stop(
      "`variant` must be one of: ",
      paste(names(catalog_def$variants), collapse = ", "),
      "."
    )
  }

  schema <- .read_cluster_label_schema(schema_path)
  evidence_text <- .format_cluster_evidence_prompt(evidence)
  vocabulary <- .read_cluster_label_vocabulary(catalog, variant_def)
  system_prompt_path <- .cluster_label_prompt_asset_path(
    catalog,
    catalog_def$system_prompt_path
  )
  user_prompt_path <- .cluster_label_prompt_asset_path(
    catalog,
    variant_def$user_prompt_path
  )

  template_values <- list(
    "{{CLUSTER_ID}}" = evidence$meta$cluster_id,
    "{{OUTPUT_SCHEMA_JSON}}" = schema$text,
    "{{CLUSTER_EVIDENCE_TEXT}}" = evidence_text,
    "{{COARSE_LABEL_VOCABULARY_TEXT}}" = vocabulary$rendered %||% ""
  )

  system_content <- .interpolate_prompt_template(
    .read_text_file(system_prompt_path),
    template_values
  )
  user_content <- .interpolate_prompt_template(
    .read_text_file(user_prompt_path),
    template_values
  )

  generation <- catalog_def$default_generation
  generation$temperature <- temperature %||% variant_def$temperature %||% generation$temperature
  generation$top_p <- top_p %||% variant_def$top_p %||% generation$top_p
  generation$seed <- as.integer(seed %||% generation$seed)
  generation$num_predict <- as.integer(num_predict %||% generation$num_predict)

  list(
    cluster_id = evidence$meta$cluster_id,
    variant = variant,
    task_type = variant_def$task_type %||% "label",
    catalog_path = catalog$path,
    schema_path = schema$path,
    schema_text = schema$text,
    schema_required = schema$required,
    schema_object = schema$parsed,
    evidence_text = evidence_text,
    vocabulary_path = vocabulary$path,
    vocabulary_text = vocabulary$rendered,
    vocabulary_object = vocabulary$parsed,
    system_path = system_prompt_path,
    user_path = user_prompt_path,
    system = system_content,
    user = user_content,
    messages = list(
      list(role = "system", content = system_content),
      list(role = "user", content = user_content)
    ),
    generation = generation
  )
}

.merge_named_lists <- function(base, override) {
  if (is.null(base)) {
    return(override)
  }
  if (is.null(override)) {
    return(base)
  }

  out <- base
  for (nm in names(override)) {
    out[[nm]] <- override[[nm]]
  }
  out
}

.build_ollama_label_request <- function(
    model,
    prompt_bundle,
    keep_alive = NULL,
    ollama_options = NULL
) {
  ollama_options <- .arg_named_list_or_null(ollama_options, "ollama_options")
  generation_options <- Filter(
    Negate(is.null),
    list(
      temperature = prompt_bundle$generation$temperature,
      top_p = prompt_bundle$generation$top_p,
      seed = prompt_bundle$generation$seed,
      num_predict = prompt_bundle$generation$num_predict
    )
  )
  options <- .merge_named_lists(ollama_options, generation_options)

  Filter(
    function(x) !is.null(x),
    list(
      model = model,
      messages = prompt_bundle$messages,
      stream = FALSE,
      think = FALSE,
      format = prompt_bundle$schema_object,
      options = options,
      keep_alive = keep_alive
    )
  )
}

.default_cluster_label_gate_variant <- function() {
  "gate_abstain_examples_v1"
}

.default_cluster_label_speculative_variants <- function() {
  opt <- getOption("cocktailr.speculative_fallback_variants", NULL)

  if (!is.null(opt)) {
    if (!is.character(opt) || !length(opt) || any(is.na(opt)) || any(!nzchar(opt))) {
      stop(
        "getOption(\"cocktailr.speculative_fallback_variants\") must be NULL ",
        "or a non-empty character vector of prompt variant IDs.",
        call. = FALSE
      )
    }

    return(unname(opt))
  }

  c("speculative_fallback_v3", "speculative_fallback_v4")
}

.default_cluster_label_speculative_model <- function() {
  model <- getOption("cocktailr.speculative_fallback_model", "phi4-mini:latest")
  if (.is_non_empty_scalar_character(model)) {
    return(model)
  }
  "phi4-mini:latest"
}

.default_cluster_label_speculative_num_predict <- function() {
  value <- suppressWarnings(as.integer(
    getOption("cocktailr.speculative_fallback_num_predict", 2400L)
  ))
  if (!is.finite(value) || is.na(value) || value < 1L) {
    return(2400L)
  }
  value
}

.default_cluster_label_speculative_ollama_options <- function() {
  opt <- getOption("cocktailr.speculative_fallback_ollama_options", NULL)
  if (!is.null(opt)) {
    opt <- .arg_named_list_or_null(
      opt,
      "getOption(\"cocktailr.speculative_fallback_ollama_options\")"
    )
  }

  .merge_named_lists(
    list(num_ctx = 8192L),
    opt
  )
}

.expect_prompt_task_type <- function(prompt_bundle, expected_task_type, variant) {
  actual_task_type <- prompt_bundle$task_type %||% "label"
  if (!identical(actual_task_type, expected_task_type)) {
    stop(
      "Prompt variant '", variant, "' is registered as task_type = '",
      actual_task_type, "' but this workflow requires task_type = '",
      expected_task_type, "'."
    )
  }
}
