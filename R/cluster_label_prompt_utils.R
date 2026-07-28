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

.default_cluster_label_internal_prompt_version <- function() {
  "v1"
}

.normalize_cluster_label_internal_prompt_version <- function(
    internal_prompt_version = .default_cluster_label_internal_prompt_version(),
    arg_name = "internal_prompt_version"
) {
  internal_prompt_version <- .arg_scalar_character(
    internal_prompt_version,
    arg_name
  )
  internal_prompt_version <- trimws(internal_prompt_version)

  if (!nzchar(internal_prompt_version)) {
    stop("`", arg_name, "` must not be empty.")
  }

  if (!grepl("^[A-Za-z0-9._-]+$", internal_prompt_version)) {
    stop(
      "`",
      arg_name,
      "` must be a simple folder name such as `v1` or `v2`."
    )
  }

  internal_prompt_version
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

.cluster_label_selection_schema_path <- function(schema_path = NULL) {
  .cluster_label_schema_path(
    schema_path = schema_path,
    default_schema_name = "cluster_label_selection_schema.json"
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
    required = .as_character_vector(parsed$required)
  )
}

.render_cluster_label_schema_prompt_text <- function(schema) {
  required_fields <- .as_character_vector(schema$required)

  lines <- c(
    "Structured output schema is attached separately via the API format field.",
    "Return exactly one raw JSON object and do not restate the schema."
  )

  if (length(required_fields)) {
    lines <- c(
      lines,
      paste0(
        "Required top-level fields: ",
        paste(required_fields, collapse = ", "),
        "."
      )
    )
  }

  paste(lines, collapse = "\n")
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

.cluster_label_prompt_variant_choices <- function(catalog_def) {
  variants <- names(catalog_def$variants %||% list())
  aliases <- names(catalog_def$legacy_variant_aliases %||% list())

  unique(c(variants, aliases))
}

.resolve_cluster_label_prompt_variant <- function(catalog_def, variant) {
  variant <- .arg_scalar_character(variant, "variant")
  variants <- catalog_def$variants %||% list()
  variant_def <- variants[[variant]] %||% NULL

  if (!is.null(variant_def)) {
    return(list(
      requested_variant = variant,
      resolved_variant = variant,
      variant_def = variant_def,
      is_legacy_alias = FALSE
    ))
  }

  aliases <- catalog_def$legacy_variant_aliases %||% list()
  alias_target <- aliases[[variant]] %||% NULL

  if (is.null(alias_target)) {
    stop(
      "`variant` must be one of: ",
      paste(.cluster_label_prompt_variant_choices(catalog_def), collapse = ", "),
      "."
    )
  }

  alias_target <- .arg_scalar_character(
    alias_target,
    paste0("catalog legacy alias target for `", variant, "`")
  )
  variant_def <- variants[[alias_target]] %||% NULL

  if (is.null(variant_def)) {
    stop(
      "The prompt catalog maps legacy alias `",
      variant,
      "` to missing variant `",
      alias_target,
      "`."
    )
  }

  list(
    requested_variant = variant,
    resolved_variant = alias_target,
    variant_def = variant_def,
    is_legacy_alias = TRUE
  )
}

.is_cluster_label_version_dir_name <- function(x) {
  x <- .as_scalar_character(x)
  !is.na(x) && grepl("^v[0-9]+[A-Za-z0-9._-]*$", x)
}

.cluster_label_prompt_asset_path <- function(
    catalog,
    rel_path,
    internal_prompt_version = .default_cluster_label_internal_prompt_version()
) {
  rel_path <- .arg_scalar_character(rel_path, "rel_path")
  rel_path_normalized <- gsub("\\\\", "/", rel_path)
  internal_prefix <- "../internal_cluster_labeling/"

  if (startsWith(rel_path_normalized, internal_prefix)) {
    internal_prompt_version <- .normalize_cluster_label_internal_prompt_version(
      internal_prompt_version
    )
    internal_root <- file.path(
      catalog$dir,
      "..",
      "internal_cluster_labeling",
      internal_prompt_version
    )

    if (!dir.exists(internal_root)) {
      stop(
        "Internal prompt version folder does not exist: `",
        internal_prompt_version,
        "` under `",
        normalizePath(
          file.path(catalog$dir, "..", "internal_cluster_labeling"),
          winslash = "/",
          mustWork = FALSE
        ),
        "`."
      )
    }

    internal_rel_path <- substr(
      rel_path_normalized,
      nchar(internal_prefix) + 1L,
      nchar(rel_path_normalized)
    )
    internal_rel_parts <- strsplit(
      internal_rel_path,
      "/",
      fixed = TRUE
    )[[1L]]

    if (length(internal_rel_parts) >= 2L &&
        .is_cluster_label_version_dir_name(internal_rel_parts[[1L]])) {
      internal_rel_path <- paste(internal_rel_parts[-1L], collapse = "/")
    }

    path <- file.path(internal_root, internal_rel_path)

    if (!file.exists(path)) {
      stop(
        "Internal prompt asset does not exist for `internal_prompt_version = \"",
        internal_prompt_version,
        "\"`: ",
        rel_path
      )
    }

    return(normalizePath(path, winslash = "/", mustWork = TRUE))
  }

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

.default_cluster_label_packaged_vocabulary_rel_path <- function() {
  "vocabulary/coarse_label_vocabulary_core_v1.json"
}

.resolve_cluster_label_vocabulary_path <- function(
    catalog,
    variant_def,
    fallback_rel_path = NULL
) {
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
    rel_path <- fallback_rel_path %||% NULL
  }
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

.read_cluster_label_vocabulary <- function(
    catalog,
    variant_def,
    fallback_rel_path = NULL
) {
  path <- .resolve_cluster_label_vocabulary_path(
    catalog = catalog,
    variant_def = variant_def,
    fallback_rel_path = fallback_rel_path
  )

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

.normalize_cluster_label_candidate_display <- function(text) {
  text <- .as_scalar_character(text)
  if (is.na(text) || !nzchar(trimws(text))) {
    return(NA_character_)
  }

  text <- trimws(text)
  text <- gsub("^[[:space:]]*[-*•]+[[:space:]]*", "", text, perl = TRUE)
  text <- gsub("^[[:space:]]*[0-9]+[.)][[:space:]]*", "", text, perl = TRUE)
  text <- gsub("`", "", text, fixed = TRUE)
  text <- gsub("^[\"']+|[\"']+$", "", text, perl = TRUE)
  text <- sub("^[[:alpha:]][[:alpha:] ]*:[[:space:]]*", "", text, perl = TRUE)
  text <- sub("[[:space:]]*\\([^)]*\\)[[:space:]]*$", "", text, perl = TRUE)
  text <- sub("[[:space:]]*:[[:space:]].*$", "", text, perl = TRUE)
  text <- gsub("[\\[\\],]+", " ", text, perl = TRUE)
  text <- gsub("[.]+$", "", text, perl = TRUE)
  text <- gsub("\\s+", " ", text, perl = TRUE)
  text <- trimws(text)

  lowered <- tolower(text)
  meta_reply_patterns <- c(
    "^(this|that|it)\\b.*\\b(answer|reply|response)\\b",
    "\\bdoes not follow\\b",
    "\\bshort label decision contract\\b",
    "\\btoo long\\b.*\\bcontract\\b"
  )
  if (any(vapply(
    meta_reply_patterns,
    function(pattern) grepl(pattern, lowered, perl = TRUE),
    logical(1)
  ))) {
    return(NA_character_)
  }

  word_count <- length(strsplit(text, "\\s+", perl = TRUE)[[1L]])

  max_chars <- getOption(
    "cocktailr.label_candidate_max_chars",
    320L
  )

  max_words <- getOption(
    "cocktailr.label_candidate_max_words",
    40L
  )

  if (!nzchar(text) || nchar(text) > max_chars || word_count > max_words) {
    return(NA_character_)
  }

  text
}

.cluster_label_candidate_to_canonical <- function(display_label) {
  display_label <- .as_scalar_character(display_label)
  if (is.na(display_label) || !nzchar(trimws(display_label))) {
    return(NA_character_)
  }

  canonical <- tolower(display_label)
  canonical <- gsub("[^a-z0-9]+", "_", canonical, perl = TRUE)
  canonical <- gsub("_+", "_", canonical, perl = TRUE)
  canonical <- gsub("^_+|_+$", "", canonical, perl = TRUE)

  max_canonical_chars <- getOption(
    "cocktailr.label_candidate_canonical_max_chars",
    300L
  )

  if (!nzchar(canonical) || nchar(canonical) > max_canonical_chars) {
    return(NA_character_)
  }

  canonical
}

.extract_cluster_label_candidates_from_draft <- function(
    draft_analysis_text,
    max_candidates = 8L
) {
  draft_analysis_text <- .as_scalar_character(draft_analysis_text)
  max_candidates <- .arg_positive_integer(
    max_candidates,
    "max_candidates"
  )

  if (is.na(draft_analysis_text) || !nzchar(trimws(draft_analysis_text))) {
    return(list())
  }

  lines <- strsplit(draft_analysis_text, "\n", fixed = TRUE)[[1L]]
  lines <- trimws(lines)
  section_idx <- grep(
    "candidate labels",
    tolower(lines),
    fixed = TRUE
  )

  candidate_lines <- character(0)
  if (length(section_idx)) {
    idx <- section_idx[[1L]]
    heading_line <- lines[[idx]]
    heading_remainder <- sub(
      ".*candidate labels[[:space:]]*:?[[:space:]]*",
      "",
      heading_line,
      ignore.case = TRUE,
      perl = TRUE
    )
    if (nzchar(heading_remainder) &&
        !identical(trimws(heading_remainder), trimws(heading_line))) {
      candidate_lines <- c(candidate_lines, heading_remainder)
    }

    if (idx < length(lines)) {
      for (line in lines[(idx + 1L):length(lines)]) {
        line_trim <- trimws(line)
        if (!nzchar(line_trim)) {
          if (length(candidate_lines)) {
            break
          }
          next
        }

        lower_line <- tolower(line_trim)
        if (grepl(
          "^(possible interpretations|main signal|noise or conflicts|what not to overclaim)\\b",
          lower_line,
          perl = TRUE
        ) || grepl("^[0-9]+\\.", lower_line, perl = TRUE)) {
          break
        }

        candidate_lines <- c(candidate_lines, line_trim)
      }
    }
  }

  if (!length(candidate_lines)) {
    return(list())
  }

  raw_candidates <- unlist(
    strsplit(candidate_lines, ";", fixed = TRUE),
    use.names = FALSE
  )
  raw_candidates <- raw_candidates[nzchar(trimws(raw_candidates))]

  out <- list()
  seen <- character(0)
  for (candidate in raw_candidates) {
    display_label <- .normalize_cluster_label_candidate_display(candidate)
    canonical_label <- .cluster_label_candidate_to_canonical(display_label)

    if (is.na(display_label) || is.na(canonical_label)) {
      next
    }
    if (canonical_label %in% seen) {
      next
    }

    seen <- c(seen, canonical_label)
    out[[length(out) + 1L]] <- list(
      display_label = display_label,
      canonical_label = canonical_label,
      source_text = .as_scalar_character(candidate)
    )

    if (length(out) >= max_candidates) {
      break
    }
  }

  out
}

.render_dynamic_label_candidates_text <- function(candidates) {
  if (!is.list(candidates) || !length(candidates)) {
    return("")
  }

  lines <- c(
    "Label-space guidance:",
    "",
    "Dynamic label mode is active.",
    "Prefer reusing one of the draft-derived candidate labels below, or a lightly normalized equivalent.",
    "Do not invent a very different labeled answer unless every listed candidate is clearly unusable.",
    "",
    "Draft-derived candidate labels:",
    ""
  )

  for (candidate in candidates) {
    display_label <- .as_scalar_character(candidate$display_label)
    canonical_label <- .as_scalar_character(candidate$canonical_label)
    if (is.na(display_label) || is.na(canonical_label)) {
      next
    }

    lines <- c(
      lines,
      paste0(
        "- `",
        display_label,
        "` -> `",
        canonical_label,
        "`"
      )
    )
  }

  paste(lines, collapse = "\n")
}

.render_cluster_label_candidate_block_text <- function(candidates) {
  if (!is.list(candidates) || !length(candidates)) {
    return("")
  }

  lines <- c(
    "Code-extracted candidate labels:",
    ""
  )

  for (candidate in candidates) {
    display_label <- .as_scalar_character(candidate$display_label)
    canonical_label <- .as_scalar_character(candidate$canonical_label)
    if (is.na(display_label) || is.na(canonical_label)) {
      next
    }

    lines <- c(
      lines,
      paste0("- `", display_label, "` -> `", canonical_label, "`")
    )
  }

  if (length(lines) <= 2L) {
    return("")
  }

  paste(lines, collapse = "\n")
}

.cluster_label_brainstorm_disabled_text <- function() {
  paste(
    "Brainstorm was disabled for this run.",
    "Choose and explain the final result from the cluster evidence directly."
  )
}

.append_cluster_label_context_guidance <- function(base_text, extra_guidance_text = NULL) {
  base_text <- .as_scalar_character(base_text)
  extra_guidance_text <- .as_scalar_character(extra_guidance_text)

  pieces <- c(
    if (!is.na(base_text) && nzchar(trimws(base_text))) trimws(base_text),
    if (!is.na(extra_guidance_text) && nzchar(trimws(extra_guidance_text))) {
      trimws(extra_guidance_text)
    }
  )
  pieces <- pieces[nzchar(pieces)]

  if (!length(pieces)) {
    return("")
  }

  paste(pieces, collapse = "\n\n")
}

.compose_cluster_label_selection_context_text <- function(
    draft_analysis_text = NULL,
    candidates = list(),
    use_brainstorm = TRUE,
    extra_guidance_text = NULL
) {
  use_brainstorm <- .arg_single_flag(use_brainstorm, "use_brainstorm")
  candidate_block <- .render_cluster_label_candidate_block_text(candidates)

  if (!isTRUE(use_brainstorm)) {
    pieces <- c(.cluster_label_brainstorm_disabled_text(), candidate_block)
    pieces <- pieces[nzchar(pieces)]
    return(.append_cluster_label_context_guidance(
      paste(pieces, collapse = "\n\n"),
      extra_guidance_text = extra_guidance_text
    ))
  }

  draft_analysis_text <- .as_scalar_character(draft_analysis_text)
  if (is.na(draft_analysis_text) || !nzchar(trimws(draft_analysis_text))) {
    draft_analysis_text <- paste(
      "Brainstorm was enabled, but no usable draft analysis text was recovered.",
      "Proceed from the cluster evidence directly."
    )
  }

  pieces <- c(draft_analysis_text, candidate_block)
  pieces <- pieces[nzchar(pieces)]
  .append_cluster_label_context_guidance(
    paste(pieces, collapse = "\n\n"),
    extra_guidance_text = extra_guidance_text
  )
}

.compose_cluster_label_explanation_context_text <- function(
    draft_analysis_text = NULL,
    use_brainstorm = TRUE,
    extra_guidance_text = NULL
) {
  use_brainstorm <- .arg_single_flag(use_brainstorm, "use_brainstorm")

  if (!isTRUE(use_brainstorm)) {
    return(.append_cluster_label_context_guidance(
      .cluster_label_brainstorm_disabled_text(),
      extra_guidance_text = extra_guidance_text
    ))
  }

  draft_analysis_text <- .as_scalar_character(draft_analysis_text)
  if (is.na(draft_analysis_text) || !nzchar(trimws(draft_analysis_text))) {
    return(.append_cluster_label_context_guidance(
      paste(
        "Brainstorm was enabled, but no usable draft analysis text was recovered.",
        "Explain the fixed selection result from the evidence directly."
      ),
      extra_guidance_text = extra_guidance_text
    ))
  }

  .append_cluster_label_context_guidance(
    draft_analysis_text,
    extra_guidance_text = extra_guidance_text
  )
}

.render_constrained_label_guidance_text <- function(vocabulary) {
  rendered <- .as_scalar_character(vocabulary$rendered)
  if (is.na(rendered) || !nzchar(rendered)) {
    return("")
  }

  paste(
    c(
      "Label-space guidance:",
      "",
      "Constrained label mode is active.",
      "If you choose a label, choose both `canonical_label` and `display_label` from the allowed labels below.",
      "If you abstain, leave the label fields null and fill `abstain_reason`.",
      "Do not invent a different labeled answer outside this list.",
      "",
      rendered
    ),
    collapse = "\n"
  )
}

.render_user_added_data_guidance_text <- function(evidence) {
  has_user_added_data <- FALSE

  direct_block <- evidence$user_added_data %||% NULL
  if (is.list(direct_block)) {
    has_user_added_data <- length(direct_block) > 0L
  } else if (is.character(direct_block)) {
    has_user_added_data <- any(nzchar(trimws(direct_block)))
  }

  meta_flag <- evidence$meta$user_added_data_present %||% NULL
  if (!has_user_added_data && isTRUE(meta_flag)) {
    has_user_added_data <- TRUE
  }

  if (!has_user_added_data) {
    return("")
  }

  paste(
    c(
      "Optional evidence note:",
      "",
      "The evidence bundle includes `user_added_data` supplied by the user.",
      "Use it only if it is explicitly shown in the evidence text.",
      "Treat it as additional raw context, not as pre-normalized or automatically verified data."
    ),
    collapse = "\n"
  )
}

.resolve_cluster_label_mode_context <- function(
    label_mode,
    catalog,
    variant_def,
    dynamic_candidates = NULL
) {
  label_mode <- .arg_cluster_label_mode(label_mode, "label_mode")

  empty_vocabulary <- list(
    path = NULL,
    text = NULL,
    parsed = NULL,
    rendered = ""
  )

  if (identical(label_mode, "open")) {
    return(list(
      requested = "open",
      effective = "open",
      note = NULL,
      guidance_text = "",
      vocabulary = empty_vocabulary,
      dynamic_candidates = list()
    ))
  }

  if (identical(label_mode, "constrained")) {
    vocabulary <- .read_cluster_label_vocabulary(
      catalog = catalog,
      variant_def = variant_def,
      fallback_rel_path = .default_cluster_label_packaged_vocabulary_rel_path()
    )

    return(list(
      requested = "constrained",
      effective = "constrained",
      note = NULL,
      guidance_text = .render_constrained_label_guidance_text(vocabulary),
      vocabulary = vocabulary,
      dynamic_candidates = list()
    ))
  }

  dynamic_candidates <- dynamic_candidates %||% list()
  if (is.character(dynamic_candidates) && length(dynamic_candidates) == 1L) {
    dynamic_candidates <- .extract_cluster_label_candidates_from_draft(
      dynamic_candidates
    )
  }

  if (!is.list(dynamic_candidates) || !length(dynamic_candidates)) {
    return(list(
      requested = "dynamic",
      effective = "open",
      note = paste(
        "Dynamic label mode did not find any usable candidate labels",
        "in the draft analysis, so the prompt fell back to open labeling."
      ),
      guidance_text = "",
      vocabulary = empty_vocabulary,
      dynamic_candidates = list()
    ))
  }

  list(
    requested = "dynamic",
    effective = "dynamic",
    note = NULL,
    guidance_text = .render_dynamic_label_candidates_text(dynamic_candidates),
    vocabulary = empty_vocabulary,
    dynamic_candidates = dynamic_candidates
  )
}

.build_cluster_label_prompt <- function(
    evidence,
    variant,
    schema_path = NULL,
    temperature = NULL,
    top_p = NULL,
    seed = NULL,
    num_predict = NULL,
    prompt_budget_chars = 10000L,
    include_schema = TRUE,
    extra_template_values = NULL,
    label_mode = "open",
    dynamic_candidates = NULL,
    internal_prompt_version = .default_cluster_label_internal_prompt_version()
) {
  catalog <- .read_cluster_label_prompt_catalog()
  catalog_def <- catalog$parsed
  variant_resolution <- .resolve_cluster_label_prompt_variant(catalog_def, variant)
  resolved_variant <- variant_resolution$resolved_variant
  variant_def <- variant_resolution$variant_def
  internal_prompt_version <- .normalize_cluster_label_internal_prompt_version(
    internal_prompt_version
  )

  include_schema <- .arg_single_flag(include_schema, "include_schema")
  extra_template_values <- extra_template_values %||% list()
  extra_template_values <- lapply(extra_template_values, function(x) {
    value <- .as_scalar_character(x)
    if (is.na(value)) "" else value
  })

  schema <- if (isTRUE(include_schema)) {
    .read_cluster_label_schema(schema_path)
  } else {
    list(
      path = NULL,
      text = "",
      parsed = NULL,
      required = character(0)
    )
  }
  prompt_budget_chars <- .arg_nullable_positive_integer(
    prompt_budget_chars,
    "prompt_budget_chars"
  )
  mode_context <- .resolve_cluster_label_mode_context(
    label_mode = label_mode,
    catalog = catalog,
    variant_def = variant_def,
    dynamic_candidates = dynamic_candidates
  )
  vocabulary <- mode_context$vocabulary
  system_prompt_path <- .cluster_label_prompt_asset_path(
    catalog,
    catalog_def$system_prompt_path,
    internal_prompt_version = internal_prompt_version
  )
  user_prompt_path <- .cluster_label_prompt_asset_path(
    catalog,
    variant_def$user_prompt_path,
    internal_prompt_version = internal_prompt_version
  )
  system_template <- .read_text_file(system_prompt_path)
  user_template <- .read_text_file(user_prompt_path)
  schema_prompt_text <- if (isTRUE(include_schema)) {
    .render_cluster_label_schema_prompt_text(schema)
  } else {
    ""
  }
  user_added_data_guidance_text <- .render_user_added_data_guidance_text(evidence)

  fixed_template_values <- c(
    list(
      "{{CLUSTER_ID}}" = evidence$meta$cluster_id,
      "{{OUTPUT_SCHEMA_JSON}}" = schema_prompt_text,
      "{{COARSE_LABEL_VOCABULARY_TEXT}}" = vocabulary$rendered %||% "",
      "{{LABEL_MODE_GUIDANCE_TEXT}}" = mode_context$guidance_text %||% "",
      "{{USER_ADDED_DATA_GUIDANCE_TEXT}}" = user_added_data_guidance_text
    ),
    extra_template_values
  )

  empty_template_values <- c(
    fixed_template_values,
    list(
      "{{CLUSTER_EVIDENCE_TEXT}}" = ""
    )
  )

  system_content_empty <- .interpolate_prompt_template(
    system_template,
    empty_template_values
  )
  user_content_empty <- .interpolate_prompt_template(
    user_template,
    empty_template_values
  )
  fixed_overhead_chars <- .cluster_evidence_prompt_char_count(system_content_empty) +
    .cluster_evidence_prompt_char_count(user_content_empty)
  evidence_budget_chars <- if (is.null(prompt_budget_chars)) {
    NULL
  } else {
    as.integer(max(prompt_budget_chars - fixed_overhead_chars, 0L))
  }
  evidence_render <- .serialize_cluster_evidence_llm_prompt(
    evidence,
    max_chars = evidence_budget_chars
  )
  evidence_text <- evidence_render$text

  template_values <- c(
    fixed_template_values,
    list(
      "{{CLUSTER_EVIDENCE_TEXT}}" = evidence_text
    )
  )

  system_content <- .interpolate_prompt_template(
    system_template,
    template_values
  )
  user_content <- .interpolate_prompt_template(
    user_template,
    template_values
  )
  total_prompt_chars <- .cluster_evidence_prompt_char_count(system_content) +
    .cluster_evidence_prompt_char_count(user_content)

  generation <- catalog_def$default_generation
  generation$temperature <- temperature %||% variant_def$temperature %||% generation$temperature
  generation$top_p <- top_p %||% variant_def$top_p %||% generation$top_p
  generation$seed <- as.integer(seed %||% generation$seed)
  generation$num_predict <- as.integer(num_predict %||% generation$num_predict)

  list(
    cluster_id = evidence$meta$cluster_id,
    variant = variant,
    resolved_variant = resolved_variant,
    is_legacy_alias = variant_resolution$is_legacy_alias,
    task_type = variant_def$task_type %||% "label",
    catalog_path = catalog$path,
    schema_path = schema$path,
    schema_text = schema$text,
    schema_required = schema$required,
    schema_object = schema$parsed,
    schema_prompt_text = schema_prompt_text,
    extra_template_values = extra_template_values,
    evidence_text = evidence_text,
    evidence_text_full = evidence_render$full_text,
    evidence_budget = list(
      prompt_budget_chars = prompt_budget_chars,
      fixed_overhead_chars = fixed_overhead_chars,
      schema_prompt_chars = .cluster_evidence_prompt_char_count(schema_prompt_text),
      schema_text_chars = .cluster_evidence_prompt_char_count(schema$text),
      evidence_budget_chars = evidence_budget_chars,
      evidence_chars_full = evidence_render$chars_full,
      evidence_chars_used = evidence_render$chars_used,
      total_prompt_chars = total_prompt_chars,
      fits_within_budget = if (is.null(prompt_budget_chars)) {
        TRUE
      } else {
        total_prompt_chars <= prompt_budget_chars
      },
      fixed_overhead_exceeds_budget = if (is.null(prompt_budget_chars)) {
        FALSE
      } else {
        fixed_overhead_chars > prompt_budget_chars
      },
      trimmed = isTRUE(evidence_render$trimmed),
      kept_block_ids = evidence_render$kept_block_ids,
      dropped_block_ids = evidence_render$dropped_block_ids,
      truncated_block_ids = evidence_render$truncated_block_ids,
      blocks = evidence_render$blocks
    ),
    label_mode_requested = mode_context$requested,
    label_mode_effective = mode_context$effective,
    label_mode_note = mode_context$note,
    label_mode_guidance_text = mode_context$guidance_text,
    dynamic_candidates = mode_context$dynamic_candidates,
    vocabulary_path = vocabulary$path,
    vocabulary_text = vocabulary$rendered,
    vocabulary_object = vocabulary$parsed,
    internal_prompt_version = internal_prompt_version,
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
      format = prompt_bundle$schema_object %||% NULL,
      options = options,
      keep_alive = keep_alive
    )
  )
}

.default_cluster_label_gate_variant <- function() {
  "gate_abstain_examples_v1"
}

.default_cluster_label_draft_variant <- function() {
  catalog <- tryCatch(
    .read_cluster_label_prompt_catalog(),
    error = function(e) NULL
  )

  draft_variant <- catalog$parsed$internal_draft_variant %||% NULL
  if (.is_non_empty_scalar_character(draft_variant)) {
    return(draft_variant)
  }

  "draft_analysis_v1"
}

.default_cluster_label_explanation_variant <- function() {
  catalog <- tryCatch(
    .read_cluster_label_prompt_catalog(),
    error = function(e) NULL
  )

  explanation_variant <- catalog$parsed$internal_explanation_variant %||% NULL
  if (.is_non_empty_scalar_character(explanation_variant)) {
    return(explanation_variant)
  }

  "explanation_pass_v1"
}

.default_cluster_label_v2_label_summary_variant <- function() {
  catalog <- tryCatch(
    .read_cluster_label_prompt_catalog(),
    error = function(e) NULL
  )

  summary_variant <- catalog$parsed$internal_v2_label_summary_variant %||% NULL
  if (.is_non_empty_scalar_character(summary_variant)) {
    return(summary_variant)
  }

  "label_summary_pass_v2"
}

.default_cluster_label_v2_abstain_reason_variant <- function() {
  catalog <- tryCatch(
    .read_cluster_label_prompt_catalog(),
    error = function(e) NULL
  )

  abstain_reason_variant <- catalog$parsed$internal_v2_abstain_reason_variant %||% NULL
  if (.is_non_empty_scalar_character(abstain_reason_variant)) {
    return(abstain_reason_variant)
  }

  "abstain_reason_pass_v2"
}

.default_cluster_label_post_label_category_variant <- function() {
  catalog <- tryCatch(
    .read_cluster_label_prompt_catalog(),
    error = function(e) NULL
  )

  variant <- catalog$parsed$internal_post_label_category_variant %||% NULL
  if (.is_non_empty_scalar_character(variant)) {
    return(variant)
  }

  "post_label_category_v1"
}

.default_cluster_label_post_label_uniqueness_variant <- function() {
  catalog <- tryCatch(
    .read_cluster_label_prompt_catalog(),
    error = function(e) NULL
  )

  variant <- catalog$parsed$internal_post_label_uniqueness_variant %||% NULL
  if (.is_non_empty_scalar_character(variant)) {
    return(variant)
  }

  "post_label_uniqueness_v1"
}

.cluster_label_selection_cascade_variants <- function(variant) {
  catalog <- .read_cluster_label_prompt_catalog()
  catalog_def <- catalog$parsed
  resolved <- .resolve_cluster_label_prompt_variant(
    catalog_def,
    variant
  )$resolved_variant
  public_variants <- .as_character_vector(
    catalog_def$public_label_variants %||%
      c("label_primary_v1", "label_soft_v1", "label_broad_v1")
  )

  start_idx <- match(resolved, public_variants)
  if (is.na(start_idx)) {
    stop(
      "workflow_steps = 3 requires a public label variant. Received `",
      variant,
      "` resolved to `",
      resolved,
      "`."
    )
  }

  cascade_public <- public_variants[start_idx:length(public_variants)]
  cascade_internal <- vapply(cascade_public, function(public_variant) {
    selection_variant <- .as_scalar_character(
      catalog_def$variants[[public_variant]]$selection_variant %||% NA_character_
    )
    if (is.na(selection_variant) || !nzchar(selection_variant)) {
      stop(
        "Public variant `",
        public_variant,
        "` is missing `selection_variant` in the prompt catalog."
      )
    }
    selection_variant
  }, character(1))

  list(
    public_variants = unname(cascade_public),
    internal_variants = unname(cascade_internal)
  )
}

.cluster_label_decision_cascade_variants <- function(variant) {
  catalog <- .read_cluster_label_prompt_catalog()
  catalog_def <- catalog$parsed
  resolved <- .resolve_cluster_label_prompt_variant(
    catalog_def,
    variant
  )$resolved_variant
  public_variants <- .as_character_vector(
    catalog_def$public_label_variants %||%
      c("label_primary_v1", "label_soft_v1", "label_broad_v1")
  )

  start_idx <- match(resolved, public_variants)
  if (is.na(start_idx)) {
    stop(
      "workflow_steps = 3 requires a public label variant. Received `",
      variant,
      "` resolved to `",
      resolved,
      "`."
    )
  }

  decision_map <- catalog_def$internal_v2_label_decision_variants %||% list()
  cascade_public <- public_variants[start_idx:length(public_variants)]
  cascade_internal <- vapply(cascade_public, function(public_variant) {
    decision_variant <- .as_scalar_character(
      decision_map[[public_variant]] %||% NA_character_
    )
    if (is.na(decision_variant) || !nzchar(decision_variant)) {
      stop(
        "Public variant `",
        public_variant,
        "` is missing a v2 decision-variant mapping in the prompt catalog."
      )
    }
    decision_variant
  }, character(1))

  list(
    public_variants = unname(cascade_public),
    internal_variants = unname(cascade_internal)
  )
}

.cluster_label_decision_shortening_repair_prompt <- function(
    variant,
    internal_prompt_version = .default_cluster_label_internal_prompt_version(),
    long_label = NULL,
    label_description = NULL
) {
  catalog <- .read_cluster_label_prompt_catalog()
  catalog_def <- catalog$parsed
  resolved <- .resolve_cluster_label_prompt_variant(
    catalog_def,
    variant
  )$resolved_variant
  repair_map <- catalog_def$internal_v2_label_decision_shortening_repair_variants %||% list()
  repair_variant <- .as_scalar_character(repair_map[[resolved]] %||% NA_character_)

  if (is.na(repair_variant) || !nzchar(repair_variant)) {
    return(list(
      public_variant = resolved,
      variant = NULL,
      text = NULL,
      path = NULL,
      available = FALSE
    ))
  }

  variant_def <- catalog_def$variants[[repair_variant]] %||% NULL
  if (is.null(variant_def)) {
    stop(
      "Shortening repair variant `",
      repair_variant,
      "` is missing from the prompt catalog."
    )
  }

  prompt_path <- tryCatch(
    .cluster_label_prompt_asset_path(
      catalog,
      variant_def$user_prompt_path,
      internal_prompt_version = internal_prompt_version
    ),
    error = function(e) NULL
  )

  if (is.null(prompt_path) || !nzchar(prompt_path) || !file.exists(prompt_path)) {
    return(list(
      public_variant = resolved,
      variant = repair_variant,
      text = NULL,
      path = NULL,
      available = FALSE
    ))
  }

  prompt_text <- trimws(.interpolate_prompt_template(
    .read_text_file(prompt_path),
    list(
      "{{LONG_LABEL}}" = .null_default(.as_scalar_character(long_label), ""),
      "{{LABEL_DESCRIPTION}}" = .null_default(.as_scalar_character(label_description), "")
    )
  ))
  if (!nzchar(prompt_text)) {
    stop(
      "Shortening repair prompt `",
      repair_variant,
      "` is empty."
    )
  }

  list(
    public_variant = resolved,
    variant = repair_variant,
    text = prompt_text,
    path = prompt_path,
    available = TRUE
  )
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

  catalog <- tryCatch(
    .read_cluster_label_prompt_catalog(),
    error = function(e) NULL
  )

  if (!is.null(catalog)) {
    ladder <- .as_character_vector(
      catalog$parsed$public_speculative_ladder %||% character(0)
    )
    if (length(ladder)) {
      return(unname(ladder))
    }
  }

  c("label_soft_v1", "label_broad_v1")
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
