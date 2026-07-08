# Internal utilities shared across the cluster-labeling workflow.
#
# The package uses a functional style rather than Python-like classes, so the
# R-idiomatic way to reduce duplication here is a small layer of internal
# helpers that can be reused by evidence extraction, LLM calls, validation,
# and review rendering.

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.arg_scalar_character <- function(x, name) {
  if (missing(x) || is.null(x) || length(x) != 1L || !is.character(x) || !nzchar(x)) {
    stop("`", name, "` must be a non-empty character scalar.")
  }
  x
}

.arg_non_negative_integer <- function(x, name) {
  if (length(x) != 1L || !is.numeric(x) || !is.finite(x) || x < 0) {
    stop("`", name, "` must be a single non-negative integer.")
  }
  as.integer(x)
}

.arg_nullable_non_negative_integer <- function(x, name) {
  if (is.null(x)) {
    return(NULL)
  }

  .arg_non_negative_integer(x, name)
}

.arg_positive_integer <- function(x, name) {
  x <- .arg_non_negative_integer(x, name)
  if (x < 1L) {
    stop("`", name, "` must be a single integer >= 1.")
  }
  x
}

.arg_nullable_positive_integer <- function(x, name) {
  if (is.null(x)) {
    return(NULL)
  }

  .arg_positive_integer(x, name)
}

.arg_workflow_steps <- function(x, name) {
  x <- .arg_non_negative_integer(x, name)
  if (!x %in% c(1L, 2L, 3L)) {
    stop("`", name, "` must be either 1, 2, or 3.")
  }
  x
}

.cluster_label_fixed_workflow_steps <- function() {
  3L
}

.normalize_cluster_label_workflow_steps <- function(x, name) {
  x <- .arg_workflow_steps(x, name)
  fixed_steps <- .cluster_label_fixed_workflow_steps()

  if (!identical(x, fixed_steps)) {
    warning(
      "`",
      name,
      "` = ",
      x,
      " is deprecated. The active cluster-label pipeline now always uses the fixed three-stage route, so this value is ignored and treated as ",
      fixed_steps,
      ".",
      call. = FALSE
    )
  }

  fixed_steps
}

.arg_cluster_label_mode <- function(x, name) {
  x <- .arg_scalar_character(x, name)
  if (!x %in% c("open", "constrained", "dynamic")) {
    stop(
      "`",
      name,
      "` must be one of: \"open\", \"constrained\", or \"dynamic\"."
    )
  }
  x
}

.normalize_cluster_label_mode <- function(x, name) {
  x <- .arg_cluster_label_mode(x, name)

  if (identical(x, "dynamic")) {
    warning(
      "`",
      name,
      "` = \"dynamic\" is deprecated. Draft-derived candidate labels are now injected into selection prompts automatically, so the active pipeline treats this as `\"open\"`.",
      call. = FALSE
    )
    return("open")
  }

  x
}

.arg_single_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", name, "` must be TRUE or FALSE.")
  }
  x
}

.arg_nullable_scalar_character <- function(x, name) {
  if (is.null(x)) {
    return(NULL)
  }
  .arg_scalar_character(x, name)
}

.arg_named_list_or_null <- function(x, name) {
  if (is.null(x)) {
    return(NULL)
  }

  if (!is.list(x)) {
    stop("`", name, "` must be NULL or a named list.")
  }

  nms <- names(x)
  if (is.null(nms) || !length(x) || any(is.na(nms)) || any(!nzchar(nms))) {
    stop("`", name, "` must be a named list with non-empty names.")
  }

  x
}

.extract_cluster_label_output <- function(x) {
  if (inherits(x, "cluster_label_result")) {
    return(x$output)
  }
  x
}

.cluster_label_public_fields <- function(output, provenance = NULL) {
  output <- output %||% list()
  provenance <- provenance %||% list()

  status <- .as_scalar_character(output$status)
  canonical_label <- .as_scalar_character(output$canonical_label)
  display_label <- .as_scalar_character(output$display_label)
  selected_label_variant <- .as_scalar_character(
    provenance$selected_label_variant %||% provenance$selected_public_variant
  )
  label_stage_exhausted <- isTRUE(
    provenance$label_stage_exhausted %||% provenance$exhausted
  )

  if (identical(status, "labeled") &&
      .is_non_empty_scalar_character(canonical_label) &&
      .is_non_empty_scalar_character(display_label)) {
    return(list(
      public_canonical_label = canonical_label,
      public_display_label = display_label,
      public_label_source = "model_output"
    ))
  }

  if (identical(status, "abstain") &&
      (identical(selected_label_variant, "selection_all_abstain") ||
        isTRUE(label_stage_exhausted))) {
    return(list(
      public_canonical_label = "chaotic_cluster",
      public_display_label = "Chaotic Cluster",
      public_label_source = "post_abstain_fallback"
    ))
  }

  list(
    public_canonical_label = NULL,
    public_display_label = NULL,
    public_label_source = NULL
  )
}

.cluster_label_output_summary_text <- function(output) {
  output <- output %||% list()

  for (field in c(
    "label_summary",
    "interpretation_summary",
    "abstain_reason",
    "explanation"
  )) {
    text <- .as_scalar_character(output[[field]])
    if (.is_non_empty_scalar_character(text)) {
      return(text)
    }
  }

  NA_character_
}

.cluster_label_output_explanation_text <- function(output) {
  output <- output %||% list()

  for (field in c(
    "explanation",
    "interpretation_summary",
    "label_summary",
    "abstain_reason"
  )) {
    text <- .as_scalar_character(output[[field]])
    if (.is_non_empty_scalar_character(text)) {
      return(text)
    }
  }

  NA_character_
}

.new_cluster_label_issue_table <- function() {
  data.frame(
    severity = character(),
    category = character(),
    code = character(),
    message = character(),
    location = character(),
    stringsAsFactors = FALSE
  )
}

.as_scalar_character <- function(x) {
  if (is.list(x) && length(x) == 1L) {
    return(.as_scalar_character(x[[1L]]))
  }
  if (is.null(x) || length(x) != 1L) {
    return(NA_character_)
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (!is.character(x)) {
    return(NA_character_)
  }
  x
}

.as_scalar_numeric <- function(x) {
  if (is.list(x) && length(x) == 1L) {
    return(.as_scalar_numeric(x[[1L]]))
  }
  if (is.null(x) || length(x) != 1L) {
    return(NA_real_)
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  out <- suppressWarnings(as.numeric(x))
  if (length(out) != 1L) {
    return(NA_real_)
  }
  out
}

.as_character_vector <- function(x) {
  if (is.null(x)) {
    return(character(0))
  }
  if (is.list(x)) {
    x <- unlist(x, recursive = TRUE, use.names = FALSE)
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (!is.character(x)) {
    return(character(0))
  }
  x <- trimws(x)
  unique(unname(x[!is.na(x) & nzchar(x)]))
}

.is_non_empty_scalar_character <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
}

.null_default <- function(x, default) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) {
    return(default)
  }
  x
}

.cluster_evidence_cluster_id <- function(evidence) {
  meta <- evidence$meta
  if (!is.list(meta) || is.null(meta$cluster_id)) {
    return(NA_character_)
  }
  .as_scalar_character(meta$cluster_id)
}

.is_absolute_output_path <- function(path) {
  path <- .as_scalar_character(path)
  if (is.na(path) || !nzchar(path)) {
    return(FALSE)
  }

  grepl("^([A-Za-z]:[/\\\\]|[/\\\\]{2}|/)", path, perl = TRUE)
}

.cocktailr_source_root_candidates <- function() {
  wd <- tryCatch(getwd(), error = function(e) "")
  ancestors <- character(0)

  if (is.character(wd) && nzchar(wd)) {
    current <- normalizePath(wd, winslash = "/", mustWork = FALSE)
    repeat {
      ancestors <- c(ancestors, current)
      parent <- dirname(current)
      if (!nzchar(parent) || identical(parent, current)) {
        break
      }
      current <- parent
    }
  }

  ns_path <- tryCatch(
    getNamespaceInfo(asNamespace("cocktailr"), "path"),
    error = function(e) ""
  )

  unique(c(
    ancestors,
    if (is.character(wd) && nzchar(wd)) file.path(wd, "cocktailr") else character(0),
    ns_path
  ))
}

.looks_like_cocktailr_source_root <- function(path) {
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    return(FALSE)
  }

  desc <- file.path(path, "DESCRIPTION")
  if (!file.exists(desc) ||
      !dir.exists(file.path(path, "R")) ||
      !dir.exists(file.path(path, "man"))) {
    return(FALSE)
  }

  desc_lines <- tryCatch(
    readLines(desc, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character(0)
  )

  any(grepl("^Package:\\s*cocktailr\\s*$", desc_lines, ignore.case = TRUE))
}

.cocktailr_source_root <- function() {
  candidates <- .cocktailr_source_root_candidates()
  matches <- vapply(candidates, .looks_like_cocktailr_source_root, logical(1))

  if (!any(matches)) {
    return(NULL)
  }

  normalizePath(candidates[which(matches)[1L]], winslash = "/", mustWork = FALSE)
}

# Resolve project-relative output paths against the local package root when
# the code is run from a source checkout. This keeps generated artifacts in the
# package workspace even if the user started R one directory higher.
.resolve_cocktailr_output_path <- function(path) {
  path <- .as_scalar_character(path)
  if (is.na(path) || !nzchar(path) || .is_absolute_output_path(path)) {
    return(path)
  }

  root <- .cocktailr_source_root()
  if (is.null(root) || !nzchar(root)) {
    return(path)
  }

  file.path(root, path)
}

.cluster_label_default_debug_log_dir <- function() {
  file.path("temp", "reports", "cluster_label_debug")
}

.cluster_label_effective_log_dir <- function(debug, log_dir = NULL) {
  debug <- .arg_single_flag(debug, "debug")

  if (!isTRUE(debug)) {
    return(NULL)
  }

  if (is.null(log_dir)) {
    return(.cluster_label_default_debug_log_dir())
  }

  .arg_scalar_character(log_dir, "log_dir")
}
