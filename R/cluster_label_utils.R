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

.arg_positive_integer <- function(x, name) {
  x <- .arg_non_negative_integer(x, name)
  if (x < 1L) {
    stop("`", name, "` must be a single integer >= 1.")
  }
  x
}

.arg_workflow_steps <- function(x, name) {
  x <- .arg_non_negative_integer(x, name)
  if (!x %in% c(1L, 2L)) {
    stop("`", name, "` must be either 1 or 2.")
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

.extract_cluster_label_output <- function(x) {
  if (inherits(x, "cluster_label_result")) {
    return(x$output)
  }
  x
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
