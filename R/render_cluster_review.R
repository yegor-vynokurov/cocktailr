#' Render a human-reviewable cluster labeling card
#'
#' Builds a markdown review card from a \code{\link{cluster_evidence}} object,
#' one structured cluster-label output, and a validation result from
#' \code{\link{validate_cluster_label}}.
#'
#' The review card is designed for expert inspection:
#' \itemize{
#'   \item it summarizes the proposed label or abstention,
#'   \item shows evidence-backed claims and cited species,
#'   \item surfaces validator warnings and unsupported-claim flags,
#'   \item leaves a manual notes section for human review.
#' }
#'
#' When \code{file} or \code{review_dir} is supplied, the function can write a
#' markdown artifact to disk. The default card is intentionally compact; set
#' \code{full = TRUE} to include the full review bundle and to save the JSON
#' sidecar metadata by default.
#'
#' @param x A parsed cluster label output list, or an object returned by
#'   \code{\link{llm_label_cluster}}.
#' @param evidence A \code{"cluster_evidence"} object produced by
#'   \code{\link{cluster_evidence}}.
#' @param validation Optional \code{"cluster_label_validation"} object. If
#'   \code{NULL}, validation is computed automatically with
#'   \code{\link{validate_cluster_label}}.
#' @param file Optional path to a markdown output file. When a relative path is
#'   used and a local \code{cocktailr} source checkout can be detected, the
#'   path is resolved against that package root.
#' @param review_dir Optional root directory for automatically generated review
#'   files. When supplied, the function creates a dataset-aware subdirectory and
#'   saves \code{"<cluster_id>_review.md"} there. When a relative path is used
#'   and a local \code{cocktailr} source checkout can be detected, it is
#'   resolved against that package root.
#' @param metadata_file Optional path to a sidecar JSON metadata file. When
#'   omitted and \code{file} is provided, the path is derived automatically.
#'   Relative paths follow the same resolution rule as \code{file}.
#' @param full Logical; if \code{FALSE} (default), render a compact final card.
#'   If \code{TRUE}, include review summary, external knowledge, validator
#'   warnings, checks, evidence snapshot, evidence limitations, and manual notes.
#' @param include_front_matter Logical; if \code{TRUE} (default when
#'   \code{full = TRUE}), include YAML front matter with machine-readable
#'   status fields.
#' @param write_metadata Logical; if \code{TRUE} (default when
#'   \code{full = TRUE}), write the metadata JSON sidecar whenever
#'   \code{file}, \code{review_dir}, or \code{metadata_file} is provided.
#' @param manual_notes Optional character vector for the manual review section.
#'   If \code{NULL}, a placeholder checklist is inserted.
#' @param title Optional markdown title. Defaults to
#'   \code{"Cluster Review: <cluster_id>"}.
#'
#' @return An object of class \code{"cluster_review_artifact"} with components
#'   \code{markdown}, \code{lines}, \code{metadata}, \code{file}, and
#'   \code{metadata_file}.
#'
#' @examples
#' syn <- generate_synthetic_vegetation_data(
#'   n_plots_per_community = 4,
#'   n_transition_plots = 2,
#'   seed = 42
#' )
#' res <- cocktail_cluster(
#'   vegmatrix = syn$wide_matrix,
#'   progress = FALSE,
#'   plot_values = "rel_cover",
#'   species_cluster_phi = TRUE,
#'   save_vegmatrix = TRUE
#' )
#' ev <- cluster_evidence(res, cluster = "c_1", top_n_phi = 3)
#' topo <- ev$summaries$species_topological
#'
#' out <- list(
#'   schema_version = "0.1.0",
#'   cluster_id = ev$meta$cluster_id,
#'   status = "abstain",
#'   canonical_label = NULL,
#'   display_label = NULL,
#'   interpretation_summary = paste(
#'     "The cluster has some recurring species, but the evidence",
#'     "is not distinctive enough for a stable ecological label."
#'   ),
#'   basis_in_data = list(
#'     list(
#'       claim_id = "C1",
#'       statement = "A recurring species core is present, but not decisive.",
#'       evidence_ids = c(topo$evidence_id[[1]]),
#'       support_strength = "weak"
#'     )
#'   ),
#'   key_species = list(
#'     list(
#'       species = topo$species[[1]],
#'       role = "topological",
#'       evidence_ids = c(topo$evidence_id[[1]])
#'     )
#'   ),
#'   external_knowledge = list(),
#'   not_confirmed_by_data = list(
#'     list(
#'       statement = "A habitat-level label cannot be confirmed.",
#'       reason = "The evidence bundle does not contain direct habitat metadata."
#'     )
#'   ),
#'   confidence = list(
#'     score = 0.35,
#'     rationale = "The cluster has some structure, but not enough for a stable label."
#'   ),
#'   checks_to_run = list(
#'     list(
#'       check = "Compare with sibling clusters.",
#'       priority = "high",
#'       reason = "Contrastive evidence would help decide whether abstention should be lifted."
#'     )
#'   ),
#'   abstain_reason = "Distinctiveness is insufficient for a stable label."
#' )
#'
#' val <- validate_cluster_label(out, ev)
#' review <- render_cluster_review(out, ev, validation = val)
#' cat(review$markdown)
#'
#' @export
render_cluster_review <- function(
    x,
    evidence,
    validation = NULL,
    file = NULL,
    review_dir = NULL,
    metadata_file = NULL,
    full = FALSE,
    include_front_matter = full,
    write_metadata = full,
    manual_notes = NULL,
    title = NULL
) {
  if (!inherits(evidence, "cluster_evidence")) {
    stop("`evidence` must be a `cluster_evidence` object.", call. = FALSE)
  }
  if (!is.logical(full) || length(full) != 1L || is.na(full)) {
    stop("`full` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(include_front_matter) ||
      length(include_front_matter) != 1L ||
      is.na(include_front_matter)) {
    stop("`include_front_matter` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(write_metadata) ||
      length(write_metadata) != 1L ||
      is.na(write_metadata)) {
    stop("`write_metadata` must be TRUE or FALSE.", call. = FALSE)
  }

  output <- .extract_cluster_label_output(x)
  if (!is.list(output) || is.null(names(output))) {
    stop("`x` must be a named list or a `cluster_label_result` object.",
      call. = FALSE
    )
  }

  if (is.null(validation)) {
    validation <- validate_cluster_label(x, evidence)
  } else if (!inherits(validation, "cluster_label_validation")) {
    stop("`validation` must be a `cluster_label_validation` object.",
      call. = FALSE
    )
  }

  cluster_id <- .null_default(.cluster_evidence_cluster_id(evidence), output$cluster_id)
  if (is.null(title)) {
    title <- paste("Cluster Review:", cluster_id)
  }

  metadata <- .build_cluster_review_metadata(
    x = x,
    evidence = evidence,
    validation = validation,
    title = title,
    full = full
  )

  lines <- .render_cluster_review_lines(
    output = output,
    evidence = evidence,
    validation = validation,
    metadata = metadata,
    full = full,
    include_front_matter = include_front_matter,
    manual_notes = manual_notes,
    title = title
  )

  markdown <- paste(lines, collapse = "\n")

  resolved_paths <- .resolve_cluster_review_paths(
    evidence = evidence,
    file = file,
    review_dir = review_dir,
    metadata_file = metadata_file
  )

  file_out <- NULL
  if (!is.null(resolved_paths$file)) {
    dir.create(dirname(resolved_paths$file), recursive = TRUE, showWarnings = FALSE)
    writeLines(lines, con = resolved_paths$file, useBytes = TRUE)
    file_out <- normalizePath(resolved_paths$file, winslash = "/", mustWork = TRUE)
  }

  metadata_out <- NULL
  if (isTRUE(write_metadata) && !is.null(resolved_paths$metadata_file)) {
    dir.create(dirname(resolved_paths$metadata_file), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(
      metadata,
      path = resolved_paths$metadata_file,
      auto_unbox = TRUE,
      pretty = TRUE,
      null = "null"
    )
    metadata_out <- normalizePath(resolved_paths$metadata_file, winslash = "/", mustWork = TRUE)
  }

  out <- list(
    markdown = markdown,
    lines = lines,
    metadata = metadata,
    file = file_out,
    metadata_file = metadata_out
  )
  class(out) <- c("cluster_review_artifact", "list")
  out
}

#' @export
print.cluster_review_artifact <- function(x, ...) {
  cat(x$markdown, "\n", sep = "")
  invisible(x)
}

.build_cluster_review_metadata <- function(x, evidence, validation, title, full) {
  output <- validation$output %||% .extract_cluster_label_output(x)
  issues <- validation$issues %||% .new_cluster_label_issue_table()
  provenance <- .cluster_review_provenance(x)
  coverage <- validation$evidence_coverage %||% list(
    score = NA_real_,
    n_valid_citations = NA_integer_,
    n_citation_targets = NA_integer_
  )
  dataset <- .cluster_review_dataset_info(evidence)

  list(
    review_card_version = "0.1.0",
    rendered_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    title = title,
    full_card = isTRUE(full),
    cluster_id = .null_default(validation$cluster_id, .cluster_evidence_cluster_id(evidence)),
    output_status = .null_default(validation$output_status, .as_scalar_character(output$status)),
    validation_status = validation$validation_status %||% NA_character_,
    review_status = .cluster_review_status(validation),
    needs_human_review = isTRUE(validation$needs_human_review),
    is_valid = isTRUE(validation$is_valid),
    evidence_coverage_score = coverage$score %||% NA_real_,
    evidence_citation_valid = coverage$n_valid_citations %||% NA_integer_,
    evidence_citation_total = coverage$n_citation_targets %||% NA_integer_,
    issue_count = nrow(issues),
    warning_count = sum(issues$severity == "warning"),
    error_count = sum(issues$severity == "error"),
    unsupported_claim_issue_count = sum(issues$category == "unsupported_claims"),
    proposed_canonical_label = .as_scalar_character(output$canonical_label),
    proposed_display_label = .as_scalar_character(output$display_label),
    provider = provenance$provider,
    model = provenance$model,
    variant = provenance$variant,
    workflow_steps = provenance$workflow_steps,
    gate_variant = provenance$gate_variant,
    gate_decision = provenance$gate_decision,
    log_run_dir = provenance$log_run_dir,
    source_class = provenance$source_class,
    prompt_catalog_path = provenance$prompt_catalog_path,
    prompt_schema_path = provenance$prompt_schema_path,
    prompt_system_path = provenance$prompt_system_path,
    prompt_user_path = provenance$prompt_user_path,
    gate_prompt_system_path = provenance$gate_prompt_system_path,
    gate_prompt_user_path = provenance$gate_prompt_user_path,
    dataset_type = dataset$type,
    dataset_label = dataset$label,
    dataset_path = dataset$path,
    dataset_folder = dataset$folder_slug
  )
}

.cluster_review_provenance <- function(x) {
  if (!inherits(x, "cluster_label_result")) {
    return(list(
      provider = NULL,
      model = NULL,
      variant = NULL,
      workflow_steps = NULL,
      gate_variant = NULL,
      gate_decision = NULL,
      log_run_dir = NULL,
      prompt_catalog_path = NULL,
      prompt_schema_path = NULL,
      prompt_system_path = NULL,
      prompt_user_path = NULL,
      gate_prompt_system_path = NULL,
      gate_prompt_user_path = NULL,
      source_class = class(x)[1L] %||% "list"
    ))
  }

  workflow <- x$workflow %||% list()
  gate_stage <- if (is.list(workflow)) workflow$gate else NULL
  gate_output <- if (is.list(gate_stage)) gate_stage$output %||% NULL else NULL
  main_prompt <- x$prompt %||% list()
  gate_prompt <- if (is.list(gate_stage)) gate_stage$prompt %||% list() else list()

  list(
    provider = .null_default(x$provider, NULL),
    model = .null_default(x$model, NULL),
    variant = .null_default(x$variant, NULL),
    workflow_steps = .null_default(x$workflow_steps, NULL),
    gate_variant = if (is.list(gate_stage)) gate_stage$variant %||% NULL else NULL,
    gate_decision = gate_output$decision %||% NULL,
    log_run_dir = x$logs$run_dir %||% NULL,
    prompt_catalog_path = main_prompt$catalog_path %||% NULL,
    prompt_schema_path = main_prompt$schema_path %||% NULL,
    prompt_system_path = main_prompt$system_path %||% NULL,
    prompt_user_path = main_prompt$user_path %||% NULL,
    gate_prompt_system_path = gate_prompt$system_path %||% NULL,
    gate_prompt_user_path = gate_prompt$user_path %||% NULL,
    source_class = "cluster_label_result"
  )
}

.cluster_review_status <- function(validation) {
  if (identical(validation$validation_status, "schema_error")) {
    return("blocked")
  }
  if (isTRUE(validation$needs_human_review)) {
    return("review_required")
  }
  if (identical(validation$output_status, "abstain")) {
    return("abstained")
  }
  "accepted"
}

.cluster_review_dataset_info <- function(evidence) {
  info <- evidence$meta$dataset %||% list()
  if (!is.list(info)) {
    info <- list()
  }

  path <- .as_scalar_character(info$path)
  label <- .as_scalar_character(info$label)
  type <- .as_scalar_character(info$type)

  if (is.na(label) && !is.na(path) && nzchar(path)) {
    label <- tools::file_path_sans_ext(basename(path))
  }

  folder_slug <- if (!is.na(label) && nzchar(label)) {
    .sanitize_review_path_component(label)
  } else if (!is.na(path) && nzchar(path)) {
    .sanitize_review_path_component(tools::file_path_sans_ext(basename(path)))
  } else {
    NA_character_
  }

  list(
    type = if (!is.na(type) && nzchar(type)) type else NA_character_,
    label = if (!is.na(label) && nzchar(label)) label else NA_character_,
    path = if (!is.na(path) && nzchar(path)) normalizePath(path, winslash = "/", mustWork = FALSE) else NA_character_,
    folder_slug = folder_slug
  )
}

.sanitize_review_path_component <- function(x) {
  x <- .as_scalar_character(x)
  if (is.na(x) || !nzchar(x)) {
    return(NA_character_)
  }

  x <- gsub("[/\\\\:*?\"<>|]+", "_", x)
  x <- gsub("[[:space:]]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("(^[._-]+|[._-]+$)", "", x)
  x <- gsub("[^A-Za-z0-9._-]", "_", x)

  if (!nzchar(x)) {
    return(NA_character_)
  }
  x
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

.resolve_review_output_path <- function(path) {
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

.resolve_cluster_review_paths <- function(evidence, file, review_dir, metadata_file) {
  if (!is.null(file) && !is.null(review_dir)) {
    stop("Use either `file` or `review_dir`, not both.", call. = FALSE)
  }

  file_target <- if (!is.null(file)) .resolve_review_output_path(file) else NULL
  if (is.null(file_target) && !is.null(review_dir)) {
    file_target <- .default_cluster_review_file(
      evidence = evidence,
      root_dir = .resolve_review_output_path(review_dir)
    )
  }

  metadata_target <- if (!is.null(metadata_file)) {
    .resolve_review_output_path(metadata_file)
  } else {
    NULL
  }
  if (is.null(metadata_target) && !is.null(file_target)) {
    metadata_target <- .default_cluster_review_metadata_file(file_target)
  }

  list(
    file = file_target,
    metadata_file = metadata_target
  )
}

.default_cluster_review_file <- function(evidence, root_dir) {
  root_dir <- .as_scalar_character(root_dir)
  if (is.na(root_dir) || !nzchar(root_dir)) {
    stop("`review_dir` must be one non-empty path.", call. = FALSE)
  }

  dataset <- .cluster_review_dataset_info(evidence)
  cluster_id <- .cluster_evidence_cluster_id(evidence) %||% "cluster"

  subdir <- if (!is.na(dataset$folder_slug) && nzchar(dataset$folder_slug)) {
    file.path(root_dir, dataset$folder_slug)
  } else {
    stamp <- Sys.time()
    file.path(
      root_dir,
      format(stamp, "%Y-%m-%d"),
      format(stamp, "%H%M%S")
    )
  }

  .unique_cluster_review_path(file.path(subdir, paste0(cluster_id, "_review.md")))
}

.unique_cluster_review_path <- function(path) {
  if (!file.exists(path)) {
    return(path)
  }

  dir_name <- dirname(path)
  ext <- tools::file_ext(path)
  base_name <- basename(path)
  stem <- if (nzchar(ext)) {
    sub(paste0("\\.", ext, "$"), "", base_name)
  } else {
    base_name
  }

  for (i in seq_len(999L)) {
    candidate <- file.path(
      dir_name,
      paste0(stem, "_", sprintf("%02d", i), if (nzchar(ext)) paste0(".", ext) else "")
    )
    if (!file.exists(candidate)) {
      return(candidate)
    }
  }

  stop("Could not create a unique review path under: ", dir_name, call. = FALSE)
}

.render_cluster_review_lines <- function(
    output,
    evidence,
    validation,
    metadata,
    full,
    include_front_matter,
    manual_notes,
    title
) {
  dataset_lines <- .cluster_review_dataset_lines(evidence)
  generation_lines <- .cluster_review_generation_lines(metadata)
  summary_lines <- .cluster_review_summary_lines(validation, metadata)
  label_lines <- .cluster_review_label_lines(output)
  claims_lines <- .cluster_review_claim_lines(output$basis_in_data)
  key_species_lines <- .cluster_review_key_species_lines(output$key_species)
  external_lines <- .cluster_review_external_knowledge_lines(output$external_knowledge)
  unconfirmed_lines <- .cluster_review_unconfirmed_lines(output$not_confirmed_by_data)
  confidence_lines <- .cluster_review_confidence_lines(output$confidence)
  issue_lines <- .cluster_review_issue_lines(validation$issues)
  checks_lines <- .cluster_review_check_lines(output$checks_to_run)
  evidence_snapshot <- .format_cluster_evidence_prompt(evidence)
  evidence_limitations <- .cluster_review_evidence_limitation_lines(evidence)
  manual_note_lines <- .cluster_review_manual_notes_lines(manual_notes)

  lines <- character()

  if (isTRUE(include_front_matter)) {
    lines <- c(lines, "---", .cluster_review_yaml_lines(metadata), "---", "")
  }

  lines <- c(lines, paste0("# ", title), "")

  if (isTRUE(full)) {
    lines <- c(
      lines,
      "## Dataset",
      dataset_lines,
      "",
      "## Generation setup",
      generation_lines,
      "",
      "## Review summary",
      summary_lines,
      "",
      "## Proposed label",
      label_lines,
      "",
      "## Interpretation summary",
      .paragraph_or_placeholder(.as_scalar_character(output$interpretation_summary)),
      "",
      "## Evidence-backed claims",
      claims_lines,
      "",
      "## Key species",
      key_species_lines,
      "",
      "## External knowledge used",
      external_lines,
      "",
      "## What is not confirmed",
      unconfirmed_lines,
      "",
      "## Confidence",
      confidence_lines,
      "",
      "## Validation warnings",
      issue_lines,
      "",
      "## Checks to run",
      checks_lines,
      "",
      "## Evidence snapshot",
      "```text",
      evidence_snapshot,
      "```",
      "",
      "## Evidence limitations",
      evidence_limitations,
      "",
      "## Manual review notes",
      manual_note_lines
    )
  } else {
    lines <- c(
      lines,
      "## Dataset",
      dataset_lines,
      "",
      "## Generation setup",
      generation_lines,
      "",
      "## Proposed label",
      label_lines,
      "",
      "## Interpretation summary",
      .paragraph_or_placeholder(.as_scalar_character(output$interpretation_summary)),
      "",
      "## Evidence-backed claims",
      claims_lines,
      "",
      "## Key species",
      key_species_lines,
      "",
      "## What is not confirmed",
      unconfirmed_lines,
      "",
      "## Confidence",
      confidence_lines
    )
  }

  lines
}

.cluster_review_summary_lines <- function(validation, metadata) {
  issues <- validation$issues %||% .new_cluster_label_issue_table()
  unsupported_codes <- unique(issues$code[issues$category == "unsupported_claims"])
  warning_codes <- unique(issues$code[issues$severity == "warning"])

  lines <- c(
    paste0("- Review status: `", metadata$review_status, "`"),
    paste0("- Output status: `", metadata$output_status, "`"),
    paste0("- Validation status: `", metadata$validation_status, "`"),
    paste0("- Needs human review: `", tolower(as.character(metadata$needs_human_review)), "`"),
    paste0(
      "- Evidence citation coverage: `",
      .format_numeric_scalar(metadata$evidence_coverage_score, digits = 2L),
      "` (`",
      metadata$evidence_citation_valid,
      "/",
      metadata$evidence_citation_total,
      "`)"
    ),
    paste0(
      "- Validator issue counts: `",
      metadata$error_count,
      "` error(s), `",
      metadata$warning_count,
      "` warning(s)"
    )
  )

  if (length(unsupported_codes)) {
    lines <- c(
      lines,
      paste0("- Unsupported-claim flags: ", paste(.md_code(unsupported_codes), collapse = ", "))
    )
  } else {
    lines <- c(lines, "- Unsupported-claim flags: none")
  }

  if (length(warning_codes)) {
    lines <- c(
      lines,
      paste0("- Warning codes: ", paste(.md_code(warning_codes), collapse = ", "))
    )
  } else {
    lines <- c(lines, "- Warning codes: none")
  }

  lines
}

.cluster_review_dataset_lines <- function(evidence) {
  dataset <- .cluster_review_dataset_info(evidence)
  lines <- character()

  if (!is.na(dataset$path) && nzchar(dataset$path)) {
    lines <- c(lines, paste0("- Dataset path: ", .md_code(dataset$path)))
  } else if (!is.na(dataset$type) && nzchar(dataset$type)) {
    lines <- c(lines, paste0("- Dataset: ", .md_code(dataset$type)))
  } else {
    lines <- c(lines, "- Dataset: not recorded.")
  }

  if (!is.na(dataset$type) && nzchar(dataset$type) &&
      !is.na(dataset$path) && nzchar(dataset$path)) {
    lines <- c(lines, paste0("- Dataset type: ", .md_code(dataset$type)))
  }

  if (!is.na(dataset$label) && nzchar(dataset$label)) {
    lines <- c(lines, paste0("- Dataset label: ", .md_code(dataset$label)))
  }

  lines
}

.cluster_review_generation_lines <- function(metadata) {
  model <- .as_scalar_character(metadata$model)
  variant <- .as_scalar_character(metadata$variant)
  system_prompt <- .as_scalar_character(metadata$prompt_system_path)
  user_prompt <- .as_scalar_character(metadata$prompt_user_path)
  gate_variant <- .as_scalar_character(metadata$gate_variant)
  gate_user_prompt <- .as_scalar_character(metadata$gate_prompt_user_path)

  any_main_prompt <- (!is.na(system_prompt) && nzchar(system_prompt)) ||
    (!is.na(user_prompt) && nzchar(user_prompt))

  lines <- character()

  if (!is.na(model) && nzchar(model)) {
    lines <- c(lines, paste0("- Model: ", .md_code(model)))
  } else {
    lines <- c(lines, "- Model: not recorded.")
  }

  if (!is.na(variant) && nzchar(variant)) {
    lines <- c(lines, paste0("- Prompt variant: ", .md_code(variant)))
  }

  if (any_main_prompt) {
    if (!is.na(system_prompt) && nzchar(system_prompt)) {
      lines <- c(lines, paste0("- System prompt: ", .md_code(system_prompt)))
    }
    if (!is.na(user_prompt) && nzchar(user_prompt)) {
      lines <- c(lines, paste0("- User prompt: ", .md_code(user_prompt)))
    }
  } else {
    lines <- c(lines, "- Prompt files: not recorded.")
  }

  if (!is.na(gate_variant) && nzchar(gate_variant)) {
    lines <- c(lines, paste0("- Gate variant: ", .md_code(gate_variant)))
  }
  if (!is.na(gate_user_prompt) && nzchar(gate_user_prompt)) {
    lines <- c(lines, paste0("- Gate user prompt: ", .md_code(gate_user_prompt)))
  }

  lines
}

.cluster_review_label_lines <- function(output) {
  status <- .as_scalar_character(output$status)

  if (identical(status, "abstain")) {
    return(c(
      "- Status: `abstain`",
      paste0(
        "- Abstain reason: ",
        .paragraph_or_placeholder(.as_scalar_character(output$abstain_reason))
      )
    ))
  }

  c(
    "- Status: `labeled`",
    paste0("- Canonical label: ", .md_code(.as_scalar_character(output$canonical_label))),
    paste0("- Display label: ", .md_code(.as_scalar_character(output$display_label)))
  )
}

.cluster_review_claim_lines <- function(basis_in_data) {
  if (!length(basis_in_data)) {
    return("- None provided.")
  }

  vapply(seq_along(basis_in_data), function(i) {
    claim <- basis_in_data[[i]]
    claim_id <- .md_code(.as_scalar_character(claim$claim_id))
    statement <- .paragraph_or_placeholder(.as_scalar_character(claim$statement))
    evidence_ids <- .collapse_md_codes(.as_character_vector(claim$evidence_ids))
    support <- .as_scalar_character(claim$support_strength)
    paste0(
      "- ", claim_id, " ", statement,
      " Evidence: ", evidence_ids,
      if (!is.na(support) && nzchar(support)) paste0(". Support: `", support, "`.") else "."
    )
  }, character(1))
}

.cluster_review_key_species_lines <- function(key_species) {
  if (!length(key_species)) {
    return("- None provided.")
  }

  vapply(seq_along(key_species), function(i) {
    item <- key_species[[i]]
    species <- .paragraph_or_placeholder(.as_scalar_character(item$species))
    role <- .as_scalar_character(item$role)
    evidence_ids <- .collapse_md_codes(.as_character_vector(item$evidence_ids))
    paste0(
      "- ", .md_code(species),
      if (!is.na(role) && nzchar(role)) paste0(" (`", role, "`)") else "",
      " Evidence: ", evidence_ids, "."
    )
  }, character(1))
}

.cluster_review_external_knowledge_lines <- function(external_knowledge) {
  if (!length(external_knowledge)) {
    return("- None.")
  }

  vapply(seq_along(external_knowledge), function(i) {
    item <- external_knowledge[[i]]
    statement <- .paragraph_or_placeholder(.as_scalar_character(item$statement))
    knowledge_type <- .as_scalar_character(item$knowledge_type)
    confidence <- .as_scalar_character(item$confidence)
    paste0(
      "- ", statement,
      if (!is.na(knowledge_type) && nzchar(knowledge_type)) {
        paste0(" Type: `", knowledge_type, "`.")
      } else {
        ""
      },
      if (!is.na(confidence) && nzchar(confidence)) {
        paste0(" Confidence: `", confidence, "`.")
      } else {
        ""
      }
    )
  }, character(1))
}

.cluster_review_unconfirmed_lines <- function(not_confirmed_by_data) {
  if (!length(not_confirmed_by_data)) {
    return("- None.")
  }

  vapply(seq_along(not_confirmed_by_data), function(i) {
    item <- not_confirmed_by_data[[i]]
    statement <- .paragraph_or_placeholder(.as_scalar_character(item$statement))
    reason <- .paragraph_or_placeholder(.as_scalar_character(item$reason))
    paste0("- ", statement, " Reason: ", reason)
  }, character(1))
}

.cluster_review_confidence_lines <- function(confidence) {
  if (!is.list(confidence) || !length(confidence)) {
    return("- Confidence was not provided.")
  }

  c(
    paste0("- Score: `", .format_numeric_scalar(confidence$score, digits = 2L), "`"),
    paste0(
      "- Rationale: ",
      .paragraph_or_placeholder(.as_scalar_character(confidence$rationale))
    )
  )
}

.cluster_review_issue_lines <- function(issues) {
  if (!is.data.frame(issues) || nrow(issues) == 0L) {
    return("- No validator issues.")
  }

  vapply(seq_len(nrow(issues)), function(i) {
    paste0(
      "- [", issues$severity[[i]], "] ",
      .md_code(issues$code[[i]]),
      " ",
      issues$message[[i]]
    )
  }, character(1))
}

.cluster_review_check_lines <- function(checks_to_run) {
  if (!length(checks_to_run)) {
    return("- None suggested.")
  }

  vapply(seq_along(checks_to_run), function(i) {
    item <- checks_to_run[[i]]
    check <- .paragraph_or_placeholder(.as_scalar_character(item$check))
    priority <- .as_scalar_character(item$priority)
    reason <- .paragraph_or_placeholder(.as_scalar_character(item$reason))
    paste0(
      "- ",
      if (!is.na(priority) && nzchar(priority)) paste0("`", priority, "` ") else "",
      check,
      ". Reason: ", reason
    )
  }, character(1))
}

.cluster_review_evidence_limitation_lines <- function(evidence) {
  lines <- character()

  missing_components <- evidence$limitations$missing_components %||% character()
  warnings <- evidence$limitations$warnings %||% character()
  unsupported <- evidence$limitations$unsupported_inferences %||% character()

  if (length(missing_components)) {
    lines <- c(
      lines,
      paste0("- Missing components: ", paste(missing_components, collapse = ", "))
    )
  }
  if (length(warnings)) {
    lines <- c(lines, paste0("- ", warnings))
  }
  if (length(unsupported)) {
    lines <- c(lines, paste0("- ", unsupported))
  }

  if (!length(lines)) {
    return("- No explicit evidence limitations were recorded.")
  }

  lines
}

.cluster_review_manual_notes_lines <- function(manual_notes) {
  if (is.null(manual_notes)) {
    return(c(
      "- Reviewer:",
      "- Review date:",
      "- Decision: `accept` / `revise` / `abstain` / `defer`",
      "- Revised display label:",
      "- Revised canonical label:",
      "- Notes:"
    ))
  }

  notes <- as.character(manual_notes)
  notes <- notes[!is.na(notes)]
  if (!length(notes)) {
    return("- Notes:")
  }
  notes
}

.cluster_review_yaml_lines <- function(metadata) {
  keys <- names(metadata)
  vapply(seq_along(keys), function(i) {
    key <- keys[[i]]
    paste0(key, ": ", .yaml_scalar(metadata[[key]]))
  }, character(1))
}

.yaml_scalar <- function(x) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) {
    return("null")
  }
  if (is.logical(x)) {
    return(ifelse(isTRUE(x), "true", "false"))
  }
  if (is.numeric(x)) {
    return(as.character(signif(x, digits = 6L)))
  }
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\"", "\\\\\"", x)
  paste0("\"", x, "\"")
}

.default_cluster_review_metadata_file <- function(file) {
  if (is.null(file)) {
    return(NULL)
  }
  if (grepl("\\.[Rr]?[Mm][Dd]$", file)) {
    return(sub("\\.[Rr]?[Mm][Dd]$", ".meta.json", file))
  }
  paste0(file, ".meta.json")
}

.md_code <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x) || !nzchar(x)) {
    return("`<missing>`")
  }
  paste0("`", x, "`")
}

.collapse_md_codes <- function(x) {
  x <- .as_character_vector(x)
  if (!length(x)) {
    return("none")
  }
  paste(vapply(x, .md_code, character(1)), collapse = ", ")
}

.format_numeric_scalar <- function(x, digits = 2L) {
  if (length(x) != 1L || is.null(x) || is.na(x)) {
    return("NA")
  }
  formatC(as.numeric(x), digits = digits, format = "f")
}

.paragraph_or_placeholder <- function(x) {
  if (is.na(x) || !nzchar(x)) {
    return("Not provided.")
  }
  x
}
