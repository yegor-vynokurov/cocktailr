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
#' For labeled outputs, the rendered card shows the full stored
#' \code{display_label} and, when shorter, a separate plot-preview label derived
#' from the registry preview rule.
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
  full <- .arg_single_flag(full, "full")
  include_front_matter <- .arg_single_flag(
    include_front_matter,
    "include_front_matter"
  )
  write_metadata <- .arg_single_flag(write_metadata, "write_metadata")
  file <- .arg_nullable_scalar_character(file, "file")
  review_dir <- .arg_nullable_scalar_character(review_dir, "review_dir")
  metadata_file <- .arg_nullable_scalar_character(metadata_file, "metadata_file")
  title <- .arg_nullable_scalar_character(title, "title")

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
    source = x,
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

  model_logs_out <- NULL
  if (!is.null(file_out)) {
    model_logs_out <- .write_cluster_review_model_logs(
      x = x,
      review_file = file_out
    )
  }
  if (!is.null(model_logs_out)) {
    metadata$review_model_logs_dir <- model_logs_out
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
    metadata_file = metadata_out,
    model_logs_dir = model_logs_out
  )
  class(out) <- c("cluster_review_artifact", "list")
  out
}

#' @method print cluster_review_artifact
#' @export
print.cluster_review_artifact <- function(x, ...) {
  cat(x$markdown, "\n", sep = "")
  invisible(x)
}

.build_cluster_review_metadata <- function(x, evidence, validation, title, full) {
  output <- validation$output %||% .extract_cluster_label_output(x)
  issues <- validation$issues %||% .new_cluster_label_issue_table()
  provenance <- .cluster_review_provenance(x)
  public_label <- .cluster_label_public_fields(output, provenance)
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
    label_tier = .cluster_label_validation_label_tier(validation),
    is_speculative = isTRUE(validation$is_speculative),
    plot_marker = .cluster_label_validation_plot_marker(validation),
    strict_outcome = validation$strict_outcome %||% NA_character_,
    strict_validation_status = validation$strict_validation_status %||% NA_character_,
    label_origin = validation$label_origin %||% NA_character_,
    species_entropy_band = validation$species_entropy_band %||% NA_character_,
    species_entropy_text = validation$species_entropy_text %||% NA_character_,
    chaoticity_score = validation$chaoticity_score %||% NA_integer_,
    chaoticity_label = validation$chaoticity_label %||% NA_character_,
    missing_for_confidence_text = validation$missing_for_confidence_text %||%
      .cluster_label_missing_for_confidence_from_output(output),
    evidence_coverage_score = coverage$score %||% NA_real_,
    evidence_citation_valid = coverage$n_valid_citations %||% NA_integer_,
    evidence_citation_total = coverage$n_citation_targets %||% NA_integer_,
    issue_count = nrow(issues),
    warning_count = sum(issues$severity == "warning"),
    error_count = sum(issues$severity == "error"),
    unsupported_claim_issue_count = sum(issues$category == "unsupported_claims"),
    proposed_canonical_label = .as_scalar_character(output$canonical_label),
    proposed_display_label = .as_scalar_character(output$display_label),
    proposed_plot_preview_label = .cluster_label_registry_plot_preview(
      .cluster_label_display_with_marker(
        output$display_label,
        .cluster_label_validation_plot_marker(validation)
      )
    ),
    public_canonical_label = public_label$public_canonical_label %||% NA_character_,
    public_display_label = public_label$public_display_label %||% NA_character_,
    public_label_source = public_label$public_label_source %||% NA_character_,
    brainstorm_used = if (is.null(provenance$brainstorm_skipped)) {
      NA
    } else {
      !isTRUE(provenance$brainstorm_skipped)
    },
    brainstorm_candidate_count = length(provenance$brainstorm_candidates %||% list()),
    user_added_data_present = isTRUE(evidence$meta$user_added_data_present %||% FALSE),
    user_added_data_entry_count = as.integer(
      evidence$meta$user_added_data_entry_count %||% 0L
    ),
    life_form_layer_present = isTRUE(
      evidence$meta$source$has_life_form_layer %||% FALSE
    ),
    life_form_layer_status = .as_scalar_character(
      evidence$meta$enrichment_layers$life_form_layer$status %||% NA_character_
    ),
    life_form_layer_error = .as_scalar_character(
      evidence$meta$enrichment_layers$life_form_layer$error %||% NA_character_
    ),
    life_form_evidence_count = as.integer(
      nrow(evidence$summaries$life_form_summary %||% data.frame())
    ),
    life_form_unmatched_count = as.integer(
      length(evidence$summaries$life_form_unmatched_species %||% character(0))
    ),
    provider = provenance$provider,
    model = provenance$model,
    variant = provenance$variant,
    workflow_steps = provenance$workflow_steps,
    selected_label_variant = provenance$selected_label_variant,
    label_stage_exhausted = if (is.null(provenance$label_stage_exhausted)) {
      NA
    } else {
      isTRUE(provenance$label_stage_exhausted)
    },
    label_stage_failure_reason = provenance$label_stage_failure_reason,
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
      selected_label_variant = NULL,
      label_stage_exhausted = NULL,
      label_stage_failure_reason = NULL,
      gate_variant = NULL,
      gate_decision = NULL,
      log_run_dir = NULL,
      prompt_catalog_path = NULL,
      prompt_schema_path = NULL,
      prompt_system_path = NULL,
      prompt_user_path = NULL,
      gate_prompt_system_path = NULL,
      gate_prompt_user_path = NULL,
      draft_analysis = NULL,
      brainstorm_skipped = NULL,
      brainstorm_candidates = list(),
      explanation_fallback_used = NULL,
      source_class = class(x)[1L] %||% "list"
    ))
  }

  workflow <- x$workflow %||% list()
  gate_stage <- if (is.list(workflow)) workflow$gate else NULL
  label_stage <- if (is.list(workflow)) workflow$label else NULL
  draft_stage <- if (is.list(workflow)) workflow$draft else NULL
  explanation_stage <- if (is.list(workflow)) workflow$explanation else NULL
  gate_output <- if (is.list(gate_stage)) gate_stage$output %||% NULL else NULL
  main_prompt <- x$prompt %||% list()
  gate_prompt <- if (is.list(gate_stage)) gate_stage$prompt %||% list() else list()
  draft_analysis <- if (is.list(draft_stage)) {
    .as_scalar_character(draft_stage$output$draft_analysis %||% NULL)
  } else {
    NA_character_
  }
  brainstorm_skipped <- if (is.list(draft_stage)) {
    isTRUE(draft_stage$skipped)
  } else {
    NULL
  }
  brainstorm_candidates <- if (!isTRUE(brainstorm_skipped) &&
      .is_non_empty_scalar_character(draft_analysis)) {
    .extract_cluster_label_candidates_from_draft(draft_analysis)
  } else {
    list()
  }
  label_stage_failure_reason <- if (is.list(label_stage)) {
    .as_scalar_character(
      label_stage$selection_output$fallback_reason %||%
        paste(label_stage$failure_messages %||% character(0), collapse = " | ")
    )
  } else {
    NA_character_
  }

  list(
    provider = .null_default(x$provider, NULL),
    model = .null_default(x$model, NULL),
    variant = .null_default(x$variant, NULL),
    workflow_steps = .null_default(x$workflow_steps, NULL),
    selected_label_variant = if (is.list(label_stage)) {
      label_stage$selected_public_variant %||% NULL
    } else {
      NULL
    },
    label_stage_exhausted = if (is.list(label_stage)) {
      isTRUE(label_stage$exhausted)
    } else {
      NULL
    },
    label_stage_failure_reason = if (is.na(label_stage_failure_reason) ||
      !nzchar(label_stage_failure_reason)) {
      NULL
    } else {
      label_stage_failure_reason
    },
    gate_variant = if (is.list(gate_stage)) gate_stage$variant %||% NULL else NULL,
    gate_decision = gate_output$decision %||% NULL,
    log_run_dir = x$logs$run_dir %||% NULL,
    prompt_catalog_path = main_prompt$catalog_path %||% NULL,
    prompt_schema_path = main_prompt$schema_path %||% NULL,
    prompt_system_path = main_prompt$system_path %||% NULL,
    prompt_user_path = main_prompt$user_path %||% NULL,
    gate_prompt_system_path = gate_prompt$system_path %||% NULL,
    gate_prompt_user_path = gate_prompt$user_path %||% NULL,
    draft_analysis = if (.is_non_empty_scalar_character(draft_analysis)) {
      draft_analysis
    } else {
      NULL
    },
    brainstorm_skipped = brainstorm_skipped,
    brainstorm_candidates = brainstorm_candidates,
    explanation_fallback_used = if (is.list(explanation_stage)) {
      isTRUE(explanation_stage$fallback_used)
    } else {
      NULL
    },
    source_class = "cluster_label_result"
  )
}

.cluster_review_status <- function(validation) {
  if (identical(validation$validation_status, "schema_error")) {
    return("blocked")
  }
  if (isTRUE(validation$is_speculative) ||
      identical(.cluster_label_validation_label_tier(validation), "speculative")) {
    return("speculative")
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

.resolve_cluster_review_paths <- function(evidence, file, review_dir, metadata_file) {
  if (!is.null(file) && !is.null(review_dir)) {
    stop("Use either `file` or `review_dir`, not both.", call. = FALSE)
  }

  # Resolve relative output paths against the package source root when we can
  # detect a local checkout. This keeps review artifacts in one predictable
  # workspace across interactive sessions.
  file_target <- if (!is.null(file)) .resolve_cocktailr_output_path(file) else NULL
  if (is.null(file_target) && !is.null(review_dir)) {
    file_target <- .default_cluster_review_file(
      evidence = evidence,
      root_dir = .resolve_cocktailr_output_path(review_dir)
    )
  }

  metadata_target <- if (!is.null(metadata_file)) {
    .resolve_cocktailr_output_path(metadata_file)
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
    source,
    full,
    include_front_matter,
    manual_notes,
    title
) {
  dataset_lines <- .cluster_review_dataset_lines(evidence)
  generation_lines <- .cluster_review_generation_lines(metadata)
  summary_lines <- .cluster_review_summary_lines(validation, metadata)
  label_lines <- .cluster_review_label_lines(output, metadata)
  tentative_lines <- .cluster_review_tentative_lines(metadata)
  brainstorm_lines <- .cluster_review_brainstorm_lines(source)
  workflow_trace_lines <- .cluster_review_workflow_trace_lines(source)
  user_added_data_lines <- .cluster_review_user_added_data_lines(evidence)
  life_form_lines <- .cluster_review_life_form_lines(evidence)
  claims_lines <- .cluster_review_claim_lines(output$basis_in_data)
  key_species_lines <- .cluster_review_key_species_lines(output$key_species)
  external_lines <- .cluster_review_external_knowledge_lines(output$external_knowledge)
  unconfirmed_lines <- .cluster_review_unconfirmed_lines(output$not_confirmed_by_data)
  confidence_lines <- .cluster_review_confidence_lines(output$confidence)
  issue_lines <- .cluster_review_issue_lines(validation$issues)
  checks_lines <- .cluster_review_check_lines(output$checks_to_run)
  evidence_snapshot <- .format_cluster_evidence_review_prompt(evidence)
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
      if (length(brainstorm_lines)) "" else NULL,
      if (length(brainstorm_lines)) "## Brainstorm trace" else NULL,
      brainstorm_lines,
      if (length(user_added_data_lines)) "" else NULL,
      if (length(user_added_data_lines)) "## User-added context" else NULL,
      user_added_data_lines,
      if (length(life_form_lines)) "" else NULL,
      if (length(life_form_lines)) "## Life-form context" else NULL,
      life_form_lines,
      "",
      "## Review summary",
      summary_lines,
      "",
      "## Proposed label",
      label_lines,
      if (length(tentative_lines)) "" else NULL,
      if (length(tentative_lines)) "## Why tentative" else NULL,
      tentative_lines,
      if (length(workflow_trace_lines)) "" else NULL,
      if (length(workflow_trace_lines)) "## Workflow trace" else NULL,
      workflow_trace_lines,
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
      if (length(tentative_lines)) "" else NULL,
      if (length(tentative_lines)) "## Why tentative" else NULL,
      tentative_lines,
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

  if (!is.na(metadata$label_tier) && nzchar(metadata$label_tier)) {
    lines <- c(lines, paste0("- Label tier: `", metadata$label_tier, "`"))
  }

  if (isTRUE(metadata$is_speculative)) {
    lines <- c(
      lines,
      paste0("- Strict outcome before fallback: `", metadata$strict_outcome %||% NA_character_, "`"),
      paste0(
        "- Strict validation status before fallback: `",
        metadata$strict_validation_status %||% NA_character_,
        "`"
      ),
      paste0("- Plot marker: ", .md_code(metadata$plot_marker %||% "*"))
    )
  }

  if (!is.na(metadata$label_origin) && nzchar(metadata$label_origin)) {
    lines <- c(lines, paste0("- Label origin: `", metadata$label_origin, "`"))
  }
  if (!is.na(metadata$species_entropy_text) && nzchar(metadata$species_entropy_text)) {
    lines <- c(
      lines,
      paste0("- Species-composition entropy: `", metadata$species_entropy_text, "`")
    )
  }
  if (!is.na(metadata$chaoticity_score)) {
    lines <- c(
      lines,
      paste0(
        "- Chaoticity score: `",
        metadata$chaoticity_score,
        "`",
        if (!is.na(metadata$chaoticity_label) && nzchar(metadata$chaoticity_label)) {
          paste0(" (`", metadata$chaoticity_label, "`)")
        } else {
          ""
        }
      )
    )
  }

  if (length(unsupported_codes)) {
    lines <- c(
      lines,
      paste0("- Unsupported-claim flags: ", .collapse_md_codes(unsupported_codes))
    )
  } else {
    lines <- c(lines, "- Unsupported-claim flags: none")
  }

  if (length(warning_codes)) {
    lines <- c(
      lines,
      paste0("- Warning codes: ", .collapse_md_codes(warning_codes))
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
  selected_label_variant <- .as_scalar_character(metadata$selected_label_variant)
  label_stage_failure_reason <- .as_scalar_character(metadata$label_stage_failure_reason)
  label_stage_exhausted <- metadata$label_stage_exhausted %||% NULL
  brainstorm_used <- metadata$brainstorm_used %||% NULL
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
  if (isTRUE(brainstorm_used) || identical(brainstorm_used, FALSE)) {
    lines <- c(
      lines,
      paste0(
        "- Brainstorm used: ",
        .md_code(tolower(as.character(isTRUE(brainstorm_used))))
      )
    )
  }
  if (!is.na(selected_label_variant) && nzchar(selected_label_variant)) {
    lines <- c(
      lines,
      paste0("- Selected label rung: ", .md_code(selected_label_variant))
    )
  }
  if (isTRUE(label_stage_exhausted) || identical(label_stage_exhausted, FALSE)) {
    lines <- c(
      lines,
      paste0(
        "- Label-stage exhausted: ",
        .md_code(tolower(as.character(isTRUE(label_stage_exhausted))))
      )
    )
  }
  if (!is.na(label_stage_failure_reason) && nzchar(label_stage_failure_reason)) {
    lines <- c(
      lines,
      paste0(
        "- Label-stage fallback reason: ",
        label_stage_failure_reason
      )
    )
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

.cluster_review_label_lines <- function(output, metadata) {
  status <- .as_scalar_character(output$status)
  label_summary <- .as_scalar_character(output$label_summary)
  public_display_label <- .as_scalar_character(metadata$public_display_label)
  public_canonical_label <- .as_scalar_character(metadata$public_canonical_label)
  public_label_source <- .as_scalar_character(metadata$public_label_source)

  if (identical(status, "abstain")) {
    lines <- c(
      "- Status: `abstain`",
      "- Model output: no stable short label was produced.",
      paste0(
        "- Abstain reason: ",
        .paragraph_or_placeholder(.as_scalar_character(output$abstain_reason))
      )
    )
    if (!is.na(public_label_source) &&
        identical(public_label_source, "post_abstain_fallback")) {
      lines <- c(
        lines,
        paste0(
          "- Programmatic public fallback display label: ",
          .md_code(public_display_label)
        ),
        paste0(
          "- Programmatic public fallback canonical label: ",
          .md_code(public_canonical_label)
        ),
        paste0(
          "- Public label source: ",
          .md_code(public_label_source)
        ),
        paste(
          "- Note: this fallback label is injected downstream for review,",
          "registry, and plotting convenience only; it is not the model's label."
        )
      )
    }
    if (!is.na(metadata$label_origin) && nzchar(metadata$label_origin)) {
      lines <- c(lines, paste0("- Label origin: `", metadata$label_origin, "`"))
    }
    if (!is.na(metadata$species_entropy_text) && nzchar(metadata$species_entropy_text)) {
      lines <- c(
        lines,
        paste0("- Species-composition entropy: `", metadata$species_entropy_text, "`")
      )
    }
    if (!is.na(metadata$chaoticity_score)) {
      lines <- c(
        lines,
        paste0(
          "- Chaoticity score: `",
          metadata$chaoticity_score,
          "`",
          if (!is.na(metadata$chaoticity_label) && nzchar(metadata$chaoticity_label)) {
            paste0(" (`", metadata$chaoticity_label, "`)")
          } else {
            ""
          }
        )
      )
    }
    return(lines)
  }

  display_label <- .cluster_label_display_with_marker(
    output$display_label,
    metadata$plot_marker %||% ""
  )
  category_label <- .as_scalar_character(output$category_label)
  subcategory_labels <- .cluster_label_subcategory_labels_text(output$subcategory_labels)
  plot_preview_label <- .as_scalar_character(
    metadata$proposed_plot_preview_label %||% NA_character_
  )

  lines <- c("- Status: `labeled`")

  if (isTRUE(metadata$is_speculative)) {
    lines <- c(
      lines,
      "- Label tier: `speculative`",
      "- Human review: strongly recommended."
    )
  }

  c(
    lines,
    paste0("- Canonical label: ", .md_code(.as_scalar_character(output$canonical_label))),
    paste0("- Display label: ", .md_code(display_label)),
    if (!is.na(category_label) && nzchar(category_label)) {
      paste0("- Category label: ", .md_code(category_label))
    },
    if (!is.na(subcategory_labels) && nzchar(subcategory_labels)) {
      paste0("- Subcategory labels: ", .md_code(subcategory_labels))
    },
    if (!is.na(plot_preview_label) &&
        nzchar(plot_preview_label) &&
        !identical(plot_preview_label, display_label)) {
      paste0("- Plot preview label: ", .md_code(plot_preview_label))
    },
    if (!is.na(label_summary) && nzchar(label_summary)) {
      paste0("- Label summary: ", label_summary)
    },
    if (isTRUE(metadata$label_stage_exhausted)) {
      paste(
        "- Cascade note: all public label-selection rungs were exhausted,",
        "so this broad fallback label should be reviewed manually."
      )
    },
    if (!is.na(metadata$label_origin) && nzchar(metadata$label_origin)) {
      paste0("- Label origin: `", metadata$label_origin, "`")
    },
    if (!is.na(metadata$species_entropy_text) && nzchar(metadata$species_entropy_text)) {
      paste0("- Species-composition entropy: `", metadata$species_entropy_text, "`")
    },
    if (!is.na(metadata$chaoticity_score)) {
      paste0(
        "- Chaoticity score: `",
        metadata$chaoticity_score,
        "`",
        if (!is.na(metadata$chaoticity_label) && nzchar(metadata$chaoticity_label)) {
          paste0(" (`", metadata$chaoticity_label, "`)")
        } else {
          ""
        }
      )
    }
  )
}

.cluster_review_brainstorm_lines <- function(x) {
  provenance <- .cluster_review_provenance(x)
  draft_analysis <- .as_scalar_character(provenance$draft_analysis)
  brainstorm_candidates <- provenance$brainstorm_candidates %||% list()

  if (isTRUE(provenance$brainstorm_skipped)) {
    return(
      "- Brainstorm step was skipped; selection and explanation used the cluster evidence directly."
    )
  }

  lines <- character()

  if (is.list(brainstorm_candidates) && length(brainstorm_candidates)) {
    for (candidate in brainstorm_candidates) {
      display_label <- .as_scalar_character(candidate$display_label)
      canonical_label <- .as_scalar_character(candidate$canonical_label)
      if (!.is_non_empty_scalar_character(display_label) ||
          !.is_non_empty_scalar_character(canonical_label)) {
        next
      }
      lines <- c(
        lines,
        paste0(
          "- Candidate label: ",
          .md_code(display_label),
          " -> ",
          .md_code(canonical_label)
        )
      )
    }
  }

  main_signal_lines <- .cluster_label_draft_section_lines(
    draft_analysis,
    "main signal"
  )
  if (length(main_signal_lines)) {
    lines <- c(
      lines,
      paste0("- Main signal: ", main_signal_lines)
    )
  }

  conflict_lines <- .cluster_label_draft_section_lines(
    draft_analysis,
    "noise or conflicts"
  )
  if (length(conflict_lines)) {
    lines <- c(
      lines,
      paste0("- Conflict or noise: ", conflict_lines)
    )
  }

  overclaim_lines <- .cluster_label_draft_section_lines(
    draft_analysis,
    "what not to overclaim"
  )
  if (length(overclaim_lines)) {
    lines <- c(
      lines,
      paste0("- Overclaim warning: ", overclaim_lines)
    )
  }

  if (!length(lines) && .is_non_empty_scalar_character(draft_analysis)) {
    lines <- c(lines, "- Brainstorm ran, but no compact candidate trace was recovered.")
  }

  lines
}

.cluster_review_stage_raw_evidence_used <- function(stage) {
  if (!is.list(stage) || !length(stage)) {
    return(NA)
  }

  prompt <- stage$prompt %||% list()
  evidence_text <- .as_scalar_character(prompt$evidence_text %||% NULL)
  user_text <- .as_scalar_character(prompt$user %||% NULL)

  if (.is_non_empty_scalar_character(evidence_text)) {
    return(TRUE)
  }

  if (.is_non_empty_scalar_character(user_text) &&
      grepl("Cluster evidence:", user_text, fixed = TRUE)) {
    return(TRUE)
  }

  FALSE
}

.cluster_review_workflow_trace_lines <- function(x) {
  if (!inherits(x, "cluster_label_result")) {
    return(character(0))
  }

  workflow <- x$workflow %||% NULL
  if (!is.list(workflow) || !length(workflow)) {
    return(character(0))
  }

  label_stage <- workflow$label %||% NULL
  summary_stage <- workflow$summary %||% NULL
  abstain_reason_stage <- workflow$abstain_reason %||% NULL
  output <- x$output %||% list()
  attempts <- if (is.list(label_stage)) label_stage$attempts %||% list() else list()

  lines <- character()

  if (is.list(attempts) && length(attempts)) {
    for (attempt in attempts) {
      public_variant <- .as_scalar_character(
        attempt$public_variant %||% attempt$variant %||% NULL
      )
      selection_variant <- .as_scalar_character(
        attempt$selection_variant %||% NULL
      )
      result <- .as_scalar_character(
        attempt$result %||% attempt$output$status %||% NULL
      )
      attempts_used <- attempt$attempts %||% NULL
      raw_response <- .as_scalar_character(
        attempt$response$content %||% NULL
      )
      parsed_output <- attempt$output %||% list()
      error_text <- .as_scalar_character(attempt$error %||% NULL)

      heading <- if (.is_non_empty_scalar_character(public_variant)) {
        paste0("### ", .md_code(public_variant))
      } else {
        "### Label-decision rung"
      }
      lines <- c(lines, heading)

      if (.is_non_empty_scalar_character(selection_variant)) {
        lines <- c(
          lines,
          paste0("- Internal label-decision prompt: ", .md_code(selection_variant))
        )
      }
      if (.is_non_empty_scalar_character(result)) {
        lines <- c(lines, paste0("- Result: ", .md_code(result)))
      }
      if (!is.null(attempts_used) && !is.na(attempts_used)) {
        lines <- c(
          lines,
          paste0("- Attempts used: ", .md_code(as.character(attempts_used)))
        )
      }
      if (.is_non_empty_scalar_character(error_text)) {
        lines <- c(lines, paste0("- Error: ", error_text))
      }

      lines <- c(lines, "", "#### Raw label-only answer")
      if (.is_non_empty_scalar_character(raw_response)) {
        lines <- c(lines, .md_text_block_lines(raw_response))
      } else {
        lines <- c(
          lines,
          "- Raw label-only answer is not available in-memory for this rung; inspect debug logs if they were written."
        )
      }

      lines <- c(lines, "", "#### Parsed label decision")
      if (is.list(parsed_output) && length(parsed_output)) {
        lines <- c(
          lines,
          paste0("- Status: ", .md_code(.as_scalar_character(parsed_output$status %||% NULL))),
          paste0("- Label decision text: ", .md_code(.empty_string_if_na(.as_scalar_character(parsed_output$label_decision_text %||% NULL)))),
          paste0("- Canonical label: ", .md_code(.empty_string_if_na(.as_scalar_character(parsed_output$canonical_label %||% NULL)))),
          paste0("- Display label: ", .md_code(.empty_string_if_na(.as_scalar_character(parsed_output$display_label %||% NULL))))
        )

        if (identical(.as_scalar_character(parsed_output$status %||% NULL), "abstain")) {
          lines <- c(
            lines,
            "- This is the model's abstain decision at the label-only step; any public fallback label shown elsewhere is added later by code."
          )
        }
      } else {
        lines <- c(lines, "- Parsed label decision was not recovered for this rung.")
      }

      lines <- c(lines, "")
    }
  }

  if (is.list(summary_stage) && length(summary_stage)) {
    lines <- c(lines, "### `label_summary_pass_v2`")
    if (.is_non_empty_scalar_character(.as_scalar_character(summary_stage$variant %||% NULL))) {
      lines <- c(
        lines,
        paste0("- Summary variant: ", .md_code(.as_scalar_character(summary_stage$variant %||% NULL)))
      )
    }
    if (isTRUE(summary_stage$skipped)) {
      lines <- c(
        lines,
        paste0("- Stage skipped: ", .md_code("true")),
        paste0("- Skip reason: ", .md_code(.empty_string_if_na(.as_scalar_character(summary_stage$skip_reason %||% NULL)))),
        ""
      )
    } else {
      lines <- c(
        lines,
        paste0(
          "- Raw cluster evidence re-read: ",
          .md_code(tolower(as.character(isTRUE(.cluster_review_stage_raw_evidence_used(summary_stage)))))
        ),
        "- Summary input scope: `label + brainstorm only`",
        "",
        "#### Raw summary answer"
      )

      raw_summary <- .as_scalar_character(summary_stage$response$content %||% NULL)
      if (.is_non_empty_scalar_character(raw_summary)) {
        lines <- c(lines, .md_text_block_lines(raw_summary))
      } else if (isTRUE(summary_stage$fallback_used)) {
        lines <- c(lines, "- No raw summary answer is attached because code assembled a programmatic fallback summary after the stage failed.")
      } else {
        lines <- c(lines, "- Raw summary answer is not available in-memory for this stage; inspect debug logs if they were written.")
      }

      lines <- c(lines, "", "#### Parsed summary")
      if (is.list(summary_stage$output) && length(summary_stage$output)) {
        lines <- c(
          lines,
          paste0("- Status: ", .md_code(.as_scalar_character(summary_stage$output$status %||% NULL))),
          paste0("- Label summary: ", .paragraph_or_placeholder(.as_scalar_character(summary_stage$output$label_summary %||% NULL)))
        )
      } else {
        lines <- c(lines, "- Parsed summary was not recovered.")
      }

      if (isTRUE(summary_stage$fallback_used)) {
        lines <- c(
          lines,
          paste0("- Programmatic fallback used: ", .md_code("true")),
          paste0("- Fallback reason: ", .paragraph_or_placeholder(.as_scalar_character(summary_stage$fallback_reason %||% NULL)))
        )
      }

      lines <- c(lines, "")
    }
  }

  if (is.list(abstain_reason_stage) && length(abstain_reason_stage)) {
    lines <- c(lines, "### `abstain_reason_pass_v2`")
    if (.is_non_empty_scalar_character(.as_scalar_character(abstain_reason_stage$variant %||% NULL))) {
      lines <- c(
        lines,
        paste0("- Abstain-reason variant: ", .md_code(.as_scalar_character(abstain_reason_stage$variant %||% NULL)))
      )
    }
    if (isTRUE(abstain_reason_stage$skipped)) {
      lines <- c(
        lines,
        paste0("- Stage skipped: ", .md_code("true")),
        paste0("- Skip reason: ", .md_code(.empty_string_if_na(.as_scalar_character(abstain_reason_stage$skip_reason %||% NULL)))),
        ""
      )
    } else {
      lines <- c(
        lines,
        "- This stage explains the already fixed label-only abstain decision; it does not create the downstream public fallback label.",
        "",
        "#### Raw abstain-reason answer"
      )

      raw_abstain_reason <- .as_scalar_character(
        abstain_reason_stage$response$content %||% NULL
      )
      if (.is_non_empty_scalar_character(raw_abstain_reason)) {
        lines <- c(lines, .md_text_block_lines(raw_abstain_reason))
      } else if (isTRUE(abstain_reason_stage$fallback_used)) {
        lines <- c(lines, "- No raw abstain-reason answer is attached because code assembled a programmatic fallback reason after the stage failed.")
      } else {
        lines <- c(lines, "- Raw abstain-reason answer is not available in-memory for this stage; inspect debug logs if they were written.")
      }

      lines <- c(lines, "", "#### Parsed abstain reason")
      if (is.list(abstain_reason_stage$output) && length(abstain_reason_stage$output)) {
        lines <- c(
          lines,
          paste0("- Status: ", .md_code(.as_scalar_character(abstain_reason_stage$output$status %||% NULL))),
          paste0("- Abstain reason: ", .paragraph_or_placeholder(.as_scalar_character(abstain_reason_stage$output$abstain_reason %||% NULL)))
        )
      } else {
        lines <- c(lines, "- Parsed abstain reason was not recovered.")
      }

      if (isTRUE(abstain_reason_stage$fallback_used)) {
        lines <- c(
          lines,
          paste0("- Programmatic fallback used: ", .md_code("true")),
          paste0("- Fallback reason: ", .paragraph_or_placeholder(.as_scalar_character(abstain_reason_stage$fallback_reason %||% NULL)))
        )
      }

      lines <- c(lines, "")
    }
  }

  if (identical(.as_scalar_character(output$status %||% NULL), "abstain") &&
      identical(.as_scalar_character(label_stage$selected_public_variant %||% NULL), "selection_all_abstain")) {
    lines <- c(
      lines,
      "### Model abstain vs public fallback",
      "- Model abstain: all label-decision rungs ended in abstain or failed to yield a usable short label.",
      "- Public fallback: any review-facing label such as `Chaotic Cluster` is injected downstream by code for registry / plotting convenience only.",
      ""
    )
  }

  lines
}

.cluster_review_user_added_data_lines <- function(evidence) {
  user_added_data <- evidence$user_added_data %||% NULL
  entries <- user_added_data$entries %||% list()
  if (!is.list(entries) || !length(entries)) {
    return(character(0))
  }

  source_type <- .as_scalar_character(user_added_data$source_type)
  if (is.na(source_type) || !nzchar(source_type)) {
    source_type <- "unknown"
  }

  lines <- c(
    paste0(
      "- Source type: ",
      .md_code(source_type)
    )
  )

  source_path <- .as_scalar_character(user_added_data$source_path)
  if (!is.na(source_path) && nzchar(source_path)) {
    lines <- c(lines, paste0("- Source path: ", .md_code(source_path)))
  }

  lines <- c(
    lines,
    paste0("- Loaded entries: ", .md_code(as.character(length(entries)))),
    paste0(
      "- Truncated: ",
      .md_code(tolower(as.character(isTRUE(user_added_data$truncated))))
    )
  )

  for (entry in entries) {
    name <- .as_scalar_character(entry$name)
    format <- .as_scalar_character(entry$format)
    label <- if (.is_non_empty_scalar_character(name)) {
      name
    } else {
      "inline_object"
    }
    if (.is_non_empty_scalar_character(format)) {
      label <- paste0(label, " [", format, "]")
    }
    if (isTRUE(entry$truncated)) {
      label <- paste0(label, " (truncated)")
    }
    lines <- c(lines, paste0("- Included user-added entry: ", label))
  }

  lines
}

.cluster_review_life_form_lines <- function(evidence) {
  summary <- evidence$summaries$life_form_summary %||% NULL
  unmatched <- evidence$summaries$life_form_unmatched_species %||% character(0)
  layer_meta <- evidence$meta$enrichment_layers$life_form_layer %||% list()
  status <- .as_scalar_character(layer_meta$status %||% NA_character_)
  error <- .as_scalar_character(layer_meta$error %||% NA_character_)

  has_summary <- is.data.frame(summary) && nrow(summary)
  has_unmatched <- length(unmatched) > 0L
  has_status <- !is.na(status) && nzchar(status) && status != "off"

  if (!has_summary && !has_unmatched && !has_status) {
    return(character(0))
  }

  lines <- if (has_status) {
    c(paste0("- Layer status: ", .md_code(status)))
  } else {
    character(0)
  }

  if (has_status && !is.na(error) && nzchar(error)) {
    lines <- c(lines, paste0("- Layer error: ", .md_code(error)))
  }

  if (has_summary) {
    summary <- as.data.frame(summary, stringsAsFactors = FALSE)
    if (!"matched_species" %in% names(summary)) {
      summary$matched_species <- ""
    }

    lines <- c(
      lines,
      paste0("- Distinct life-form signals: ", .md_code(as.character(nrow(summary))))
    )

    for (i in seq_len(nrow(summary))) {
      species_part <- trimws(as.character(summary$matched_species[[i]]))
      species_text <- if (nzchar(species_part)) {
        paste0("; matched species: ", species_part)
      } else {
        ""
      }

      lines <- c(
        lines,
        paste0(
          "- ",
          summary$label[[i]],
          ": ",
          summary$phrase[[i]],
          species_text
        )
      )
    }
  }

  if (has_unmatched) {
    lines <- c(
      lines,
      paste0(
        "- Unmatched life-form species: ",
        paste(as.character(unmatched), collapse = ", ")
      )
    )
  }

  lines
}

.cluster_review_explanation_lines <- function(output) {
  .paragraph_or_placeholder(.cluster_label_output_explanation_text(output))
}

.cluster_review_tentative_lines <- function(metadata) {
  if (!isTRUE(metadata$is_speculative)) {
    return(character(0))
  }

  lines <- c(
    paste0(
      "- The strict workflow did not accept a stable evidence-backed label (`",
      metadata$strict_outcome %||% "unknown",
      "`)."
    )
  )

  missing_text <- .as_scalar_character(metadata$missing_for_confidence_text)
  if (!is.na(missing_text) && nzchar(missing_text)) {
    lines <- c(lines, paste0("- What prevents full confidence: ", missing_text))
  } else {
    lines <- c(lines, "- What prevents full confidence: not explicitly recorded.")
  }

  lines
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

.cluster_review_has_model_trace <- function(x) {
  if (!is.list(x)) {
    return(FALSE)
  }

  if (inherits(x, "cluster_label_result")) {
    return(TRUE)
  }

  is.list(x$prompt %||% NULL) ||
    is.list(x$request %||% NULL) ||
    is.list(x$response %||% NULL) ||
    (is.list(x$workflow %||% NULL) && length(x$workflow %||% NULL)) ||
    .is_non_empty_scalar_character(.as_scalar_character(x$logs$run_dir %||% NULL))
}

.cluster_review_model_logs_dir <- function(review_file) {
  review_file <- .as_scalar_character(review_file)
  if (is.na(review_file) || !nzchar(review_file)) {
    return(NULL)
  }

  stem <- if (grepl("\\.[Rr]?[Mm][Dd]$", review_file)) {
    sub("\\.[Rr]?[Mm][Dd]$", "", review_file)
  } else {
    review_file
  }

  paste0(stem, "_model_logs")
}

.write_cluster_review_model_logs <- function(x, review_file) {
  if (!.cluster_review_has_model_trace(x)) {
    return(NULL)
  }

  target_dir <- .cluster_review_model_logs_dir(review_file)
  if (is.null(target_dir)) {
    return(NULL)
  }

  if (dir.exists(target_dir)) {
    unlink(target_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

  root_debug_run_dir <- .cluster_review_stage_debug_run_dir(x)
  if (.is_non_empty_scalar_character(root_debug_run_dir)) {
    .write_text_file(
      file.path(target_dir, "workflow_debug_run_dir.txt"),
      root_debug_run_dir
    )
    .copy_cluster_review_debug_snapshot(
      from_dir = root_debug_run_dir,
      to_dir = target_dir
    )
  }

  stage_specs <- .cluster_review_model_log_stage_specs(x)
  for (spec in stage_specs) {
    if (identical(spec$kind, "label_ladder")) {
      .write_cluster_review_label_ladder_logs(
        label_stage = spec$payload,
        stage_dir = file.path(target_dir, spec$dir_name),
        stage_name = spec$stage_name
      )
    } else {
      .write_cluster_review_single_stage_logs(
        stage = spec$payload,
        stage_dir = file.path(target_dir, spec$dir_name),
        stage_name = spec$stage_name
      )
    }
  }

  .write_cluster_review_json_artifact(
    file.path(target_dir, "manifest.json"),
    list(
      generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      review_file = review_file,
      cluster_id = .as_scalar_character(
        x$cluster_id %||% x$output$cluster_id %||% NULL
      ),
      provider = .as_scalar_character(x$provider %||% NULL),
      model = .as_scalar_character(x$model %||% NULL),
      variant = .as_scalar_character(x$variant %||% NULL),
      workflow_steps = as.integer(x$workflow_steps %||% NA_integer_),
      workflow_debug_run_dir = root_debug_run_dir,
      stages = lapply(stage_specs, function(spec) {
        list(
          stage_name = spec$stage_name,
          kind = spec$kind,
          dir = spec$dir_name
        )
      })
    )
  )

  normalizePath(target_dir, winslash = "/", mustWork = TRUE)
}

.cluster_review_model_log_stage_specs <- function(x) {
  workflow <- x$workflow %||% NULL

  if (is.list(workflow) &&
      (is.list(workflow$draft) || is.list(workflow$label))) {
    return(Filter(Negate(is.null), list(
      if (is.list(workflow$draft)) {
        list(
          kind = "stage",
          dir_name = "stage1_brainstorm",
          stage_name = "brainstorm",
          payload = workflow$draft
        )
      },
      if (is.list(workflow$label)) {
        list(
          kind = "label_ladder",
          dir_name = "stage2_label_decision",
          stage_name = "label_decision",
          payload = workflow$label
        )
      },
      if (is.list(workflow$summary)) {
        list(
          kind = "stage",
          dir_name = "stage3_label_summary",
          stage_name = "label_summary",
          payload = workflow$summary
        )
      },
      if (is.list(workflow$abstain_reason)) {
        list(
          kind = "stage",
          dir_name = "stage3_abstain_reason",
          stage_name = "abstain_reason",
          payload = workflow$abstain_reason
        )
      }
    )))
  }

  if (is.list(workflow) &&
      (is.list(workflow$gate) || is.list(workflow$label))) {
    return(Filter(Negate(is.null), list(
      if (is.list(workflow$gate)) {
        list(
          kind = "stage",
          dir_name = "stage1_gate",
          stage_name = "gate",
          payload = workflow$gate
        )
      },
      if (is.list(workflow$label)) {
        list(
          kind = "stage",
          dir_name = "stage2_label",
          stage_name = "label",
          payload = workflow$label
        )
      }
    )))
  }

  list(
    list(
      kind = "stage",
      dir_name = if (isTRUE(x$speculative$used %||% FALSE)) {
        "stage1_speculative_fallback"
      } else {
        "stage1_label"
      },
      stage_name = if (isTRUE(x$speculative$used %||% FALSE)) {
        "speculative_fallback"
      } else {
        "label"
      },
      payload = x
    )
  )
}

.write_cluster_review_label_ladder_logs <- function(label_stage, stage_dir, stage_name) {
  dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)

  debug_run_dir <- .cluster_review_stage_debug_run_dir(label_stage)
  if (.is_non_empty_scalar_character(debug_run_dir)) {
    .write_text_file(
      file.path(stage_dir, "debug_run_dir.txt"),
      debug_run_dir
    )
    .copy_cluster_review_debug_snapshot(
      from_dir = debug_run_dir,
      to_dir = stage_dir
    )
  }

  .write_cluster_review_json_artifact(
    file.path(stage_dir, "stage_info.json"),
    list(
      stage_name = stage_name,
      selected_public_variant = .as_scalar_character(
        label_stage$selected_public_variant %||% NULL
      ),
      selected_selection_variant = .as_scalar_character(
        label_stage$selected_selection_variant %||% NULL
      ),
      exhausted = isTRUE(label_stage$exhausted),
      failure_messages = label_stage$failure_messages %||% character(0),
      debug_run_dir = debug_run_dir
    )
  )

  attempts <- label_stage$attempts %||% list()
  if (!is.list(attempts) || !length(attempts)) {
    return(invisible(NULL))
  }

  for (i in seq_along(attempts)) {
    attempt <- attempts[[i]]
    attempt_variant <- .as_scalar_character(
      attempt$public_variant %||% attempt$variant %||% attempt$selection_variant %||% NULL
    )
    if (is.na(attempt_variant) || !nzchar(attempt_variant)) {
      attempt_variant <- paste0("attempt", i)
    }

    .write_cluster_review_single_stage_logs(
      stage = attempt,
      stage_dir = file.path(
        stage_dir,
        paste0("attempt", i, "_", .safe_file_stub(attempt_variant))
      ),
      stage_name = stage_name
    )
  }

  invisible(NULL)
}

.write_cluster_review_single_stage_logs <- function(stage, stage_dir, stage_name) {
  dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)

  debug_run_dir <- .cluster_review_stage_debug_run_dir(stage)
  if (.is_non_empty_scalar_character(debug_run_dir)) {
    .write_text_file(
      file.path(stage_dir, "debug_run_dir.txt"),
      debug_run_dir
    )
    .copy_cluster_review_debug_snapshot(
      from_dir = debug_run_dir,
      to_dir = stage_dir
    )
  }

  .write_cluster_review_json_artifact(
    file.path(stage_dir, "stage_info.json"),
    .cluster_review_stage_info(stage, stage_name)
  )

  .write_cluster_review_stage_snapshot_files(stage, stage_dir)
  invisible(NULL)
}

.cluster_review_stage_info <- function(stage, stage_name) {
  list(
    stage_name = stage_name,
    variant = .as_scalar_character(stage$variant %||% NULL),
    public_variant = .as_scalar_character(stage$public_variant %||% NULL),
    selection_variant = .as_scalar_character(stage$selection_variant %||% NULL),
    result = .as_scalar_character(stage$result %||% NULL),
    output_status = .cluster_review_stage_output_status(stage),
    skipped = isTRUE(stage$skipped),
    skip_reason = .as_scalar_character(stage$skip_reason %||% NULL),
    attempts = as.integer(stage$attempts %||% NA_integer_),
    retry_exhausted = isTRUE(stage$retry_exhausted),
    repair_source = .as_scalar_character(stage$repair_source %||% NULL),
    repair_variant = .as_scalar_character(stage$repair_variant %||% NULL),
    repair_history_entries = length(stage$repair_history %||% list()),
    repair_history_preview = .cluster_review_repair_history_preview(
      stage$repair_history %||% list()
    ),
    fallback_used = isTRUE(stage$fallback_used),
    fallback_reason = .as_scalar_character(stage$fallback_reason %||% NULL),
    error = .as_scalar_character(stage$error %||% NULL),
    debug_run_dir = .cluster_review_stage_debug_run_dir(stage)
  )
}

.cluster_review_repair_history_preview <- function(repair_history) {
  repair_history <- repair_history %||% list()
  if (!is.list(repair_history) || !length(repair_history)) {
    return(list())
  }

  lapply(repair_history, function(entry) {
    candidate <- entry$parsed_output_candidate %||% list()
    list(
      attempt = as.integer(entry$attempt %||% NA_integer_),
      error = .as_scalar_character(entry$error %||% NULL),
      response_content = .as_scalar_character(entry$response_content %||% NULL),
      candidate_display_label = .as_scalar_character(
        candidate$display_label %||% candidate$label_decision_text %||% NULL
      ),
      candidate_canonical_label = .as_scalar_character(
        candidate$canonical_label %||% NULL
      ),
      repair_instruction = .as_scalar_character(
        entry$repair_instruction %||% NULL
      )
    )
  })
}

.cluster_review_stage_output_status <- function(stage) {
  output <- stage$output %||% NULL
  .as_scalar_character(output$status %||% output$decision %||% NULL)
}

.cluster_review_stage_debug_run_dir <- function(stage) {
  path <- .as_scalar_character(stage$logs$run_dir %||% NULL)
  if (!.is_non_empty_scalar_character(path)) {
    return(NULL)
  }
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

.copy_cluster_review_debug_snapshot <- function(from_dir, to_dir) {
  from_dir <- .as_scalar_character(from_dir)
  to_dir <- .as_scalar_character(to_dir)

  if (is.na(from_dir) || !nzchar(from_dir) || !dir.exists(from_dir)) {
    return(invisible(FALSE))
  }
  if (is.na(to_dir) || !nzchar(to_dir)) {
    return(invisible(FALSE))
  }

  files <- list.files(
    from_dir,
    recursive = FALSE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  if (!length(files)) {
    return(invisible(FALSE))
  }

  info <- file.info(files)
  files <- files[!is.na(info$isdir) & !info$isdir]
  if (!length(files)) {
    return(invisible(FALSE))
  }

  keep_pattern <- paste0(
    "^(",
    paste(
      c(
        "metadata\\.json",
        "system_prompt\\.md",
        "user_prompt\\.md",
        "request(?:_attempt[0-9]+)?\\.json",
        "response_content(?:_attempt[0-9]+)?\\.txt",
        "response(?:_attempt[0-9]+_envelope)?\\.json",
        "parsed_output\\.json",
        "parsed_text_fields\\.json",
        "attempt_diagnostics_attempt[0-9]+\\.json",
        "error\\.txt"
      ),
      collapse = "|"
    ),
    ")$"
  )

  files <- files[grepl(keep_pattern, basename(files), perl = TRUE)]
  if (!length(files)) {
    return(invisible(FALSE))
  }

  dir.create(to_dir, recursive = TRUE, showWarnings = FALSE)
  copied <- file.copy(
    from = files,
    to = file.path(to_dir, basename(files)),
    overwrite = TRUE
  )
  invisible(all(copied))
}

.write_cluster_review_stage_snapshot_files <- function(stage, stage_dir) {
  prompt <- stage$prompt %||% NULL
  request <- stage$request %||% NULL
  response <- stage$response %||% NULL
  output <- stage$output %||% NULL

  if (is.list(prompt)) {
    .write_cluster_review_text_artifact_if_missing(
      file.path(stage_dir, "system_prompt.md"),
      .as_scalar_character(prompt$system %||% NULL)
    )
    .write_cluster_review_text_artifact_if_missing(
      file.path(stage_dir, "user_prompt.md"),
      .as_scalar_character(prompt$user %||% NULL)
    )
  }

  if (is.list(request) && !file.exists(file.path(stage_dir, "request.json"))) {
    .write_cluster_review_json_artifact(
      file.path(stage_dir, "request.json"),
      request
    )
  }

  if (is.list(response)) {
    .write_cluster_review_text_artifact_if_missing(
      file.path(stage_dir, "response_content.txt"),
      .as_scalar_character(response$content %||% NULL)
    )

    response_raw <- .as_scalar_character(response$raw %||% NULL)
    response_envelope_path <- file.path(stage_dir, "response_envelope.json")
    if (!file.exists(response_envelope_path)) {
      if (.is_non_empty_scalar_character(response_raw)) {
        .write_text_file(response_envelope_path, response_raw)
      } else if (is.list(response$envelope)) {
        .write_cluster_review_json_artifact(
          response_envelope_path,
          response$envelope
        )
      }
    }
  }

  if (is.list(output) && !file.exists(file.path(stage_dir, "parsed_output.json"))) {
    .write_cluster_review_json_artifact(
      file.path(stage_dir, "parsed_output.json"),
      output
    )
  }

  error_text <- .as_scalar_character(stage$error %||% NULL)
  if (.is_non_empty_scalar_character(error_text) &&
      !file.exists(file.path(stage_dir, "error.txt"))) {
    .write_text_file(file.path(stage_dir, "error.txt"), error_text)
  }

  .write_cluster_review_repair_history_artifacts(stage, stage_dir)
}

.write_cluster_review_repair_history_artifacts <- function(stage, stage_dir) {
  repair_history <- stage$repair_history %||% list()
  if (!is.list(repair_history) || !length(repair_history)) {
    return(invisible(NULL))
  }

  .write_cluster_review_json_artifact(
    file.path(stage_dir, "repair_history.json"),
    .cluster_review_repair_history_preview(repair_history)
  )

  for (i in seq_along(repair_history)) {
    entry <- repair_history[[i]]
    attempt_index <- as.integer(entry$attempt %||% i)
    if (is.na(attempt_index) || attempt_index < 1L) {
      attempt_index <- i
    }

    request_path <- file.path(stage_dir, paste0("request_attempt", attempt_index, ".json"))
    if (is.list(entry$request) && !file.exists(request_path)) {
      .write_cluster_review_json_artifact(request_path, entry$request)
    }

    response_content_path <- file.path(
      stage_dir,
      paste0("response_content_attempt", attempt_index, ".txt")
    )
    .write_cluster_review_text_artifact_if_missing(
      response_content_path,
      .as_scalar_character(entry$response_content %||% NULL)
    )

    response_envelope_path <- file.path(
      stage_dir,
      paste0("response_attempt", attempt_index, "_envelope.json")
    )
    response_raw <- .as_scalar_character(entry$response_raw %||% NULL)
    if (.is_non_empty_scalar_character(response_raw) &&
        !file.exists(response_envelope_path)) {
      .write_text_file(response_envelope_path, response_raw)
    }

    parsed_candidate <- entry$parsed_output_candidate %||% NULL
    parsed_candidate_path <- file.path(
      stage_dir,
      paste0("parsed_output_candidate_attempt", attempt_index, ".json")
    )
    if (is.list(parsed_candidate) && !file.exists(parsed_candidate_path)) {
      .write_cluster_review_json_artifact(
        parsed_candidate_path,
        parsed_candidate
      )
    }
  }

  invisible(NULL)
}

.write_cluster_review_json_artifact <- function(path, x) {
  .write_text_file(
    path,
    jsonlite::toJSON(
      x,
      auto_unbox = TRUE,
      null = "null",
      pretty = TRUE
    )
  )
}

.write_cluster_review_text_artifact_if_missing <- function(path, text) {
  text <- .as_scalar_character(text)
  if (!.is_non_empty_scalar_character(text) || file.exists(path)) {
    return(invisible(NULL))
  }

  .write_text_file(path, text)
  invisible(NULL)
}

.md_code <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x) || !nzchar(x)) {
    return("`<missing>`")
  }
  paste0("`", x, "`")
}

.empty_string_if_na <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x)) {
    return("")
  }
  x
}

.md_text_block_lines <- function(x) {
  x <- .as_scalar_character(x)
  if (is.na(x) || !nzchar(x)) {
    return("Not provided.")
  }

  c("````text", strsplit(x, "\n", fixed = TRUE)[[1]], "````")
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
