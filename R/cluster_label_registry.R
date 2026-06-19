#' Build a flat cluster-label registry from `label_clusters()` results
#'
#' @description
#' Converts the high-level batch object returned by [label_clusters()] into a
#' one-row-per-cluster registry table. The registry is intended as a bridge
#' between the evidence-first LLM workflow and downstream plotting or reporting:
#' it preserves stable Cocktail cluster IDs while exposing human-readable labels,
#' review status, prompt provenance, and artifact paths in scalar columns.
#'
#' The default visual convention supported by this registry is:
#' \itemize{
#'   \item keep `cluster` / `plot_label_id` as the on-plot identifier,
#'   \item use `display_label` or `plot_label_short` when a short label is needed,
#'   \item use `legend_label` in legends or figure captions.
#' }
#'
#' @param x An object returned by [label_clusters()].
#'
#' @return A data frame of class `cluster_label_registry` with one row per
#'   processed cluster. Important columns include:
#'   \describe{
#'     \item{cluster}{Cluster ID such as `"c_12"`.}
#'     \item{plot_label_id}{Stable ID recommended for direct plot annotation.}
#'     \item{plot_label_short}{Human-readable short label when available, otherwise the cluster ID.}
#'     \item{legend_label}{Preformatted `cluster: label` text for legends or captions.}
#'     \item{display_label, canonical_label}{Structured labels returned by the LLM.}
#'     \item{label_available}{Whether a non-empty labeled output is available.}
#'     \item{accepted_label}{Whether a labeled output is available and the review status is `"accepted"`.}
#'     \item{review_status, validation_status, output_status}{Review and validation summaries.}
#'     \item{review_file, review_metadata_file}{Saved review-card artifact paths, when present.}
#'     \item{model, variant, workflow_steps}{Prompt / model provenance.}
#'     \item{dataset_type, dataset_label, dataset_path}{Dataset provenance carried through evidence extraction.}
#'   }
#'
#' @examples
#' \dontrun{
#' run <- label_clusters(
#'   x = res,
#'   clusters = c("c_12", "c_27"),
#'   model = "gemma4:12b",
#'   variant = "strict_abstention_gate_v1"
#' )
#'
#' reg <- cluster_label_registry(run)
#' reg[, c("cluster", "display_label", "legend_label", "review_file")]
#' }
#'
#' @export
cluster_label_registry <- function(x) {
  if (!inherits(x, "cluster_label_batch_result")) {
    stop("`x` must be an object returned by `label_clusters()`.", call. = FALSE)
  }

  results <- x$results %||% list()
  if (!length(results)) {
    out <- .empty_cluster_label_registry()
    class(out) <- c("cluster_label_registry", class(out))
    return(out)
  }

  summary_tbl <- x$summary %||% data.frame(stringsAsFactors = FALSE)
  rows <- lapply(seq_along(results), function(i) {
    .cluster_label_registry_row(
      cluster_run = results[[i]],
      summary_tbl = summary_tbl,
      selection_rank = i
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  class(out) <- c("cluster_label_registry", class(out))
  out
}

.empty_cluster_label_registry <- function() {
  data.frame(
    cluster = character(),
    cluster_numeric_id = integer(),
    selection_rank = integer(),
    h = numeric(),
    k = numeric(),
    m = numeric(),
    score = numeric(),
    plot_label_id = character(),
    plot_label_short = character(),
    legend_label = character(),
    display_label = character(),
    canonical_label = character(),
    label_available = logical(),
    accepted_label = logical(),
    output_status = character(),
    review_status = character(),
    validation_status = character(),
    run_status = character(),
    needs_human_review = logical(),
    is_valid = logical(),
    used_placeholder = logical(),
    repair_used = logical(),
    iterations_used = integer(),
    num_predict_used = integer(),
    confidence_score = numeric(),
    confidence_rationale = character(),
    interpretation_summary = character(),
    abstain_reason = character(),
    key_species_text = character(),
    basis_claim_count = integer(),
    evidence_coverage_score = numeric(),
    evidence_citation_valid = integer(),
    evidence_citation_total = integer(),
    provider = character(),
    model = character(),
    variant = character(),
    workflow_steps = integer(),
    gate_variant = character(),
    gate_decision = character(),
    prompt_catalog_path = character(),
    prompt_schema_path = character(),
    prompt_system_path = character(),
    prompt_user_path = character(),
    gate_prompt_system_path = character(),
    gate_prompt_user_path = character(),
    dataset_type = character(),
    dataset_label = character(),
    dataset_path = character(),
    dataset_folder = character(),
    log_run_dir = character(),
    review_file = character(),
    review_metadata_file = character(),
    stringsAsFactors = FALSE
  )
}

.cluster_label_registry_row <- function(cluster_run, summary_tbl, selection_rank) {
  evidence <- cluster_run$evidence %||% NULL
  validation <- cluster_run$validation %||% NULL
  llm_result <- cluster_run$llm_result %||% NULL
  review <- cluster_run$review %||% list()

  if (is.null(evidence) || !inherits(evidence, "cluster_evidence")) {
    stop("Each `label_clusters()` result must contain a `cluster_evidence` object.",
      call. = FALSE
    )
  }
  if (is.null(validation) || !inherits(validation, "cluster_label_validation")) {
    stop("Each `label_clusters()` result must contain a `cluster_label_validation` object.",
      call. = FALSE
    )
  }

  output <- validation$output %||% .extract_cluster_label_output(llm_result)
  if (!is.list(output) || is.null(names(output))) {
    stop("Each `label_clusters()` result must contain a structured label output.",
      call. = FALSE
    )
  }

  cluster_id <- .null_default(
    validation$cluster_id,
    .null_default(.cluster_evidence_cluster_id(evidence), .as_scalar_character(output$cluster_id))
  )
  summary_row <- .cluster_label_registry_summary_row(summary_tbl, cluster_id)
  coverage <- validation$evidence_coverage %||% list(
    score = NA_real_,
    n_valid_citations = NA_integer_,
    n_citation_targets = NA_integer_
  )
  provenance <- .cluster_review_provenance(llm_result)
  dataset <- .cluster_review_dataset_info(evidence)

  display_label <- .as_scalar_character(output$display_label)
  canonical_label <- .as_scalar_character(output$canonical_label)
  output_status <- .null_default(validation$output_status, .as_scalar_character(output$status))
  review_status <- .cluster_review_status(validation)
  used_placeholder <- isTRUE(cluster_run$used_placeholder)
  label_available <- identical(output_status, "labeled") &&
    .is_non_empty_scalar_character(display_label) &&
    !used_placeholder
  accepted_label <- label_available && identical(review_status, "accepted")

  confidence <- output$confidence %||% list()
  basis_in_data <- output$basis_in_data %||% list()

  data.frame(
    cluster = cluster_id,
    cluster_numeric_id = .cluster_label_registry_numeric_id(cluster_id),
    selection_rank = as.integer(selection_rank),
    h = .cluster_label_registry_numeric(summary_row$h),
    k = .cluster_label_registry_integer(summary_row$k),
    m = .cluster_label_registry_integer(summary_row$m),
    score = .cluster_label_registry_numeric(summary_row$score),
    plot_label_id = cluster_id,
    plot_label_short = if (label_available) display_label else cluster_id,
    legend_label = .cluster_label_registry_legend_label(
      cluster_id = cluster_id,
      display_label = display_label,
      output_status = output_status,
      used_placeholder = used_placeholder
    ),
    display_label = .cluster_label_registry_character(display_label),
    canonical_label = .cluster_label_registry_character(canonical_label),
    label_available = label_available,
    accepted_label = accepted_label,
    output_status = .cluster_label_registry_character(output_status),
    review_status = .cluster_label_registry_character(review_status),
    validation_status = .cluster_label_registry_character(validation$validation_status),
    run_status = .cluster_label_registry_character(cluster_run$run_status),
    needs_human_review = isTRUE(validation$needs_human_review),
    is_valid = isTRUE(validation$is_valid),
    used_placeholder = used_placeholder,
    repair_used = isTRUE(cluster_run$repair_used),
    iterations_used = .cluster_label_registry_integer(cluster_run$iterations_used),
    num_predict_used = .cluster_label_registry_integer(cluster_run$num_predict_used),
    confidence_score = .cluster_label_registry_numeric(confidence$score),
    confidence_rationale = .cluster_label_registry_character(
      .as_scalar_character(confidence$rationale)
    ),
    interpretation_summary = .cluster_label_registry_character(
      .as_scalar_character(output$interpretation_summary)
    ),
    abstain_reason = .cluster_label_registry_character(
      .as_scalar_character(output$abstain_reason)
    ),
    key_species_text = .cluster_label_registry_key_species_text(output$key_species),
    basis_claim_count = .cluster_label_registry_integer(length(basis_in_data)),
    evidence_coverage_score = .cluster_label_registry_numeric(coverage$score),
    evidence_citation_valid = .cluster_label_registry_integer(coverage$n_valid_citations),
    evidence_citation_total = .cluster_label_registry_integer(coverage$n_citation_targets),
    provider = .cluster_label_registry_character(provenance$provider),
    model = .cluster_label_registry_character(provenance$model),
    variant = .cluster_label_registry_character(provenance$variant),
    workflow_steps = .cluster_label_registry_integer(provenance$workflow_steps),
    gate_variant = .cluster_label_registry_character(provenance$gate_variant),
    gate_decision = .cluster_label_registry_character(provenance$gate_decision),
    prompt_catalog_path = .cluster_label_registry_character(provenance$prompt_catalog_path),
    prompt_schema_path = .cluster_label_registry_character(provenance$prompt_schema_path),
    prompt_system_path = .cluster_label_registry_character(provenance$prompt_system_path),
    prompt_user_path = .cluster_label_registry_character(provenance$prompt_user_path),
    gate_prompt_system_path = .cluster_label_registry_character(provenance$gate_prompt_system_path),
    gate_prompt_user_path = .cluster_label_registry_character(provenance$gate_prompt_user_path),
    dataset_type = .cluster_label_registry_character(dataset$type),
    dataset_label = .cluster_label_registry_character(dataset$label),
    dataset_path = .cluster_label_registry_character(dataset$path),
    dataset_folder = .cluster_label_registry_character(dataset$folder_slug),
    log_run_dir = .cluster_label_registry_character(provenance$log_run_dir),
    review_file = .cluster_label_registry_character(review$file %||% summary_row$review_file),
    review_metadata_file = .cluster_label_registry_character(review$metadata_file),
    stringsAsFactors = FALSE
  )
}

.cluster_label_registry_summary_row <- function(summary_tbl, cluster_id) {
  if (!is.data.frame(summary_tbl) || !nrow(summary_tbl) || !"cluster" %in% names(summary_tbl)) {
    return(data.frame(
      cluster = cluster_id,
      h = NA_real_,
      k = NA_integer_,
      m = NA_integer_,
      score = NA_real_,
      review_file = NA_character_,
      stringsAsFactors = FALSE
    ))
  }

  idx <- match(cluster_id, summary_tbl$cluster)
  if (is.na(idx)) {
    return(data.frame(
      cluster = cluster_id,
      h = NA_real_,
      k = NA_integer_,
      m = NA_integer_,
      score = NA_real_,
      review_file = NA_character_,
      stringsAsFactors = FALSE
    ))
  }

  summary_tbl[idx, , drop = FALSE]
}

.cluster_label_registry_numeric_id <- function(cluster_id) {
  cluster_id <- .as_scalar_character(cluster_id)
  if (is.na(cluster_id) || !grepl("^c_[0-9]+$", cluster_id)) {
    return(NA_integer_)
  }
  as.integer(sub("^c_", "", cluster_id))
}

.cluster_label_registry_character <- function(x) {
  x <- .as_scalar_character(x)
  if (is.na(x) || !nzchar(x)) {
    return(NA_character_)
  }
  x
}

.cluster_label_registry_integer <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    return(NA_integer_)
  }
  as.integer(x)
}

.cluster_label_registry_numeric <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    return(NA_real_)
  }
  as.numeric(x)
}

.cluster_label_registry_legend_label <- function(
    cluster_id,
    display_label,
    output_status,
    used_placeholder
) {
  if (.is_non_empty_scalar_character(display_label) && identical(output_status, "labeled") &&
      !isTRUE(used_placeholder)) {
    return(paste0(cluster_id, ": ", display_label))
  }

  if (identical(output_status, "abstain")) {
    return(paste0(cluster_id, ": [abstained]"))
  }

  if (isTRUE(used_placeholder)) {
    return(paste0(cluster_id, ": [no valid label]"))
  }

  cluster_id
}

.cluster_label_registry_key_species_text <- function(key_species) {
  if (!is.list(key_species) || !length(key_species)) {
    return(NA_character_)
  }

  species <- unique(vapply(key_species, function(x) {
    .cluster_label_registry_character(x$species)
  }, character(1)))
  species <- species[!is.na(species) & nzchar(species)]

  if (!length(species)) {
    return(NA_character_)
  }

  paste(species, collapse = "; ")
}
