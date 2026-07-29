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
#'   \item use `display_label` as the full stored human label,
#'   \item use `plot_label_short` when a short plot preview is needed,
#'   \item use `legend_label` in legends or figure captions,
#'   \item use `hclust_label_compact` for dense `hclust` leaf labeling.
#' }
#'
#' @param x An object returned by [label_clusters()].
#'
#' @return A data frame of class `cluster_label_registry` with one row per
#'   processed cluster. Important columns include:
#'   \describe{
#'     \item{cluster}{Cluster ID such as `"c_12"`.}
#'     \item{plot_label_id}{Stable ID recommended for direct plot annotation.}
#'     \item{plot_label_short}{Deterministic plot-preview label: the full label
#'       when it already fits within three words, otherwise the first three
#'       whitespace-delimited words plus literal `" ..."`; falls back to the
#'       cluster ID when no label is available.}
#'     \item{legend_label}{Preformatted `cluster: preview` text for legends or
#'       captions.}
#'     \item{hclust_label_compact}{Compact `cluster: preview` text for dense
#'       `hclust` leaf labels.}
#'     \item{display_label, canonical_label}{Structured labels returned by the
#'       LLM: `display_label` stores the full human label, while
#'       `canonical_label` stores the short/projected programmatic form.}
#'     \item{public_display_label, public_canonical_label, public_label_source}{Downstream-only public label projection, including the post-abstain fallback when applicable.}
#'     \item{label_available}{Whether a non-empty labeled output is available.}
#'     \item{accepted_label}{Whether a labeled output is available and the review status is `"accepted"`.}
#'     \item{review_status, validation_status, output_status}{Review and validation summaries.}
#'     \item{review_file, review_metadata_file}{Saved review-card artifact paths, when present.}
#'     \item{model, variant, workflow_steps}{Prompt / model provenance.}
#'     \item{selected_label_variant, label_stage_exhausted}{Resolved stage-B rung
#'       and whether the public selection cascade was fully exhausted.}
#'     \item{use_double_brainstorm, double_brainstorm_enabled, draft_status,
#'       draft_evidence_status}{Experimental double-brainstorm strategy
#'       metadata when present.}
#'     \item{dataset_type, dataset_label, dataset_path}{Dataset provenance carried through evidence extraction.}
#'   }
#'
#' @examples
#' \dontrun{
#' run <- label_clusters(
#'   x = res,
#'   clusters = c("c_12", "c_27"),
#'   model = "gemma4:12b",
#'   variant = "label_primary_v1"
#' )
#'
#' reg <- cluster_label_registry(run)
#' reg[, c("cluster", "display_label", "legend_label", "hclust_label_compact", "review_file")]
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

.cluster_label_registry_basename <- function() {
  "cluster_label_registry.csv"
}

.cluster_label_registry_storage_dir <- function(label_registry, results, review_dir) {
  review_files <- label_registry$review_file %||% character(0)
  review_files <- review_files[!is.na(review_files) & nzchar(review_files)]

  if (length(review_files)) {
    dirs <- dirname(review_files)
    dir_tab <- sort(table(dirs), decreasing = TRUE)
    if (length(dir_tab)) {
      return(names(dir_tab)[[1]])
    }
  }

  root_dir <- .resolve_cocktailr_output_path(review_dir)
  if (!length(results)) {
    return(root_dir)
  }

  evidence <- results[[1]]$evidence %||% NULL
  if (inherits(evidence, "cluster_evidence")) {
    dataset <- .cluster_review_dataset_info(evidence)
    if (!is.na(dataset$folder_slug) && nzchar(dataset$folder_slug)) {
      return(file.path(root_dir, dataset$folder_slug))
    }
  }

  root_dir
}

.write_cluster_label_registry_file <- function(label_registry, results, review_dir) {
  if (!inherits(label_registry, "cluster_label_registry")) {
    stop("`label_registry` must inherit from `cluster_label_registry`.", call. = FALSE)
  }

  target_dir <- .cluster_label_registry_storage_dir(
    label_registry = label_registry,
    results = results,
    review_dir = review_dir
  )
  if (is.null(target_dir) || is.na(target_dir) || !nzchar(target_dir)) {
    stop("Could not determine where to save the cluster label registry.", call. = FALSE)
  }

  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  file <- file.path(target_dir, .cluster_label_registry_basename())
  utils::write.csv(label_registry, file = file, row.names = FALSE, na = "NA")
  normalizePath(file, winslash = "/", mustWork = TRUE)
}

.attach_cluster_label_registry_file <- function(label_registry, file) {
  if (!inherits(label_registry, "cluster_label_registry")) {
    return(label_registry)
  }

  attr(label_registry, "file") <- .cluster_label_registry_character(file)
  label_registry
}

.read_cluster_label_registry_file <- function(file) {
  file <- .as_scalar_character(file)
  if (is.na(file) || !nzchar(file) || !file.exists(file)) {
    stop("`file` must point to an existing cluster label registry CSV.", call. = FALSE)
  }

  reg <- utils::read.csv(
    file,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = "NA"
  )
  reg <- .cluster_label_registry_add_hclust_label_compact(reg)
  class(reg) <- c("cluster_label_registry", "data.frame")
  attr(reg, "file") <- normalizePath(file, winslash = "/", mustWork = TRUE)
  reg
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
    hclust_label_compact = character(),
    display_label = character(),
    canonical_label = character(),
    category_label = character(),
    subcategory_labels = character(),
    public_display_label = character(),
    public_canonical_label = character(),
    public_label_source = character(),
    label_available = logical(),
    accepted_label = logical(),
    label_tier = character(),
    is_speculative = logical(),
    plot_marker = character(),
    label_origin = character(),
    species_entropy_band = character(),
    species_entropy_text = character(),
    chaoticity_score = integer(),
    chaoticity_label = character(),
    output_status = character(),
    review_status = character(),
    validation_status = character(),
    strict_outcome = character(),
    strict_validation_status = character(),
    run_status = character(),
    needs_human_review = logical(),
    is_valid = logical(),
    used_placeholder = logical(),
    repair_used = logical(),
    iterations_used = integer(),
    num_predict_used = integer(),
    confidence_score = numeric(),
    confidence_rationale = character(),
    missing_for_confidence_text = character(),
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
    selected_label_variant = character(),
    label_stage_exhausted = logical(),
    label_stage_failure_reason = character(),
    use_double_brainstorm = logical(),
    double_brainstorm_enabled = logical(),
    draft_status = character(),
    draft_evidence_status = character(),
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
  llm_metadata <- llm_result$metadata %||% list()
  dataset <- .cluster_review_dataset_info(evidence)

  display_label <- .as_scalar_character(output$display_label)
  canonical_label <- .as_scalar_character(output$canonical_label)
  category_label <- .as_scalar_character(output$category_label)
  subcategory_labels <- .cluster_label_subcategory_labels_text(output$subcategory_labels)
  public_label <- .cluster_label_public_fields(output, provenance)
  public_display_label <- .as_scalar_character(public_label$public_display_label)
  public_canonical_label <- .as_scalar_character(public_label$public_canonical_label)
  public_label_source <- .as_scalar_character(public_label$public_label_source)
  output_status <- .null_default(validation$output_status, .as_scalar_character(output$status))
  review_status <- .cluster_review_status(validation)
  label_tier <- .cluster_label_validation_label_tier(validation)
  is_speculative <- isTRUE(validation$is_speculative)
  plot_marker <- .cluster_label_validation_plot_marker(validation)
  label_origin <- .as_scalar_character(validation$label_origin)
  used_placeholder <- isTRUE(cluster_run$used_placeholder)
  label_available <- identical(output_status, "labeled") &&
    .is_non_empty_scalar_character(display_label) &&
    !used_placeholder
  accepted_label <- label_available && identical(review_status, "accepted")
  display_label_plot <- .cluster_label_display_with_marker(display_label, plot_marker)
  display_label_preview <- .cluster_label_registry_plot_preview(display_label_plot)
  public_display_label_plot <- .cluster_label_display_with_marker(
    public_display_label,
    if (identical(public_label_source, "model_output")) plot_marker else ""
  )
  public_display_label_preview <- .cluster_label_registry_plot_preview(
    public_display_label_plot
  )

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
    plot_label_short = if (label_available) {
      display_label_preview
    } else if (.is_non_empty_scalar_character(public_display_label_preview) &&
        identical(public_label_source, "post_abstain_fallback")) {
      public_display_label_preview
    } else {
      cluster_id
    },
    legend_label = .cluster_label_registry_legend_label(
      cluster_id = cluster_id,
      display_label = display_label_preview,
      output_status = output_status,
      used_placeholder = used_placeholder,
      public_display_label = public_display_label_preview,
      public_label_source = public_label_source
    ),
    hclust_label_compact = .cluster_label_registry_hclust_label(
      cluster_id = cluster_id,
      display_label = display_label_preview,
      output_status = output_status,
      used_placeholder = used_placeholder,
      public_display_label = public_display_label_preview,
      public_label_source = public_label_source,
      plot_marker = plot_marker
    ),
    display_label = .cluster_label_registry_character(display_label),
    canonical_label = .cluster_label_registry_character(canonical_label),
    category_label = .cluster_label_registry_character(category_label),
    subcategory_labels = .cluster_label_registry_character(subcategory_labels),
    public_display_label = .cluster_label_registry_character(public_display_label),
    public_canonical_label = .cluster_label_registry_character(public_canonical_label),
    public_label_source = .cluster_label_registry_character(public_label_source),
    label_available = label_available,
    accepted_label = accepted_label,
    label_tier = .cluster_label_registry_character(label_tier),
    is_speculative = is_speculative,
    plot_marker = .cluster_label_registry_character(plot_marker),
    label_origin = .cluster_label_registry_character(label_origin),
    species_entropy_band = .cluster_label_registry_character(validation$species_entropy_band),
    species_entropy_text = .cluster_label_registry_character(validation$species_entropy_text),
    chaoticity_score = .cluster_label_registry_integer(validation$chaoticity_score),
    chaoticity_label = .cluster_label_registry_character(validation$chaoticity_label),
    output_status = .cluster_label_registry_character(output_status),
    review_status = .cluster_label_registry_character(review_status),
    validation_status = .cluster_label_registry_character(validation$validation_status),
    strict_outcome = .cluster_label_registry_character(validation$strict_outcome),
    strict_validation_status = .cluster_label_registry_character(validation$strict_validation_status),
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
    missing_for_confidence_text = .cluster_label_registry_character(
      validation$missing_for_confidence_text %||%
        .cluster_label_missing_for_confidence_from_output(output)
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
    selected_label_variant = .cluster_label_registry_character(
      provenance$selected_label_variant
    ),
    label_stage_exhausted = isTRUE(provenance$label_stage_exhausted),
    label_stage_failure_reason = .cluster_label_registry_character(
      provenance$label_stage_failure_reason
    ),
    use_double_brainstorm = isTRUE(llm_metadata$use_double_brainstorm),
    double_brainstorm_enabled = isTRUE(llm_metadata$double_brainstorm_enabled),
    draft_status = .cluster_label_registry_character(llm_metadata$draft_status),
    draft_evidence_status = .cluster_label_registry_character(llm_metadata$draft_evidence_status),
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

.cluster_label_registry_plot_preview_word_limit <- function() {
  3L
}

.cluster_label_registry_plot_preview <- function(
    label,
    word_limit = .cluster_label_registry_plot_preview_word_limit(),
    ellipsis = " ..."
) {
  label <- .as_scalar_character(label)
  if (is.na(label) || !nzchar(label)) {
    return(NA_character_)
  }

  word_limit <- suppressWarnings(as.integer(word_limit))
  if (is.na(word_limit) || word_limit < 1L) {
    return(label)
  }

  marker_suffix <- ""
  if (grepl("\\*$", label, perl = TRUE)) {
    marker_suffix <- "*"
    label <- sub("\\*$", "", label, perl = TRUE)
    label <- trimws(label)
  }

  words <- strsplit(label, "[[:space:]]+", perl = TRUE)[[1]]
  words <- words[nzchar(words)]
  if (length(words) <= word_limit) {
    return(paste0(label, marker_suffix))
  }

  paste0(
    paste(words[seq_len(word_limit)], collapse = " "),
    ellipsis,
    marker_suffix
  )
}

.cluster_label_registry_legend_label <- function(
    cluster_id,
    display_label,
    output_status,
    used_placeholder,
    public_display_label = NULL,
    public_label_source = NULL
) {
  if (.is_non_empty_scalar_character(display_label) && identical(output_status, "labeled") &&
      !isTRUE(used_placeholder)) {
    return(paste0(cluster_id, ": ", display_label))
  }

  if (identical(output_status, "abstain") &&
      .is_non_empty_scalar_character(public_display_label) &&
      identical(public_label_source, "post_abstain_fallback")) {
    return(paste0(cluster_id, ": ", public_display_label))
  }

  if (identical(output_status, "abstain")) {
    return(paste0(cluster_id, ": [abstained]"))
  }

  if (isTRUE(used_placeholder)) {
    return(paste0(cluster_id, ": [no valid label]"))
  }

  cluster_id
}

.cluster_label_registry_hclust_label_max_chars <- function() {
  52L
}

.cluster_label_registry_hclust_label <- function(
    cluster_id,
    display_label,
    output_status,
    used_placeholder,
    public_display_label = NULL,
    public_label_source = NULL,
    plot_marker = NULL,
    max_chars = .cluster_label_registry_hclust_label_max_chars()
) {
  cluster_id <- .as_scalar_character(cluster_id)
  if (is.na(cluster_id) || !nzchar(cluster_id)) {
    return(NA_character_)
  }

  label_text <- .cluster_label_registry_hclust_label_text(
    display_label = display_label,
    output_status = output_status,
    used_placeholder = used_placeholder,
    public_display_label = public_display_label,
    public_label_source = public_label_source
  )
  if (is.na(label_text) || !nzchar(label_text)) {
    label_text <- "[unlabeled]"
  }

  prefix <- paste0(cluster_id, ": ")
  body_budget <- as.integer(max_chars) - nchar(prefix, type = "chars")
  if (!is.finite(body_budget) || body_budget <= 0L) {
    return(prefix)
  }

  paste0(
    prefix,
    .cluster_label_registry_truncate_hclust_text(
      text = label_text,
      max_chars = body_budget,
      plot_marker = plot_marker
    )
  )
}

.cluster_label_registry_hclust_label_text <- function(
    display_label,
    output_status,
    used_placeholder,
    public_display_label = NULL,
    public_label_source = NULL
) {
  if (.is_non_empty_scalar_character(display_label) &&
      identical(output_status, "labeled") &&
      !isTRUE(used_placeholder)) {
    return(display_label)
  }

  if (identical(output_status, "abstain") &&
      .is_non_empty_scalar_character(public_display_label) &&
      identical(public_label_source, "post_abstain_fallback")) {
    return(public_display_label)
  }

  if (identical(output_status, "abstain")) {
    return("[abstained]")
  }

  if (isTRUE(used_placeholder)) {
    return("[no valid label]")
  }

  "[unlabeled]"
}

.cluster_label_registry_hclust_generic_terms_pattern <- function() {
  paste(
    c(
      "cluster",
      "community",
      "assemblage",
      "association",
      "alliance",
      "interface",
      "complex",
      "ensemble",
      "mosaic"
    ),
    collapse = "|"
  )
}

.cluster_label_registry_simplify_hclust_text <- function(text) {
  text <- .as_scalar_character(text)
  if (is.na(text) || !nzchar(text)) {
    return(text)
  }

  marker_suffix <- ""
  if (grepl("\\*$", text, perl = TRUE)) {
    marker_suffix <- "*"
    text <- sub("\\*$", "", text, perl = TRUE)
  }

  simplified <- gsub(
    paste0("\\b(", .cluster_label_registry_hclust_generic_terms_pattern(), ")\\b"),
    "",
    text,
    ignore.case = TRUE,
    perl = TRUE
  )
  simplified <- gsub("[[:space:]]+", " ", simplified, perl = TRUE)
  simplified <- gsub("[[:space:]]+([,;:/)])", "\\1", simplified, perl = TRUE)
  simplified <- gsub("([(])\\s+", "\\1", simplified, perl = TRUE)
  simplified <- gsub("([-/])\\s+|\\s+([-/])", "\\1\\2", simplified, perl = TRUE)
  simplified <- gsub("[[:space:][:punct:]]+$", "", simplified, perl = TRUE)
  simplified <- trimws(simplified)

  if (!nzchar(simplified)) {
    simplified <- trimws(text)
  }

  paste0(simplified, marker_suffix)
}

.cluster_label_registry_truncate_hclust_text <- function(
    text,
    max_chars,
    plot_marker = NULL,
    ellipsis = "..."
) {
  text <- .as_scalar_character(text)
  if (is.na(text) || !nzchar(text)) {
    return("")
  }

  max_chars <- as.integer(max_chars)
  if (!is.finite(max_chars) || max_chars <= 0L) {
    return("")
  }

  if (nchar(text, type = "chars") <= max_chars) {
    return(text)
  }

  plot_marker <- .as_scalar_character(plot_marker)
  preserve_marker <- !is.na(plot_marker) &&
    nzchar(plot_marker) &&
    endsWith(text, plot_marker)

  text_core <- text
  marker_suffix <- ""
  if (preserve_marker) {
    marker_chars <- nchar(plot_marker, type = "chars")
    text_chars <- nchar(text, type = "chars")
    text_core <- substr(text, 1L, text_chars - marker_chars)
    marker_suffix <- plot_marker
  }

  reserved_chars <- nchar(ellipsis, type = "chars") +
    nchar(marker_suffix, type = "chars")
  if (max_chars <= reserved_chars) {
    return(substr(text, 1L, max_chars))
  }

  keep_chars <- max_chars - reserved_chars
  kept <- substr(text_core, 1L, keep_chars)
  kept <- sub("[[:space:]]+$", "", kept, perl = TRUE)
  paste0(kept, ellipsis, marker_suffix)
}

.cluster_label_registry_add_hclust_label_compact <- function(reg) {
  if (!is.data.frame(reg)) {
    return(reg)
  }

  can_rebuild <- "cluster" %in% names(reg) &&
    "output_status" %in% names(reg) &&
    any(c(
      "display_label",
      "public_display_label",
      "public_label_source",
      "used_placeholder",
      "plot_marker"
    ) %in% names(reg))

  if ("hclust_label_compact" %in% names(reg) && !can_rebuild) {
    return(reg)
  }

  n <- nrow(reg)
  if (!n) {
    reg$hclust_label_compact <- character(0)
    return(reg)
  }

  display_label <- if ("display_label" %in% names(reg)) {
    reg$display_label
  } else {
    rep(NA_character_, n)
  }
  public_display_label <- if ("public_display_label" %in% names(reg)) {
    reg$public_display_label
  } else {
    rep(NA_character_, n)
  }
  public_label_source <- if ("public_label_source" %in% names(reg)) {
    reg$public_label_source
  } else {
    rep(NA_character_, n)
  }
  output_status <- if ("output_status" %in% names(reg)) {
    reg$output_status
  } else {
    rep(NA_character_, n)
  }
  used_placeholder <- if ("used_placeholder" %in% names(reg)) {
    reg$used_placeholder
  } else {
    rep(FALSE, n)
  }
  plot_marker <- if ("plot_marker" %in% names(reg)) {
    reg$plot_marker
  } else {
    rep(NA_character_, n)
  }

  reg$hclust_label_compact <- vapply(seq_len(n), function(i) {
    display_label_plot <- .cluster_label_display_with_marker(
      display_label[[i]],
      plot_marker[[i]]
    )
    display_label_preview <- .cluster_label_registry_plot_preview(display_label_plot)
    public_display_label_plot <- .cluster_label_display_with_marker(
      public_display_label[[i]],
      if (identical(public_label_source[[i]], "model_output")) plot_marker[[i]] else ""
    )
    public_display_label_preview <- .cluster_label_registry_plot_preview(
      public_display_label_plot
    )

    .cluster_label_registry_hclust_label(
      cluster_id = reg$cluster[[i]],
      display_label = display_label_preview,
      output_status = output_status[[i]],
      used_placeholder = used_placeholder[[i]],
      public_display_label = public_display_label_preview,
      public_label_source = public_label_source[[i]],
      plot_marker = plot_marker[[i]]
    )
  }, character(1))

  reg
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
