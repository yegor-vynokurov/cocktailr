#' Validate a structured cluster label against cluster evidence
#'
#' This function performs a lightweight validation pass over the structured
#' output returned by [llm_label_cluster()] and checks whether the answer stays
#' within the evidence supplied by [cluster_evidence()].
#'
#' The validator combines:
#' \itemize{
#'   \item schema-like structural checks for required fields,
#'   \item evidence citation integrity checks,
#'   \item simple heuristics for unsupported ecological overreach,
#'   \item consistency checks between label, abstention, and confidence.
#' }
#'
#' It is intentionally conservative: outputs that look structurally correct but
#' cite missing evidence IDs, blur external knowledge with data-derived claims,
#' or make habitat-level claims without explicit grounding are flagged for human
#' review.
#'
#' @param x A parsed cluster label output list, or an object returned by
#'   [llm_label_cluster()].
#' @param evidence A `cluster_evidence` object produced by
#'   [cluster_evidence()].
#'
#' @return An object of class `cluster_label_validation` with the following
#'   components:
#'   \describe{
#'     \item{cluster_id}{Cluster identifier.}
#'     \item{output_status}{Model output status (`"labeled"` or `"abstain"`).}
#'     \item{validation_status}{One of `"valid"`, `"valid_with_warnings"`,
#'       `"schema_error"`, `"unsupported_claims"`, or `"abstained"`.}
#'     \item{is_valid}{Logical flag indicating whether the output passed without
#'       hard validation failures.}
#'     \item{needs_human_review}{Logical flag for downstream review.}
#'     \item{evidence_coverage}{Citation coverage summary.}
#'     \item{issues}{Data frame of validation issues.}
#'     \item{output}{The parsed output that was validated.}
#'   }
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
#' print(val)
#'
#' @export
validate_cluster_label <- function(x, evidence) {
  output <- .extract_cluster_label_output(x)

  if (!inherits(evidence, "cluster_evidence")) {
    stop("`evidence` must be a `cluster_evidence` object.", call. = FALSE)
  }
  if (!is.list(output) || is.null(names(output))) {
    stop("`x` must be a named list or a `cluster_label_result` object.",
      call. = FALSE
    )
  }

  issues <- .new_cluster_label_issue_table()
  add_issue <- function(severity, category, code, message, location = NA_character_) {
    issues <<- rbind(
      issues,
      data.frame(
        severity = severity,
        category = category,
        code = code,
        message = message,
        location = location,
        stringsAsFactors = FALSE
      )
    )
  }

  required_fields <- c(
    "schema_version",
    "cluster_id",
    "status",
    "canonical_label",
    "display_label",
    "interpretation_summary",
    "basis_in_data",
    "key_species",
    "external_knowledge",
    "not_confirmed_by_data",
    "confidence",
    "checks_to_run",
    "abstain_reason"
  )
  optional_fields <- c(
    "ontology_slots",
    "contrastive_notes",
    "report_recommendation"
  )
  allowed_fields <- c(required_fields, optional_fields)

  missing_fields <- setdiff(required_fields, names(output))
  if (length(missing_fields) > 0L) {
    add_issue(
      "error",
      "schema",
      "missing_required_fields",
      sprintf(
        "Missing required field(s): %s.",
        paste(missing_fields, collapse = ", ")
      ),
      "top_level"
    )
  }

  unknown_fields <- setdiff(names(output), allowed_fields)
  if (length(unknown_fields) > 0L) {
    add_issue(
      "error",
      "schema",
      "unknown_top_level_fields",
      sprintf(
        "Unknown top-level field(s): %s.",
        paste(unknown_fields, collapse = ", ")
      ),
      "top_level"
    )
  }

  evidence_index <- .cluster_evidence_index(evidence)
  available_ids <- evidence_index$id
  available_id_set <- stats::setNames(evidence_index$type, evidence_index$id)
  expected_cluster_id <- .cluster_evidence_cluster_id(evidence)

  schema_version <- .as_scalar_character(output$schema_version)
  if (is.na(schema_version) || !identical(schema_version, "0.1.0")) {
    add_issue(
      "error",
      "schema",
      "invalid_schema_version",
      "schema_version must be the supported value \"0.1.0\".",
      "schema_version"
    )
  }

  cluster_id <- .as_scalar_character(output$cluster_id)
  if (is.na(cluster_id) || identical(cluster_id, "")) {
    add_issue(
      "error",
      "schema",
      "invalid_cluster_id",
      "cluster_id must be a non-empty string.",
      "cluster_id"
    )
  } else if (!is.na(expected_cluster_id) && !identical(cluster_id, expected_cluster_id)) {
    add_issue(
      "error",
      "schema",
      "cluster_id_mismatch",
      sprintf(
        "Output cluster_id \"%s\" does not match evidence cluster_id \"%s\".",
        cluster_id,
        expected_cluster_id
      ),
      "cluster_id"
    )
  }

  output_status <- .as_scalar_character(output$status)
  if (!output_status %in% c("labeled", "abstain")) {
    add_issue(
      "error",
      "schema",
      "invalid_status",
      "status must be either \"labeled\" or \"abstain\".",
      "status"
    )
  }

  canonical_label <- output$canonical_label
  display_label <- output$display_label
  abstain_reason <- output$abstain_reason

  if (identical(output_status, "labeled")) {
    if (!.is_non_empty_scalar_character(canonical_label)) {
      add_issue(
        "error",
        "schema",
        "missing_canonical_label",
        "Labeled outputs must provide a non-empty canonical_label.",
        "canonical_label"
      )
    } else if (!grepl("^[a-z0-9_]+$", canonical_label)) {
      add_issue(
        "error",
        "schema",
        "invalid_canonical_label_format",
        "canonical_label must use lowercase snake_case.",
        "canonical_label"
      )
    } else if (nchar(canonical_label, type = "chars") > .cluster_label_max_canonical_length()) {
      add_issue(
        "error",
        "schema",
        "canonical_label_too_long",
        sprintf(
          "canonical_label must be at most %d characters long.",
          .cluster_label_max_canonical_length()
        ),
        "canonical_label"
      )
    }
    if (!.is_non_empty_scalar_character(display_label)) {
      add_issue(
        "error",
        "schema",
        "missing_display_label",
        "Labeled outputs must provide a non-empty display_label.",
        "display_label"
      )
    } else {
      display_label_trimmed <- trimws(display_label)
      display_word_count <- .cluster_label_word_count(display_label_trimmed)

      if (nchar(display_label_trimmed, type = "chars") > .cluster_label_max_display_length()) {
        add_issue(
          "error",
          "schema",
          "display_label_too_long",
          sprintf(
            "display_label must be at most %d characters long.",
            .cluster_label_max_display_length()
          ),
          "display_label"
        )
      }

      if (display_word_count > .cluster_label_max_display_words()) {
        add_issue(
          "error",
          "schema",
          "display_label_too_many_words",
          sprintf(
            "display_label must contain at most %d words.",
            .cluster_label_max_display_words()
          ),
          "display_label"
        )
      }

      if (grepl(.cluster_label_forbidden_display_punctuation_pattern(), display_label_trimmed, perl = TRUE)) {
        add_issue(
          "error",
          "schema",
          "display_label_forbidden_punctuation",
          "display_label must not contain commas or brackets.",
          "display_label"
        )
      }

      if (grepl("\\.$", display_label_trimmed, perl = TRUE)) {
        add_issue(
          "error",
          "schema",
          "display_label_trailing_period",
          "display_label must not end with a period.",
          "display_label"
        )
      }
    }
    if (!is.null(abstain_reason)) {
      add_issue(
        "error",
        "schema",
        "unexpected_abstain_reason",
        "Labeled outputs must set abstain_reason to null.",
        "abstain_reason"
      )
    }
  }

  if (identical(output_status, "abstain")) {
    if (!is.null(canonical_label)) {
      add_issue(
        "error",
        "schema",
        "unexpected_canonical_label",
        "Abstaining outputs must set canonical_label to null.",
        "canonical_label"
      )
    }
    if (!is.null(display_label)) {
      add_issue(
        "error",
        "schema",
        "unexpected_display_label",
        "Abstaining outputs must set display_label to null.",
        "display_label"
      )
    }
    if (!.is_non_empty_scalar_character(abstain_reason)) {
      add_issue(
        "error",
        "schema",
        "missing_abstain_reason",
        "Abstaining outputs must provide a non-empty abstain_reason.",
        "abstain_reason"
      )
    }
  }

  interpretation_summary <- .as_scalar_character(output$interpretation_summary)
  if (is.na(interpretation_summary) || !nzchar(trimws(interpretation_summary))) {
    add_issue(
      "error",
      "schema",
      "missing_interpretation_summary",
      "interpretation_summary must be a non-empty string.",
      "interpretation_summary"
    )
  }

  basis_in_data <- output$basis_in_data
  if (!is.list(basis_in_data)) {
    add_issue(
      "error",
      "schema",
      "invalid_basis_in_data",
      "basis_in_data must be a list.",
      "basis_in_data"
    )
    basis_in_data <- list()
  }

  key_species <- output$key_species
  if (!is.list(key_species)) {
    add_issue(
      "error",
      "schema",
      "invalid_key_species",
      "key_species must be a list.",
      "key_species"
    )
    key_species <- list()
  }

  external_knowledge <- output$external_knowledge
  if (!is.list(external_knowledge)) {
    add_issue(
      "error",
      "schema",
      "invalid_external_knowledge",
      "external_knowledge must be a list.",
      "external_knowledge"
    )
    external_knowledge <- list()
  }

  not_confirmed_by_data <- output$not_confirmed_by_data
  if (!is.list(not_confirmed_by_data)) {
    add_issue(
      "error",
      "schema",
      "invalid_not_confirmed_by_data",
      "not_confirmed_by_data must be a list.",
      "not_confirmed_by_data"
    )
    not_confirmed_by_data <- list()
  }

  confidence <- output$confidence
  if (!is.list(confidence)) {
    add_issue(
      "error",
      "schema",
      "invalid_confidence",
      "confidence must be an object with score and rationale fields.",
      "confidence"
    )
    confidence <- list()
  }

  checks_to_run <- output$checks_to_run
  if (!is.list(checks_to_run)) {
    add_issue(
      "error",
      "schema",
      "invalid_checks_to_run",
      "checks_to_run must be a list.",
      "checks_to_run"
    )
    checks_to_run <- list()
  }

  basis_validation <- .validate_basis_claims(
    basis_in_data = basis_in_data,
    available_id_set = available_id_set,
    add_issue = add_issue
  )
  key_species_validation <- .validate_key_species_claims(
    key_species = key_species,
    available_id_set = available_id_set,
    add_issue = add_issue
  )
  .validate_external_knowledge_claims(
    external_knowledge = external_knowledge,
    add_issue = add_issue
  )
  .validate_not_confirmed_claims(
    not_confirmed_by_data = not_confirmed_by_data,
    add_issue = add_issue
  )
  confidence_score <- .validate_confidence_block(
    confidence = confidence,
    add_issue = add_issue
  )
  .validate_checks_to_run(
    checks_to_run = checks_to_run,
    add_issue = add_issue
  )

  coverage <- .compute_cluster_label_evidence_coverage(
    basis_validation = basis_validation,
    key_species_validation = key_species_validation
  )

  if (identical(output_status, "labeled") && length(basis_in_data) == 0L) {
    add_issue(
      "error",
      "unsupported_claims",
      "missing_basis_claims_for_label",
      "Labeled outputs must provide at least one basis_in_data claim.",
      "basis_in_data"
    )
  }

  if (identical(output_status, "labeled") && length(key_species) == 0L) {
    add_issue(
      "warning",
      "consistency",
      "missing_key_species_for_label",
      "Labeled outputs should name at least one key species.",
      "key_species"
    )
  }

  if (coverage$score < 0.75) {
    add_issue(
      "warning",
      "consistency",
      "low_evidence_coverage",
      sprintf(
        "Evidence citation coverage is low (%.2f).",
        coverage$score
      ),
      "citations"
    )
  }

  .validate_habitat_overreach(
    output = output,
    output_status = output_status,
    add_issue = add_issue
  )

  if (identical(output_status, "labeled") &&
      is.finite(confidence_score) &&
      confidence_score >= 0.8 &&
      length(not_confirmed_by_data) >= 2L) {
    add_issue(
      "warning",
      "consistency",
      "high_confidence_with_multiple_uncertainties",
      "High confidence is difficult to justify when multiple limitations are explicitly listed.",
      "confidence"
    )
  }

  if (identical(output_status, "labeled") &&
      is.finite(confidence_score) &&
      confidence_score < 0.4) {
    add_issue(
      "warning",
      "consistency",
      "low_confidence_labeled_output",
      "The output is labeled but the confidence score is low.",
      "confidence"
    )
  }

  validation_status <- .finalize_cluster_label_validation_status(
    output_status = output_status,
    issues = issues
  )

  needs_human_review <- .cluster_label_needs_human_review(
    validation_status = validation_status,
    issues = issues
  )

  result <- list(
    cluster_id = cluster_id,
    output_status = output_status,
    validation_status = validation_status,
    is_valid = validation_status %in% c("valid", "valid_with_warnings", "abstained"),
    needs_human_review = needs_human_review,
    label_tier = if (identical(output_status, "labeled")) "accepted" else NA_character_,
    is_speculative = FALSE,
    plot_marker = "",
    strict_outcome = if (identical(output_status, "labeled")) "accepted" else "abstained",
    strict_validation_status = validation_status,
    label_origin = NA_character_,
    species_entropy_band = NA_character_,
    species_entropy_text = NA_character_,
    chaoticity_score = NA_integer_,
    chaoticity_label = NA_character_,
    missing_for_confidence_text = .cluster_label_missing_for_confidence_from_output(output),
    evidence_coverage = coverage,
    issues = issues,
    available_evidence_ids = available_ids,
    output = output
  )

  class(result) <- "cluster_label_validation"
  result
}

#' @export
print.cluster_label_validation <- function(x, ...) {
  cat("<cluster_label_validation>\n")
  cat("  cluster_id: ", .null_default(x$cluster_id, NA_character_), "\n", sep = "")
  cat("  output_status: ", .null_default(x$output_status, NA_character_), "\n", sep = "")
  cat("  validation_status: ", .null_default(x$validation_status, NA_character_), "\n", sep = "")
  cat("  needs_human_review: ", x$needs_human_review, "\n", sep = "")
  cat(
    "  evidence_coverage: ",
    sprintf("%.2f", x$evidence_coverage$score),
    " (",
    x$evidence_coverage$n_valid_citations,
    "/",
    x$evidence_coverage$n_citation_targets,
    ")\n",
    sep = ""
  )

  if (nrow(x$issues) == 0L) {
    cat("  issues: none\n")
  } else {
    cat("  issues:\n")
    for (i in seq_len(nrow(x$issues))) {
      cat(
        "    - [",
        x$issues$severity[[i]],
        "] ",
        x$issues$code[[i]],
        ": ",
        x$issues$message[[i]],
        "\n",
        sep = ""
      )
    }
  }

  invisible(x)
}

.cluster_evidence_index <- function(evidence) {
  items <- evidence$evidence$items
  if (!is.list(items) || length(items) == 0L) {
    return(data.frame(
      id = character(),
      type = character(),
      stringsAsFactors = FALSE
    ))
  }

  ids <- vapply(items, function(item) {
    if (is.list(item) && .is_non_empty_scalar_character(item$id)) {
      return(item$id)
    }
    NA_character_
  }, character(1))

  item_names <- names(items)
  missing_ids <- is.na(ids) | !nzchar(ids)
  if (!is.null(item_names) && any(missing_ids)) {
    ids[missing_ids] <- item_names[missing_ids]
  }

  types <- vapply(items, function(item) {
    if (is.list(item) && .is_non_empty_scalar_character(item$type)) {
      return(item$type)
    }
    NA_character_
  }, character(1))

  keep <- !is.na(ids) & nzchar(ids)
  data.frame(
    id = ids[keep],
    type = types[keep],
    stringsAsFactors = FALSE
  )
}

.validate_basis_claims <- function(basis_in_data, available_id_set, add_issue) {
  valid_count <- 0L
  target_count <- 0L
  basis_text <- character()

  for (i in seq_along(basis_in_data)) {
    claim <- basis_in_data[[i]]
    location <- sprintf("basis_in_data[%d]", i)

    if (!is.list(claim)) {
      add_issue(
        "error",
        "schema",
        "invalid_basis_claim",
        "Each basis_in_data entry must be an object.",
        location
      )
      next
    }

    claim_id <- .as_scalar_character(claim$claim_id)
    statement <- .as_scalar_character(claim$statement)
    evidence_ids <- .as_character_vector(claim$evidence_ids)
    support_strength <- .as_scalar_character(claim$support_strength)

    if (is.na(claim_id) || !nzchar(trimws(claim_id))) {
      add_issue(
        "error",
        "schema",
        "missing_basis_claim_id",
        "Each basis_in_data entry must provide a non-empty claim_id.",
        location
      )
    }

    if (is.na(statement) || !nzchar(trimws(statement))) {
      add_issue(
        "error",
        "schema",
        "missing_basis_statement",
        "Each basis_in_data entry must provide a non-empty statement.",
        location
      )
    } else {
      basis_text <- c(basis_text, statement)
    }

    if (!support_strength %in% c("strong", "moderate", "weak", NA_character_)) {
      add_issue(
        "error",
        "schema",
        "invalid_support_strength",
        "support_strength must be one of strong, moderate, or weak when present.",
        location
      )
    }

    target_count <- target_count + 1L
    if (length(evidence_ids) == 0L) {
      add_issue(
        "error",
        "unsupported_claims",
        "basis_claim_without_evidence_ids",
        "Each basis_in_data claim must cite at least one evidence_id.",
        location
      )
      next
    }

    unknown_ids <- setdiff(evidence_ids, names(available_id_set))
    if (length(unknown_ids) > 0L) {
      add_issue(
        "error",
        "unsupported_claims",
        "unknown_basis_evidence_ids",
        sprintf(
          "Unknown evidence_id(s) referenced: %s.",
          paste(unknown_ids, collapse = ", ")
        ),
        location
      )
      next
    }

    cited_types <- unname(available_id_set[evidence_ids])
    if (all(cited_types %in% "limitation")) {
      add_issue(
        "error",
        "unsupported_claims",
        "basis_claim_cites_only_limitations",
        "A basis_in_data claim cannot rely only on limitation evidence IDs.",
        location
      )
      next
    }

    valid_count <- valid_count + 1L
  }

  list(
    n_valid = valid_count,
    n_total = target_count,
    text = basis_text
  )
}

.validate_key_species_claims <- function(key_species, available_id_set, add_issue) {
  valid_count <- 0L
  target_count <- 0L

  for (i in seq_along(key_species)) {
    claim <- key_species[[i]]
    location <- sprintf("key_species[%d]", i)

    if (!is.list(claim)) {
      add_issue(
        "error",
        "schema",
        "invalid_key_species_entry",
        "Each key_species entry must be an object.",
        location
      )
      next
    }

    species <- .as_scalar_character(claim$species)
    role <- .as_scalar_character(claim$role)
    evidence_ids <- .as_character_vector(claim$evidence_ids)

    if (is.na(species) || !nzchar(trimws(species))) {
      add_issue(
        "error",
        "schema",
        "missing_key_species_name",
        "Each key_species entry must provide a species name.",
        location
      )
    }
    if (is.na(role) || !nzchar(trimws(role))) {
      add_issue(
        "error",
        "schema",
        "missing_key_species_role",
        "Each key_species entry must provide a role.",
        location
      )
    } else if (!role %in% c(
      "topological",
      "phi_ranked",
      "prototype_support",
      "borderline_indicator",
      "other"
    )) {
      add_issue(
        "error",
        "schema",
        "invalid_key_species_role",
        "key_species role is outside the supported schema values.",
        location
      )
    }

    target_count <- target_count + 1L
    if (length(evidence_ids) == 0L) {
      add_issue(
        "error",
        "unsupported_claims",
        "key_species_without_evidence_ids",
        "Each key_species entry must cite at least one evidence_id.",
        location
      )
      next
    }

    unknown_ids <- setdiff(evidence_ids, names(available_id_set))
    if (length(unknown_ids) > 0L) {
      add_issue(
        "error",
        "unsupported_claims",
        "unknown_key_species_evidence_ids",
        sprintf(
          "Unknown evidence_id(s) referenced: %s.",
          paste(unknown_ids, collapse = ", ")
        ),
        location
      )
      next
    }

    cited_types <- unname(available_id_set[evidence_ids])
    if (all(cited_types %in% "limitation")) {
      add_issue(
        "error",
        "unsupported_claims",
        "key_species_cites_only_limitations",
        "A key_species entry cannot rely only on limitation evidence IDs.",
        location
      )
      next
    }

    valid_count <- valid_count + 1L
  }

  list(
    n_valid = valid_count,
    n_total = target_count
  )
}

.validate_external_knowledge_claims <- function(external_knowledge, add_issue) {
  data_language_pattern <- paste(
    c(
      "\\bthe data\\b",
      "\\bthis data\\b",
      "\\bthe evidence\\b",
      "\\bprovided evidence\\b",
      "\\bthe plots\\b",
      "\\bthese plots\\b",
      "\\bphi\\b",
      "\\bcover\\b",
      "\\bobserved in (the )?plots\\b",
      "\\bmeasured in (the )?plots\\b",
      "\\bcluster\\s+members\\b"
    ),
    collapse = "|"
  )

  for (i in seq_along(external_knowledge)) {
    claim <- external_knowledge[[i]]
    location <- sprintf("external_knowledge[%d]", i)

    if (!is.list(claim)) {
      add_issue(
        "error",
        "schema",
        "invalid_external_knowledge_entry",
        "Each external_knowledge entry must be an object.",
        location
      )
      next
    }

    statement <- .as_scalar_character(claim$statement)
    knowledge_type <- .as_scalar_character(claim$knowledge_type)
    confidence <- .as_scalar_character(claim$confidence)

    if (is.na(statement) || !nzchar(trimws(statement))) {
      add_issue(
        "error",
        "schema",
        "missing_external_knowledge_statement",
        "Each external_knowledge entry must provide a statement.",
        location
      )
    } else if (grepl(data_language_pattern, statement, ignore.case = TRUE, perl = TRUE)) {
      add_issue(
        "error",
        "unsupported_claims",
        "external_knowledge_poses_as_data",
        "External knowledge statements must not present themselves as data-derived evidence.",
        location
      )
    }

    if (!knowledge_type %in% c("habitat_hint", "species_trait", "syntaxonomic_hint", "other", NA_character_)) {
      add_issue(
        "error",
        "schema",
        "invalid_knowledge_type",
        "knowledge_type must be habitat_hint, species_trait, syntaxonomic_hint, or other.",
        location
      )
    }

    if (!confidence %in% c("high", "medium", "low", NA_character_)) {
      add_issue(
        "error",
        "schema",
        "invalid_external_knowledge_confidence",
        "External knowledge confidence must be high, medium, or low.",
        location
      )
    }
  }
}

.validate_not_confirmed_claims <- function(not_confirmed_by_data, add_issue) {
  for (i in seq_along(not_confirmed_by_data)) {
    claim <- not_confirmed_by_data[[i]]
    location <- sprintf("not_confirmed_by_data[%d]", i)

    if (!is.list(claim)) {
      add_issue(
        "error",
        "schema",
        "invalid_not_confirmed_entry",
        "Each not_confirmed_by_data entry must be an object.",
        location
      )
      next
    }

    statement <- .as_scalar_character(claim$statement)
    reason <- .as_scalar_character(claim$reason)

    if (is.na(statement) || !nzchar(trimws(statement))) {
      add_issue(
        "error",
        "schema",
        "missing_not_confirmed_statement",
        "Each not_confirmed_by_data entry must provide a statement.",
        location
      )
    }

    if (is.na(reason) || !nzchar(trimws(reason))) {
      add_issue(
        "error",
        "schema",
        "missing_not_confirmed_reason",
        "Each not_confirmed_by_data entry must provide a reason.",
        location
      )
    }
  }
}

.validate_confidence_block <- function(confidence, add_issue) {
  score <- suppressWarnings(as.numeric(confidence$score))
  rationale <- .as_scalar_character(confidence$rationale)

  if (!is.finite(score) || score < 0 || score > 1) {
    add_issue(
      "error",
      "schema",
      "invalid_confidence_score",
      "confidence.score must be numeric and between 0 and 1.",
      "confidence.score"
    )
    score <- NA_real_
  }

  if (is.na(rationale) || !nzchar(trimws(rationale))) {
    add_issue(
      "error",
      "schema",
      "missing_confidence_rationale",
      "confidence.rationale must be a non-empty string.",
      "confidence.rationale"
    )
  }

  score
}

.validate_checks_to_run <- function(checks_to_run, add_issue) {
  for (i in seq_along(checks_to_run)) {
    check_item <- checks_to_run[[i]]
    location <- sprintf("checks_to_run[%d]", i)

    if (!is.list(check_item)) {
      add_issue(
        "error",
        "schema",
        "invalid_check_entry",
        "Each checks_to_run entry must be an object.",
        location
      )
      next
    }

    check_name <- .as_scalar_character(check_item$check)
    priority <- .as_scalar_character(check_item$priority)
    reason <- .as_scalar_character(check_item$reason)

    if (is.na(check_name) || !nzchar(trimws(check_name))) {
      add_issue(
        "error",
        "schema",
        "missing_check_name",
        "Each checks_to_run entry must provide a check name.",
        location
      )
    }

    if (!priority %in% c("high", "medium", "low", NA_character_)) {
      add_issue(
        "error",
        "schema",
        "invalid_check_priority",
        "Check priority must be high, medium, or low.",
        location
      )
    }

    if (is.na(reason) || !nzchar(trimws(reason))) {
      add_issue(
        "error",
        "schema",
        "missing_check_reason",
        "Each checks_to_run entry must provide a reason.",
        location
      )
    }
  }
}

.cluster_label_max_canonical_length <- function() {
  64L
}

.cluster_label_max_display_length <- function() {
  80L
}

.cluster_label_max_display_words <- function() {
  6L
}

.cluster_label_forbidden_display_punctuation_pattern <- function() {
  "[,()\\[\\]]"
}

.cluster_label_word_count <- function(x) {
  x <- .as_scalar_character(x)
  if (is.na(x)) {
    return(NA_integer_)
  }

  x <- trimws(x)
  if (!nzchar(x)) {
    return(0L)
  }

  length(strsplit(x, "\\s+", perl = TRUE)[[1]])
}

.compute_cluster_label_evidence_coverage <- function(basis_validation, key_species_validation) {
  n_valid <- basis_validation$n_valid + key_species_validation$n_valid
  n_total <- basis_validation$n_total + key_species_validation$n_total
  score <- if (n_total == 0L) {
    0
  } else {
    n_valid / n_total
  }

  list(
    score = unname(score),
    n_valid_citations = n_valid,
    n_citation_targets = n_total,
    basis_claims_valid = basis_validation$n_valid,
    basis_claims_total = basis_validation$n_total,
    key_species_valid = key_species_validation$n_valid,
    key_species_total = key_species_validation$n_total
  )
}

.validate_habitat_overreach <- function(output, output_status, add_issue) {
  if (!identical(output_status, "labeled")) {
    return(invisible(NULL))
  }

  basis_in_data <- if (is.list(output$basis_in_data)) output$basis_in_data else list()
  external_knowledge <- if (is.list(output$external_knowledge)) output$external_knowledge else list()
  not_confirmed_by_data <- if (is.list(output$not_confirmed_by_data)) output$not_confirmed_by_data else list()

  habitat_pattern <- paste(
    c(
      "\\bwoodland\\b",
      "\\bforest\\b",
      "\\bgrassland\\b",
      "\\bmeadow\\b",
      "\\bwetland\\b",
      "\\bfen\\b",
      "\\bmarsh\\b",
      "\\bbog\\b",
      "\\bsteppe\\b",
      "\\bruderal\\b",
      "\\becotone\\b",
      "\\bedge\\b",
      "\\bcalcareous\\b",
      "\\bacid(ic)?\\b",
      "\\bnitrophil",
      "\\bshade\\b",
      "\\bshaded\\b",
      "\\bmesic\\b",
      "\\bmoist\\b",
      "\\bdry\\b"
    ),
    collapse = "|"
  )

  label_text <- paste(
    c(
      .as_scalar_character(output$canonical_label),
      .as_scalar_character(output$display_label),
      .as_scalar_character(output$interpretation_summary)
    ),
    collapse = " "
  )
  basis_text <- paste(
    vapply(basis_in_data, function(item) .as_scalar_character(item$statement), character(1)),
    collapse = " "
  )
  external_text <- paste(
    vapply(external_knowledge, function(item) .as_scalar_character(item$statement), character(1)),
    collapse = " "
  )
  not_confirmed_text <- paste(
    vapply(not_confirmed_by_data, function(item) .as_scalar_character(item$statement), character(1)),
    collapse = " "
  )

  label_has_habitat_language <- nzchar(label_text) &&
    grepl(habitat_pattern, label_text, ignore.case = TRUE, perl = TRUE)
  basis_has_habitat_language <- nzchar(basis_text) &&
    grepl(habitat_pattern, basis_text, ignore.case = TRUE, perl = TRUE)
  external_has_habitat_language <- nzchar(external_text) &&
    grepl(habitat_pattern, external_text, ignore.case = TRUE, perl = TRUE)
  not_confirmed_has_habitat_language <- nzchar(not_confirmed_text) &&
    grepl(habitat_pattern, not_confirmed_text, ignore.case = TRUE, perl = TRUE)

  if (label_has_habitat_language &&
      !basis_has_habitat_language &&
      !external_has_habitat_language &&
      !not_confirmed_has_habitat_language) {
    add_issue(
      "error",
      "unsupported_claims",
      "unsupported_habitat_overreach",
      paste(
        "The label or interpretation uses habitat-level language without",
        "making the species-to-habitat inference explicit in external_knowledge",
        "or not_confirmed_by_data."
      ),
      "labeling"
    )
  }
}

.finalize_cluster_label_validation_status <- function(output_status, issues) {
  if (any(issues$category == "schema" & issues$severity == "error")) {
    return("schema_error")
  }
  if (any(issues$category == "unsupported_claims")) {
    return("unsupported_claims")
  }
  if (identical(output_status, "abstain")) {
    return("abstained")
  }
  if (nrow(issues) > 0L) {
    return("valid_with_warnings")
  }
  "valid"
}

.cluster_label_needs_human_review <- function(validation_status, issues) {
  if (validation_status %in% c("schema_error", "unsupported_claims")) {
    return(TRUE)
  }
  any(issues$severity == "warning")
}
