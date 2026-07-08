.build_validation_test_cluster_evidence <- function() {
  vm <- matrix(
    c(
      55, 40, 0, 0,
      50, 35, 0, 0,
      25, 10, 30, 0,
      0, 0, 60, 45,
      0, 0, 50, 35,
      0, 0, 40, 25
    ),
    nrow = 6,
    byrow = TRUE,
    dimnames = list(
      paste0("plot", 1:6),
      paste0("sp", 1:4)
    )
  )

  x <- suppressWarnings(cocktail_cluster(
    vm,
    progress = FALSE,
    plot_values = "rel_cover",
    species_cluster_phi = TRUE,
    save_vegmatrix = TRUE
  ))

  cluster_evidence(
    x,
    cluster = "c_1",
    top_n_phi = 3,
    n_prototype_plots = 2,
    n_borderline_plots = 2
  )
}

.build_valid_label_output <- function(ev) {
  topo <- ev$summaries$species_topological
  phi <- ev$summaries$species_phi
  cover <- ev$summaries$cover_summary
  proto <- ev$summaries$plots_prototype

  list(
    schema_version = "0.1.0",
    cluster_id = ev$meta$cluster_id,
    status = "labeled",
    canonical_label = "sp1_sp2_cluster",
    display_label = "sp1-sp2 cluster",
    interpretation_summary = "The cluster is defined by a compact recurring species core and consistent prototype plots.",
    basis_in_data = list(
      list(
        claim_id = "C1",
        statement = "Two topological species recur across the cluster core.",
        evidence_ids = c("E4", topo$evidence_id[[1]], topo$evidence_id[[2]]),
        support_strength = "strong"
      ),
      list(
        claim_id = "C2",
        statement = "Prototype plots and relative cover summaries reinforce the same dominant species pattern.",
        evidence_ids = c(proto$evidence_id[[1]], cover$evidence_id[[1]]),
        support_strength = "moderate"
      )
    ),
    key_species = list(
      list(
        species = topo$species[[1]],
        role = "topological",
        evidence_ids = c(topo$evidence_id[[1]])
      ),
      list(
        species = phi$species[[1]],
        role = "phi_ranked",
        evidence_ids = c(phi$evidence_id[[1]])
      )
    ),
    external_knowledge = list(),
    not_confirmed_by_data = list(),
    confidence = list(
      score = 0.72,
      rationale = "The cluster is cohesive in the available evidence, but the label is intentionally compositional rather than ecological."
    ),
    checks_to_run = list(
      list(
        check = "Compare against neighboring clusters if finer interpretation is needed.",
        priority = "medium",
        reason = "A contrastive pass would help decide whether a broader ecological label is justified."
      )
    ),
    abstain_reason = NULL
  )
}

test_that("validate_cluster_label accepts parsed output and cluster_label_result objects", {
  ev <- .build_validation_test_cluster_evidence()
  output <- .build_valid_label_output(ev)

  res <- list(output = output)
  class(res) <- c("cluster_label_result", "list")

  val <- validate_cluster_label(res, ev)

  expect_s3_class(val, "cluster_label_validation")
  expect_equal(val$cluster_id, "c_1")
  expect_equal(val$output_status, "labeled")
  expect_equal(val$validation_status, "valid")
  expect_false(val$needs_human_review)
  expect_equal(val$label_tier, "accepted")
  expect_false(val$is_speculative)
  expect_equal(val$plot_marker, "")
  expect_equal(val$strict_outcome, "accepted")
  expect_equal(val$strict_validation_status, "valid")
  expect_equal(val$evidence_coverage$score, 1)
  expect_equal(nrow(val$issues), 0L)
})

test_that("validate_cluster_label flags missing required fields as schema errors", {
  ev <- .build_validation_test_cluster_evidence()
  output <- .build_valid_label_output(ev)
  output$checks_to_run <- NULL

  val <- validate_cluster_label(output, ev)

  expect_equal(val$validation_status, "schema_error")
  expect_true(any(val$issues$code == "missing_required_fields"))
  expect_true(val$needs_human_review)
})

test_that("validate_cluster_label flags unknown evidence IDs", {
  ev <- .build_validation_test_cluster_evidence()
  output <- .build_valid_label_output(ev)
  output$basis_in_data[[1]]$evidence_ids <- c("E999")

  val <- validate_cluster_label(output, ev)

  expect_equal(val$validation_status, "unsupported_claims")
  expect_true(any(val$issues$code == "unknown_basis_evidence_ids"))
  expect_true(val$needs_human_review)
})

test_that("validate_cluster_label accepts evidence_ids returned as nested lists", {
  ev <- .build_validation_test_cluster_evidence()
  output <- .build_valid_label_output(ev)
  output$basis_in_data[[1]]$evidence_ids <- as.list(output$basis_in_data[[1]]$evidence_ids)
  output$key_species[[1]]$evidence_ids <- as.list(output$key_species[[1]]$evidence_ids)

  val <- validate_cluster_label(output, ev)

  expect_equal(val$validation_status, "valid")
  expect_equal(val$evidence_coverage$score, 1)
})

test_that("validate_cluster_label flags external knowledge that poses as data", {
  ev <- .build_validation_test_cluster_evidence()
  output <- .build_valid_label_output(ev)
  output$external_knowledge <- list(
    list(
      statement = "The data show this cluster is a wet meadow.",
      knowledge_type = "habitat_hint",
      confidence = "medium"
    )
  )

  val <- validate_cluster_label(output, ev)

  expect_equal(val$validation_status, "unsupported_claims")
  expect_true(any(val$issues$code == "external_knowledge_poses_as_data"))
})

test_that("validate_cluster_label turns malformed scalar confidence into schema errors", {
  ev <- .build_validation_test_cluster_evidence()
  output <- .build_valid_label_output(ev)
  output$confidence <- 0.9

  expect_no_error(val <- validate_cluster_label(output, ev))
  expect_equal(val$validation_status, "schema_error")
  expect_true(any(val$issues$code == "invalid_confidence"))
  expect_true(any(val$issues$code == "invalid_confidence_score"))
  expect_true(any(val$issues$code == "missing_confidence_rationale"))
  expect_true(val$needs_human_review)
})

test_that("validate_cluster_label does not reject habitat wording via the disabled heuristic", {
  ev <- .build_validation_test_cluster_evidence()
  output <- .build_valid_label_output(ev)
  output$canonical_label <- "dry_grassland_cluster"
  output$display_label <- "dry grassland cluster"
  output$interpretation_summary <- paste(
    "The cluster looks like a dry grassland assemblage based on its compact species core."
  )

  val <- validate_cluster_label(output, ev)

  expect_equal(val$validation_status, "valid")
  expect_false(any(val$issues$code == "unsupported_habitat_overreach"))
  expect_false(val$needs_human_review)
})

test_that("cluster label schema asset encodes label length and format limits", {
  schema_path <- cocktailr:::.package_asset_path("schemas", "cluster_label_output_schema.json")
  schema <- jsonlite::read_json(
    schema_path,
    simplifyVector = FALSE
  )

  expect_equal(schema$properties$canonical_label$maxLength, 64)
  expect_equal(schema$properties$display_label$maxLength, 80)
  expect_equal(
    schema$properties$display_label$pattern,
    "^(?!.*[,()\\[\\]])(?!.*\\.\\s*$)\\S+(?:\\s+\\S+){0,5}$"
  )
})

test_that("validate_cluster_label rejects overly long canonical labels", {
  ev <- .build_validation_test_cluster_evidence()
  output <- .build_valid_label_output(ev)
  output$canonical_label <- paste(rep("a", 65), collapse = "")

  val <- validate_cluster_label(output, ev)

  expect_equal(val$validation_status, "schema_error")
  expect_true(any(val$issues$code == "canonical_label_too_long"))
})

test_that("validate_cluster_label rejects overly long display labels", {
  ev <- .build_validation_test_cluster_evidence()
  output <- .build_valid_label_output(ev)
  output$display_label <- paste(rep("a", 81), collapse = "")

  val <- validate_cluster_label(output, ev)

  expect_equal(val$validation_status, "schema_error")
  expect_true(any(val$issues$code == "display_label_too_long"))
})

test_that("validate_cluster_label rejects display labels with too many words", {
  ev <- .build_validation_test_cluster_evidence()
  output <- .build_valid_label_output(ev)
  output$display_label <- "one two three four five six seven"

  val <- validate_cluster_label(output, ev)

  expect_equal(val$validation_status, "schema_error")
  expect_true(any(val$issues$code == "display_label_too_many_words"))
})

test_that("validate_cluster_label rejects forbidden punctuation in display labels", {
  ev <- .build_validation_test_cluster_evidence()
  output <- .build_valid_label_output(ev)
  output$display_label <- "woodland, edge"

  val <- validate_cluster_label(output, ev)

  expect_equal(val$validation_status, "schema_error")
  expect_true(any(val$issues$code == "display_label_forbidden_punctuation"))
})

test_that("validate_cluster_label rejects display labels with a trailing period", {
  ev <- .build_validation_test_cluster_evidence()
  output <- .build_valid_label_output(ev)
  output$display_label <- "Woodland Edge."

  val <- validate_cluster_label(output, ev)

  expect_equal(val$validation_status, "schema_error")
  expect_true(any(val$issues$code == "display_label_trailing_period"))
})

test_that("validate_cluster_label recognizes a clean abstention output", {
  ev <- .build_validation_test_cluster_evidence()

  output <- list(
    schema_version = "0.1.0",
    cluster_id = "c_1",
    status = "abstain",
    canonical_label = NULL,
    display_label = NULL,
    interpretation_summary = "The cluster has some internal structure, but the available evidence is not distinctive enough for a stable ecological label.",
    basis_in_data = list(
      list(
        claim_id = "C1",
        statement = "A few recurring species define the cluster core, but they do not separate the cluster strongly enough from nearby alternatives.",
        evidence_ids = c("E1"),
        support_strength = "weak"
      )
    ),
    key_species = list(
      list(
        species = "sp1",
        role = "topological",
        evidence_ids = c("E1")
      )
    ),
    external_knowledge = list(),
    not_confirmed_by_data = list(
      list(
        statement = "A habitat-level name cannot be confirmed.",
        reason = "The evidence bundle contains species composition but no direct habitat metadata."
      )
    ),
    confidence = list(
      score = 0.41,
      rationale = "The evidence supports coherence, but not enough distinctiveness for a stable label."
    ),
    checks_to_run = list(
      list(
        check = "Compare with neighboring clusters.",
        priority = "high",
        reason = "A contrastive view would help determine whether the cluster is truly label-worthy."
      )
    ),
    abstain_reason = "Distinctiveness is insufficient for a stable label."
  )

  val <- validate_cluster_label(output, ev)

  expect_equal(val$output_status, "abstain")
  expect_equal(val$validation_status, "abstained")
  expect_false(val$needs_human_review)
  expect_true(is.na(val$label_tier))
  expect_false(val$is_speculative)
  expect_equal(val$plot_marker, "")
  expect_equal(val$strict_outcome, "abstained")
  expect_equal(val$evidence_coverage$score, 1)
})

test_that("speculative fallback validation remains distinct from accepted labels", {
  ev <- .build_validation_test_cluster_evidence()
  output <- .build_valid_label_output(ev)
  output$confidence$score <- 0
  output$confidence$rationale <- "Tentative only; the direction is visible but not stable."
  output$not_confirmed_by_data <- list(
    list(
      statement = "A habitat-level label is not confirmed.",
      reason = "The evidence bundle does not resolve contrast against nearby alternatives."
    )
  )

  val <- validate_cluster_label(output, ev)
  spec_val <- cocktailr:::.mark_speculative_validation(
    val,
    strict_validation_status = "unsupported_claims"
  )

  expect_equal(spec_val$validation_status, "valid_with_warnings")
  expect_true(spec_val$is_valid)
  expect_true(spec_val$needs_human_review)
  expect_equal(spec_val$label_tier, "speculative")
  expect_true(spec_val$is_speculative)
  expect_equal(spec_val$plot_marker, "*")
  expect_equal(spec_val$strict_outcome, "placeholder")
  expect_equal(spec_val$strict_validation_status, "unsupported_claims")
  expect_match(spec_val$missing_for_confidence_text, "not confirmed", ignore.case = TRUE)
  expect_true(any(spec_val$issues$code == "speculative_fallback_label"))
})
