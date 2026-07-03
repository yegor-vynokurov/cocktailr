.build_registry_test_cocktail <- function() {
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

  suppressWarnings(cocktail_cluster(
    vm,
    progress = FALSE,
    plot_values = "rel_cover",
    species_cluster_phi = TRUE,
    save_vegmatrix = TRUE
  ))
}

.build_registry_valid_output <- function(ev) {
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
    label_summary = "A compact recurring species core supports a short compositional label.",
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
    abstain_reason = NULL,
    explanation = "The same species core recurs across the evidence bundle, so a short compositional label is safe."
  )
}

.build_registry_abstain_output <- function(ev) {
  topo <- ev$summaries$species_topological
  phi <- ev$summaries$species_phi

  list(
    schema_version = "0.1.0",
    cluster_id = ev$meta$cluster_id,
    status = "abstain",
    canonical_label = NULL,
    display_label = NULL,
    label_summary = NULL,
    interpretation_summary = "The cluster is internally coherent, but the current evidence is not distinctive enough for a stable final label.",
    basis_in_data = list(),
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
    not_confirmed_by_data = list(
      list(
        statement = "A habitat-level interpretation cannot yet be confirmed.",
        reason = "The current bundle lacks stronger contrastive evidence against nearby alternatives."
      )
    ),
    confidence = list(
      score = 0.34,
      rationale = "The cluster has a recognizable core, but not enough distinctiveness for a reliable ecological label."
    ),
    checks_to_run = list(
      list(
        check = "Compare against sibling clusters.",
        priority = "high",
        reason = "A contrastive pass may reveal whether abstention should be lifted."
      )
    ),
    abstain_reason = "Distinctiveness is insufficient for a stable label.",
    explanation = "The evidence stays too mixed to defend a stable short label."
  )
}

.registry_outer <- function(payload, content) {
  jsonlite::toJSON(
    list(
      model = payload$model,
      created_at = "2026-06-19T12:00:00Z",
      message = list(
        role = "assistant",
        content = content
      ),
      done = TRUE,
      done_reason = "stop"
    ),
    auto_unbox = TRUE,
    null = "null"
  )
}

test_that("cluster_label_registry flattens batch output into plotting-ready fields", {
  x <- .build_registry_test_cocktail()
  ev1 <- cluster_evidence(x, "c_1", top_n_phi = 3, n_prototype_plots = 2, n_borderline_plots = 2)
  ev2 <- cluster_evidence(x, "c_2", top_n_phi = 3, n_prototype_plots = 2, n_borderline_plots = 2)
  out1 <- .build_registry_valid_output(ev1)
  out2 <- .build_registry_abstain_output(ev2)
  val1 <- validate_cluster_label(out1, ev1)
  val2 <- validate_cluster_label(out2, ev2)
  review_dir <- file.path(tempdir(), "cocktailr-registry-manual")
  dir.create(review_dir, recursive = TRUE, showWarnings = FALSE)
  review1 <- file.path(review_dir, "c_1_review.md")
  review2 <- file.path(review_dir, "c_2_review.md")
  writeLines("# c_1", review1, useBytes = TRUE)
  writeLines("# c_2", review2, useBytes = TRUE)

  llm1 <- list(
    provider = "ollama",
    model = "fake-model",
    variant = "label_primary_v1",
    workflow_steps = 3L,
    output = out1
  )
  class(llm1) <- c("cluster_label_result", "list")

  llm2 <- list(
    provider = "ollama",
    model = "fake-model",
    variant = "label_primary_v1",
    workflow_steps = 3L,
    output = out2
  )
  class(llm2) <- c("cluster_label_result", "list")

  run <- list(
    summary = data.frame(
      cluster = c("c_1", "c_2"),
      stringsAsFactors = FALSE
    ),
    results = list(
      list(
        evidence = ev1,
        validation = val1,
        llm_result = llm1,
        review = list(file = review1),
        run_status = "success",
        used_placeholder = FALSE,
        repair_used = FALSE,
        iterations_used = 1L,
        num_predict_used = 600L
      ),
      list(
        evidence = ev2,
        validation = val2,
        llm_result = llm2,
        review = list(file = review2),
        run_status = "success",
        used_placeholder = FALSE,
        repair_used = FALSE,
        iterations_used = 1L,
        num_predict_used = 600L
      )
    ),
    selection = data.frame(stringsAsFactors = FALSE)
  )
  class(run) <- c("cluster_label_batch_result", "list")

  reg <- cluster_label_registry(run)

  expect_s3_class(reg, "cluster_label_registry")
  expect_true(inherits(reg, "data.frame"))
  expect_equal(nrow(reg), 2L)
  expect_equal(reg$cluster, c("c_1", "c_2"))
  expect_equal(reg$selection_rank, c(1L, 2L))
  expect_true(all(c(
    "plot_label_id", "plot_label_short", "legend_label", "display_label",
    "canonical_label", "label_available", "accepted_label", "review_status",
    "model", "variant", "workflow_steps", "review_file"
  ) %in% names(reg)))

  row1 <- reg[reg$cluster == "c_1", , drop = FALSE]
  row2 <- reg[reg$cluster == "c_2", , drop = FALSE]

  expect_equal(row1$plot_label_id[[1]], "c_1")
  expect_equal(row1$plot_label_short[[1]], "sp1-sp2 cluster")
  expect_equal(row1$legend_label[[1]], "c_1: sp1-sp2 cluster")
  expect_equal(row1$display_label[[1]], "sp1-sp2 cluster")
  expect_equal(row1$canonical_label[[1]], "sp1_sp2_cluster")
  expect_true(row1$label_available[[1]])
  expect_true(row1$accepted_label[[1]])
  expect_equal(row1$review_status[[1]], "accepted")
  expect_equal(row1$model[[1]], "fake-model")
  expect_equal(row1$variant[[1]], "label_primary_v1")
  expect_equal(row1$workflow_steps[[1]], 3L)
  expect_true(file.exists(row1$review_file[[1]]))

  expect_equal(row2$plot_label_id[[1]], "c_2")
  expect_equal(row2$plot_label_short[[1]], "c_2")
  expect_equal(row2$legend_label[[1]], "c_2: [abstained]")
  expect_true(is.na(row2$display_label[[1]]))
  expect_true(is.na(row2$canonical_label[[1]]))
  expect_false(row2$label_available[[1]])
  expect_false(row2$accepted_label[[1]])
  expect_equal(row2$output_status[[1]], "abstain")
  expect_equal(row2$review_status[[1]], "abstained")
  expect_true(file.exists(row2$review_file[[1]]))
})

test_that("cluster_label_registry exposes post-abstain public fallback separately from model output", {
  x <- .build_registry_test_cocktail()
  ev <- cluster_evidence(x, "c_1", top_n_phi = 3, n_prototype_plots = 2, n_borderline_plots = 2)
  out <- .build_registry_abstain_output(ev)
  val <- validate_cluster_label(out, ev)

  llm <- list(
    provider = "ollama",
    model = "fake-model",
    variant = "label_primary_v1",
    workflow_steps = 3L,
    output = out,
    workflow = list(
      label = list(
        selected_public_variant = "selection_all_abstain",
        exhausted = TRUE,
        selection_output = list(
          abstain_reason = out$abstain_reason
        ),
        failure_messages = c(
          "label_primary_v1: abstained",
          "label_soft_v1: abstained",
          "label_broad_v1: abstained"
        )
      )
    )
  )
  class(llm) <- c("cluster_label_result", "list")

  batch <- list(
    summary = data.frame(
      cluster = "c_1",
      stringsAsFactors = FALSE
    ),
    results = list(
      list(
        evidence = ev,
        validation = val,
        llm_result = llm,
        review = list(file = "temp/reports/cluster_reviews/demo/c_1_review.md"),
        run_status = "success",
        used_placeholder = FALSE,
        repair_used = FALSE,
        iterations_used = 1L,
        num_predict_used = 600L
      )
    ),
    selection = data.frame(stringsAsFactors = FALSE)
  )
  class(batch) <- c("cluster_label_batch_result", "list")

  reg <- cluster_label_registry(batch)
  row <- reg[1, , drop = FALSE]

  expect_equal(row$output_status[[1]], "abstain")
  expect_true(is.na(row$display_label[[1]]))
  expect_true(is.na(row$canonical_label[[1]]))
  expect_equal(row$public_display_label[[1]], "Chaotic Cluster")
  expect_equal(row$public_canonical_label[[1]], "chaotic_cluster")
  expect_equal(row$public_label_source[[1]], "post_abstain_fallback")
  expect_false(row$label_available[[1]])
  expect_equal(row$plot_label_short[[1]], "Chaotic Cluster")
  expect_equal(row$legend_label[[1]], "c_1: Chaotic Cluster")
  expect_equal(row$review_status[[1]], "abstained")
})

test_that("cluster_label_registry returns a typed empty registry for empty batch results", {
  empty <- list(
    summary = data.frame(stringsAsFactors = FALSE),
    results = list(),
    selection = data.frame(stringsAsFactors = FALSE)
  )
  class(empty) <- c("cluster_label_batch_result", "list")

  reg <- cluster_label_registry(empty)

  expect_s3_class(reg, "cluster_label_registry")
  expect_true(inherits(reg, "data.frame"))
  expect_equal(nrow(reg), 0L)
  expect_true(all(c("cluster", "legend_label", "review_file") %in% names(reg)))
})

test_that("cluster_label_registry carries speculative plotting metadata", {
  x <- .build_registry_test_cocktail()
  ev1 <- cluster_evidence(x, "c_1", top_n_phi = 3, n_prototype_plots = 2, n_borderline_plots = 2)
  ev2 <- cluster_evidence(x, "c_2", top_n_phi = 3, n_prototype_plots = 2, n_borderline_plots = 2)

  out1 <- .build_registry_valid_output(ev1)
  out2 <- .build_registry_valid_output(ev2)
  out2$confidence$score <- 0
  out2$confidence$rationale <- "Tentative only; the evidence gives direction but not stability."
  out2$not_confirmed_by_data <- list(
    list(
      statement = "A habitat-level label is not confirmed.",
      reason = "Contrast against nearby alternatives is still too weak."
    )
  )

  val1 <- validate_cluster_label(out1, ev1)
  val2 <- validate_cluster_label(out2, ev2)
  val2 <- cocktailr:::.mark_speculative_validation(
    val2,
    strict_validation_status = "unsupported_claims"
  )

  llm1 <- list(
    provider = "ollama",
    model = "fake-model",
    variant = "label_primary_v1",
    workflow_steps = 1L,
    output = out1
  )
  class(llm1) <- c("cluster_label_result", "list")

  llm2 <- list(
    provider = "ollama",
    model = "fake-model",
    variant = "speculative_fallback_v1_after_rejection",
    workflow_steps = 1L,
    output = out2
  )
  class(llm2) <- c("cluster_label_result", "list")

  batch <- list(
    summary = data.frame(
      cluster = c("c_1", "c_2"),
      stringsAsFactors = FALSE
    ),
    results = list(
      list(
        evidence = ev1,
        validation = val1,
        llm_result = llm1,
        review = list(file = "temp/reports/cluster_reviews/demo/c_1_review.md"),
        run_status = "success",
        used_placeholder = FALSE,
        repair_used = FALSE,
        iterations_used = 1L,
        num_predict_used = 600L
      ),
      list(
        evidence = ev2,
        validation = val2,
        llm_result = llm2,
        review = list(file = "temp/reports/cluster_reviews/demo/c_2_review.md"),
        run_status = "speculative",
        label_tier = "speculative",
        is_speculative = TRUE,
        strict_outcome = "placeholder",
        strict_validation_status = "unsupported_claims",
        used_placeholder = FALSE,
        repair_used = FALSE,
        iterations_used = 1L,
        num_predict_used = 600L
      )
    ),
    selection = data.frame(stringsAsFactors = FALSE)
  )
  class(batch) <- c("cluster_label_batch_result", "list")

  reg <- cluster_label_registry(batch)
  row2 <- reg[reg$cluster == "c_2", , drop = FALSE]

  expect_equal(row2$review_status[[1]], "speculative")
  expect_equal(row2$label_tier[[1]], "speculative")
  expect_true(row2$is_speculative[[1]])
  expect_equal(row2$plot_marker[[1]], "*")
  expect_true(row2$label_available[[1]])
  expect_false(row2$accepted_label[[1]])
  expect_equal(row2$plot_label_short[[1]], "sp1-sp2 cluster*")
  expect_equal(row2$legend_label[[1]], "c_2: sp1-sp2 cluster*")
  expect_equal(row2$strict_outcome[[1]], "placeholder")
  expect_equal(row2$strict_validation_status[[1]], "unsupported_claims")
  expect_match(row2$missing_for_confidence_text[[1]], "not confirmed", ignore.case = TRUE)
})

