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

.build_registry_abstain_output <- function(ev) {
  topo <- ev$summaries$species_topological
  phi <- ev$summaries$species_phi

  list(
    schema_version = "0.1.0",
    cluster_id = ev$meta$cluster_id,
    status = "abstain",
    canonical_label = NULL,
    display_label = NULL,
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
    abstain_reason = "Distinctiveness is insufficient for a stable label."
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

test_that("cluster_label_registry flattens label_clusters output into plotting-ready fields", {
  x <- .build_registry_test_cocktail()
  ev1 <- cluster_evidence(x, "c_1", top_n_phi = 3, n_prototype_plots = 2, n_borderline_plots = 2)
  ev2 <- cluster_evidence(x, "c_2", top_n_phi = 3, n_prototype_plots = 2, n_borderline_plots = 2)
  out1 <- .build_registry_valid_output(ev1)
  out2 <- .build_registry_abstain_output(ev2)

  outputs <- list(
    c_1 = jsonlite::toJSON(out1, auto_unbox = TRUE, null = "null"),
    c_2 = jsonlite::toJSON(out2, auto_unbox = TRUE, null = "null")
  )

  fake_request <- function(url, payload, timeout_sec) {
    cluster_id_match <- regmatches(
      payload$messages[[2]]$content,
      regexpr("c_[0-9]+", payload$messages[[2]]$content, perl = TRUE)
    )
    cluster_id <- if (length(cluster_id_match)) cluster_id_match[[1]] else NA_character_

    list(
      status_code = 200L,
      body_text = .registry_outer(payload, outputs[[cluster_id]]),
      parsed = NULL
    )
  }

  review_dir <- file.path(tempdir(), "cocktailr-registry")
  unlink(review_dir, recursive = TRUE, force = TRUE)

  run <- label_clusters(
    x = x,
    clusters = c("c_1", "c_2"),
    model = "fake-model",
    variant = "strict_abstention_gate_v1",
    timeout_sec = 1,
    review_dir = review_dir,
    verbose = FALSE,
    request_fn = fake_request
  )

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
  expect_equal(row1$variant[[1]], "strict_abstention_gate_v1")
  expect_equal(row1$workflow_steps[[1]], 1L)
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
