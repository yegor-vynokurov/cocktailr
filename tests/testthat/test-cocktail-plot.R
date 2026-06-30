.build_cocktail_plot_test_cocktail <- function() {
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

.build_cocktail_plot_valid_output <- function(ev) {
  topo <- ev$summaries$species_topological
  phi <- ev$summaries$species_phi
  cover <- ev$summaries$cover_summary
  proto <- ev$summaries$plots_prototype

  list(
    schema_version = "0.1.0",
    cluster_id = ev$meta$cluster_id,
    status = "labeled",
    canonical_label = paste0(gsub("^c_", "", ev$meta$cluster_id), "_cluster"),
    display_label = paste0("label for ", ev$meta$cluster_id),
    interpretation_summary = "Compact test label.",
    basis_in_data = list(
      list(
        claim_id = "C1",
        statement = "Topological species recur across the cluster core.",
        evidence_ids = c("E4", topo$evidence_id[[1]]),
        support_strength = "strong"
      ),
      list(
        claim_id = "C2",
        statement = "Cover summaries reinforce the same pattern.",
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
      rationale = "Test confidence."
    ),
    checks_to_run = list(
      list(
        check = "Compare against neighboring clusters if needed.",
        priority = "medium",
        reason = "Test check."
      )
    ),
    abstain_reason = NULL
  )
}

.build_cocktail_plot_speculative_output <- function(ev) {
  out <- .build_cocktail_plot_valid_output(ev)
  out$canonical_label <- paste0("possible_", gsub("^c_", "", ev$meta$cluster_id), "_cluster")
  out$display_label <- paste0("possible label for ", ev$meta$cluster_id)
  out$interpretation_summary <- "Tentative orientation label after strict failure."
  out$not_confirmed_by_data <- list(
    list(
      statement = "A stable habitat-level label is not confirmed.",
      reason = "The available evidence is directional, but still too weak for a stable accepted label."
    )
  )
  out$confidence <- list(
    score = 0,
    rationale = "Tentative only."
  )
  out$checks_to_run <- list(
    list(
      check = "Compare against neighboring clusters.",
      priority = "high",
      reason = "Additional contrastive evidence is needed."
    )
  )
  out
}

.cocktail_plot_llm_outer <- function(payload, content) {
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

test_that("cocktail plot legend lines include shared review folder and filenames", {
  reg <- data.frame(
    cluster = c("c_1", "c_2"),
    legend_label = c("c_1: mesic woodland", "c_2: wet meadow edge"),
    review_file = c(
      "temp/reports/cluster_reviews/my_dataset/c_1_review.md",
      "temp/reports/cluster_reviews/my_dataset/c_2_review.md"
    ),
    stringsAsFactors = FALSE
  )

  lines <- cocktailr:::.cocktail_plot_legend_lines(reg, max_entries = 5L)

  expect_equal(lines[[1]], "Label reviews: temp/reports/cluster_reviews/my_dataset/")
  expect_match(lines[[2]], "c_1: mesic woodland \\(c_1_review.md\\)")
  expect_match(lines[[3]], "c_2: wet meadow edge \\(c_2_review.md\\)")
})

test_that("cocktail plot legend lines explain speculative star markers", {
  reg <- data.frame(
    cluster = c("c_1", "c_2"),
    legend_label = c("c_1: mesic woodland", "c_2: wet meadow edge*"),
    review_file = c(
      "temp/reports/cluster_reviews/my_dataset/c_1_review.md",
      "temp/reports/cluster_reviews/my_dataset/c_2_review.md"
    ),
    is_speculative = c(FALSE, TRUE),
    plot_marker = c(NA_character_, "*"),
    review_status = c("accepted", "speculative"),
    stringsAsFactors = FALSE
  )

  lines <- cocktailr:::.cocktail_plot_legend_lines(reg, max_entries = 5L)

  expect_true(any(grepl("tentative / speculative label", lines, fixed = TRUE)))
})

test_that("cocktail_plot accepts a label registry and still writes a png", {
  x <- .build_cocktail_plot_test_cocktail()
  ev1 <- cluster_evidence(x, "c_1", top_n_phi = 3, n_prototype_plots = 2, n_borderline_plots = 2)
  ev2 <- cluster_evidence(x, "c_2", top_n_phi = 3, n_prototype_plots = 2, n_borderline_plots = 2)
  out1 <- .build_cocktail_plot_valid_output(ev1)
  out2 <- .build_cocktail_plot_valid_output(ev2)

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
      body_text = .cocktail_plot_llm_outer(payload, outputs[[cluster_id]]),
      parsed = NULL
    )
  }

  review_dir <- file.path(tempdir(), "cocktailr-cocktail-plot-registry")
  unlink(review_dir, recursive = TRUE, force = TRUE)

  run <- label_clusters(
    x = x,
    clusters = c("c_1", "c_2"),
    model = "fake-model",
    variant = "label_primary_v1",
    timeout_sec = 1,
    review_dir = review_dir,
    labels_for_imgs = TRUE,
    verbose = FALSE,
    request_fn = fake_request
  )

  out_file <- file.path(tempdir(), "cocktailr_plot_with_registry.png")
  unlink(out_file, force = TRUE)

  expect_invisible(
    cocktail_plot(
      x = x,
      file = out_file,
      clusters = c("c_1", "c_2"),
      label_clusters = TRUE,
      label_registry = run$label_registry,
      cex_species = 0.8
    )
  )
  expect_true(file.exists(out_file))
})

test_that("cocktail_plot can auto-load a saved label registry", {
  x <- .build_cocktail_plot_test_cocktail()
  x$dataset <- list(
    label = "plot_auto_registry_test",
    type = "synthetic"
  )

  ev1 <- cluster_evidence(x, "c_1", top_n_phi = 3, n_prototype_plots = 2, n_borderline_plots = 2)
  ev2 <- cluster_evidence(x, "c_2", top_n_phi = 3, n_prototype_plots = 2, n_borderline_plots = 2)
  out1 <- .build_cocktail_plot_valid_output(ev1)
  out2 <- .build_cocktail_plot_valid_output(ev2)

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
      body_text = .cocktail_plot_llm_outer(payload, outputs[[cluster_id]]),
      parsed = NULL
    )
  }

  review_root <- cocktailr:::.resolve_cocktailr_output_path(
    file.path("temp", "reports", "cluster_reviews")
  )
  expected_dir <- file.path(review_root, "plot_auto_registry_test")
  unlink(expected_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(expected_dir, recursive = TRUE, force = TRUE), add = TRUE)

  run <- label_clusters(
    x = x,
    clusters = c("c_1", "c_2"),
    model = "fake-model",
    variant = "label_primary_v1",
    timeout_sec = 1,
    labels_for_imgs = TRUE,
    verbose = FALSE,
    request_fn = fake_request
  )

  expect_true(file.exists(run$label_registry_file))
  expect_true(startsWith(run$label_registry_file, normalizePath(expected_dir, winslash = "/", mustWork = FALSE)))

  out_file <- file.path(tempdir(), "cocktailr_plot_with_auto_registry.png")
  unlink(out_file, force = TRUE)

  expect_invisible(
    cocktail_plot(
      x = x,
      file = out_file,
      clusters = c("c_1", "c_2"),
      label_clusters = TRUE,
      label_registry = "auto",
      cex_species = 0.8
    )
  )
  expect_true(file.exists(out_file))
})

test_that("auto-loaded speculative registries preserve starred labels", {
  x <- .build_cocktail_plot_test_cocktail()
  x$dataset <- list(
    label = "plot_auto_registry_speculative_test",
    type = "synthetic"
  )

  ev1 <- cluster_evidence(x, "c_1", top_n_phi = 3, n_prototype_plots = 2, n_borderline_plots = 2)
  ev2 <- cluster_evidence(x, "c_2", top_n_phi = 3, n_prototype_plots = 2, n_borderline_plots = 2)
  out1 <- .build_cocktail_plot_valid_output(ev1)
  out2 <- .build_cocktail_plot_speculative_output(ev2)

  fake_request <- function(url, payload, timeout_sec) {
    cluster_id_match <- regmatches(
      payload$messages[[2]]$content,
      regexpr("c_[0-9]+", payload$messages[[2]]$content, perl = TRUE)
    )
    cluster_id <- if (length(cluster_id_match)) cluster_id_match[[1]] else NA_character_
    is_speculative <- identical(payload$model %||% NA_character_, "phi4-mini:latest") ||
      any(vapply(payload$messages, function(msg) {
        grepl("Task mode: `label_soft_v1`", msg$content, fixed = TRUE) ||
          grepl("Task mode: `label_broad_v1`", msg$content, fixed = TRUE)
      }, logical(1)))

    content <- if (identical(cluster_id, "c_1")) {
      jsonlite::toJSON(out1, auto_unbox = TRUE, null = "null")
    } else if (identical(cluster_id, "c_2") && is_speculative) {
      jsonlite::toJSON(out2, auto_unbox = TRUE, null = "null")
    } else {
      "{\"status\":\"bad\"}"
    }

    list(
      status_code = 200L,
      body_text = .cocktail_plot_llm_outer(payload, content),
      parsed = NULL
    )
  }

  review_root <- cocktailr:::.resolve_cocktailr_output_path(
    file.path("temp", "reports", "cluster_reviews")
  )
  expected_dir <- file.path(review_root, "plot_auto_registry_speculative_test")
  unlink(expected_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(expected_dir, recursive = TRUE, force = TRUE), add = TRUE)

  run <- label_clusters(
    x = x,
    clusters = c("c_1", "c_2"),
    model = "fake-model",
    variant = "label_primary_v1",
    speculative_fallback_mode = "after_rejection",
    timeout_sec = 1,
    labels_for_imgs = TRUE,
    verbose = FALSE,
    request_fn = fake_request
  )

  expect_true(file.exists(run$label_registry_file))

  reg_auto <- cocktailr:::.load_cocktail_plot_label_registry_auto(x)
  row2 <- reg_auto[reg_auto$cluster == "c_2", , drop = FALSE]

  expect_true(row2$is_speculative[[1]])
  expect_equal(row2$review_status[[1]], "speculative")
  expect_equal(row2$plot_label_short[[1]], "possible label for c_2*")
  expect_equal(row2$legend_label[[1]], "c_2: possible label for c_2*")

  D <- cluster_phi_dist(x = x, clusters = c("c_1", "c_2"))
  hc <- hclust(D, method = "average")
  hc_labeled <- label_hclust_leaves(
    hc,
    label_registry = "auto",
    x = x,
    label_field = "plot_label_short",
    warn_missing = FALSE
  )

  expect_true(any(grepl("\\*$", hc_labeled$labels)))
})

test_that("label_hclust_leaves replaces leaf labels and preserves originals", {
  x <- .build_cocktail_plot_test_cocktail()
  D <- cluster_phi_dist(x = x, clusters = c("c_1", "c_2"))
  hc <- hclust(D, method = "average")

  reg <- data.frame(
    cluster = c("c_1", "c_2"),
    plot_label_short = c("mesic woodland", "wet meadow edge"),
    legend_label = c("c_1: mesic woodland", "c_2: wet meadow edge"),
    display_label = c("mesic woodland", "wet meadow edge"),
    review_file = c(
      "temp/reports/cluster_reviews/my_dataset/c_1_review.md",
      "temp/reports/cluster_reviews/my_dataset/c_2_review.md"
    ),
    stringsAsFactors = FALSE
  )

  hc_legend <- label_hclust_leaves(
    hc,
    label_registry = reg,
    label_field = "legend_label",
    warn_missing = FALSE
  )

  expect_equal(hc_legend$labels, reg$legend_label)
  expect_equal(attr(hc_legend, "cocktailr_original_labels"), c("c_1", "c_2"))

  hc_short <- label_hclust_leaves(
    hc_legend,
    label_registry = reg,
    label_field = "plot_label_short",
    warn_missing = FALSE
  )

  expect_equal(hc_short$labels, reg$plot_label_short)
  expect_equal(attr(hc_short, "cocktailr_original_labels"), c("c_1", "c_2"))
})

test_that("label_hclust_leaves can auto-load a saved registry", {
  x <- .build_cocktail_plot_test_cocktail()
  x$dataset <- list(
    label = "hclust_auto_registry_test",
    type = "synthetic"
  )

  ev1 <- cluster_evidence(x, "c_1", top_n_phi = 3, n_prototype_plots = 2, n_borderline_plots = 2)
  ev2 <- cluster_evidence(x, "c_2", top_n_phi = 3, n_prototype_plots = 2, n_borderline_plots = 2)
  out1 <- .build_cocktail_plot_valid_output(ev1)
  out2 <- .build_cocktail_plot_valid_output(ev2)

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
      body_text = .cocktail_plot_llm_outer(payload, outputs[[cluster_id]]),
      parsed = NULL
    )
  }

  review_root <- cocktailr:::.resolve_cocktailr_output_path(
    file.path("temp", "reports", "cluster_reviews")
  )
  expected_dir <- file.path(review_root, "hclust_auto_registry_test")
  unlink(expected_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(expected_dir, recursive = TRUE, force = TRUE), add = TRUE)

  run <- label_clusters(
    x = x,
    clusters = c("c_1", "c_2"),
    model = "fake-model",
    variant = "label_primary_v1",
    timeout_sec = 1,
    labels_for_imgs = TRUE,
    verbose = FALSE,
    request_fn = fake_request
  )

  expect_true(file.exists(run$label_registry_file))

  D <- cluster_phi_dist(x = x, clusters = c("c_1", "c_2"))
  hc <- hclust(D, method = "average")

  hc_labeled <- label_hclust_leaves(
    hc,
    label_registry = "auto",
    x = x,
    label_field = "legend_label",
    warn_missing = FALSE
  )

  expect_equal(hc_labeled$labels, c("c_1: label for c_1", "c_2: label for c_2"))
})

test_that("cluster_hclust_plot draws a one-call hclust figure", {
  x <- .build_cocktail_plot_test_cocktail()
  out_file <- file.path(tempdir(), "cluster_hclust_plot_basic.png")
  unlink(out_file, force = TRUE)

  reg <- data.frame(
    cluster = c("c_1", "c_2"),
    plot_label_short = c("mesic woodland", "wet meadow edge*"),
    legend_label = c("c_1: mesic woodland", "c_2: wet meadow edge*"),
    display_label = c("mesic woodland", "wet meadow edge"),
    review_file = c(
      "temp/reports/cluster_reviews/my_dataset/c_1_review.md",
      "temp/reports/cluster_reviews/my_dataset/c_2_review.md"
    ),
    is_speculative = c(FALSE, TRUE),
    plot_marker = c(NA_character_, "*"),
    review_status = c("accepted", "speculative"),
    stringsAsFactors = FALSE
  )

  res <- cluster_hclust_plot(
    x = x,
    clusters = c("c_1", "c_2"),
    label_registry = reg,
    file = out_file,
    warn_missing = FALSE,
    cex = 0.8
  )

  expect_true(file.exists(out_file))
  expect_equal(res$clusters, c("c_1", "c_2"))
  expect_s3_class(res$dist, "dist")
  expect_s3_class(res$hclust, "hclust")
  expect_s3_class(res$hclust_plot, "hclust")
  expect_equal(res$hclust_plot$labels, reg$plot_label_short)
})

test_that("cluster_hclust_plot can auto-load saved labels in one call", {
  x <- .build_cocktail_plot_test_cocktail()
  x$dataset <- list(
    label = "cluster_hclust_plot_auto_test",
    type = "synthetic"
  )

  ev1 <- cluster_evidence(x, "c_1", top_n_phi = 3, n_prototype_plots = 2, n_borderline_plots = 2)
  ev2 <- cluster_evidence(x, "c_2", top_n_phi = 3, n_prototype_plots = 2, n_borderline_plots = 2)
  out1 <- .build_cocktail_plot_valid_output(ev1)
  out2 <- .build_cocktail_plot_valid_output(ev2)

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
      body_text = .cocktail_plot_llm_outer(payload, outputs[[cluster_id]]),
      parsed = NULL
    )
  }

  review_root <- cocktailr:::.resolve_cocktailr_output_path(
    file.path("temp", "reports", "cluster_reviews")
  )
  expected_dir <- file.path(review_root, "cluster_hclust_plot_auto_test")
  unlink(expected_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(expected_dir, recursive = TRUE, force = TRUE), add = TRUE)

  run <- label_clusters(
    x = x,
    clusters = c("c_1", "c_2"),
    model = "fake-model",
    variant = "label_primary_v1",
    timeout_sec = 1,
    labels_for_imgs = TRUE,
    verbose = FALSE,
    request_fn = fake_request
  )

  expect_true(file.exists(run$label_registry_file))

  out_file <- file.path(tempdir(), "cluster_hclust_plot_auto.png")
  unlink(out_file, force = TRUE)

  res <- cluster_hclust_plot(
    x = x,
    clusters = c("c_1", "c_2"),
    label_registry = "auto",
    file = out_file,
    warn_missing = FALSE,
    cex = 0.8
  )

  expect_true(file.exists(out_file))
  expect_equal(res$hclust_plot$labels, c("label for c_1", "label for c_2"))
})

