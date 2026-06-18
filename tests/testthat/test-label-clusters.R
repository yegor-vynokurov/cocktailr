.build_label_clusters_test_cocktail <- function() {
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

.build_label_clusters_valid_output <- function(ev) {
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

.llm_outer <- function(payload, content) {
  jsonlite::toJSON(
    list(
      model = payload$model,
      created_at = "2026-06-18T12:00:00Z",
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

test_that("label_clusters runs a one-cluster workflow and writes a review card", {
  x <- .build_label_clusters_test_cocktail()
  ev <- cluster_evidence(x, "c_1", top_n_phi = 3, n_prototype_plots = 2, n_borderline_plots = 2)
  out_valid <- .build_label_clusters_valid_output(ev)

  fake_request <- function(url, payload, timeout_sec) {
    list(
      status_code = 200L,
      body_text = .llm_outer(
        payload,
        jsonlite::toJSON(out_valid, auto_unbox = TRUE, null = "null")
      ),
      parsed = NULL
    )
  }

  review_dir <- file.path(tempdir(), "cocktailr-label-clusters-basic")
  unlink(review_dir, recursive = TRUE, force = TRUE)

  res <- label_clusters(
    x = x,
    clusters = "c_1",
    model = "fake-model",
    variant = "strict_abstention_gate_v1",
    timeout_sec = 1,
    review_dir = review_dir,
    verbose = FALSE,
    request_fn = fake_request
  )

  expect_s3_class(res, "cluster_label_batch_result")
  expect_equal(nrow(res$summary), 1L)
  expect_equal(res$summary$cluster[[1]], "c_1")
  expect_equal(res$summary$run_status[[1]], "success")
  expect_equal(res$summary$validation_status[[1]], "valid")
  expect_false(res$summary$used_placeholder[[1]])
  expect_true(file.exists(res$summary$review_file[[1]]))
  expect_match(
    paste(readLines(res$summary$review_file[[1]], warn = FALSE), collapse = "\n"),
    "Model: `fake-model`",
    fixed = TRUE
  )
})

test_that("label_clusters performs one validator-guided repair pass", {
  x <- .build_label_clusters_test_cocktail()
  ev <- cluster_evidence(x, "c_1", top_n_phi = 3, n_prototype_plots = 2, n_borderline_plots = 2)
  out_invalid <- .build_label_clusters_valid_output(ev)
  out_invalid$canonical_label <- "dry_grassland_cluster"
  out_invalid$display_label <- "dry grassland cluster"
  out_invalid$interpretation_summary <- "The cluster looks like a dry grassland assemblage based on its compact species core."

  out_valid <- .build_label_clusters_valid_output(ev)

  state <- new.env(parent = emptyenv())
  state$n <- 0L
  state$message_counts <- integer(0)

  fake_request <- function(url, payload, timeout_sec) {
    state$n <- state$n + 1L
    state$message_counts[state$n] <- length(payload$messages)

    content <- if (state$n == 1L) {
      jsonlite::toJSON(out_invalid, auto_unbox = TRUE, null = "null")
    } else {
      jsonlite::toJSON(out_valid, auto_unbox = TRUE, null = "null")
    }

    list(
      status_code = 200L,
      body_text = .llm_outer(payload, content),
      parsed = NULL
    )
  }

  review_dir <- file.path(tempdir(), "cocktailr-label-clusters-repair")
  unlink(review_dir, recursive = TRUE, force = TRUE)

  res <- label_clusters(
    x = x,
    clusters = "c_1",
    model = "fake-model",
    variant = "strict_abstention_gate_v1",
    timeout_sec = 1,
    review_dir = review_dir,
    verbose = FALSE,
    request_fn = fake_request
  )

  expect_equal(state$n, 2L)
  expect_equal(state$message_counts[[1]], 2L)
  expect_equal(state$message_counts[[2]], 4L)
  expect_equal(res$summary$run_status[[1]], "success")
  expect_equal(res$summary$validation_status[[1]], "valid")
  expect_true(res$summary$repair_used[[1]])
  expect_equal(res$summary$iterations_used[[1]], 2L)
  expect_false(res$summary$used_placeholder[[1]])
})

test_that("label_clusters doubles num_predict after EOF-like failures", {
  x <- .build_label_clusters_test_cocktail()
  ev <- cluster_evidence(x, "c_1", top_n_phi = 3, n_prototype_plots = 2, n_borderline_plots = 2)
  out_valid <- .build_label_clusters_valid_output(ev)

  state <- new.env(parent = emptyenv())
  state$num_predict <- integer(0)

  fake_request <- function(url, payload, timeout_sec) {
    np <- if (!is.null(payload$options$num_predict)) {
      as.integer(payload$options$num_predict)
    } else {
      NA_integer_
    }
    state$num_predict <- c(state$num_predict, np)

    content <- if (is.na(np) || np < 2400L) {
      "{\n  \"schema_version\": \"0.1.0\", "
    } else {
      jsonlite::toJSON(out_valid, auto_unbox = TRUE, null = "null")
    }

    list(
      status_code = 200L,
      body_text = .llm_outer(payload, content),
      parsed = NULL
    )
  }

  review_dir <- file.path(tempdir(), "cocktailr-label-clusters-eof")
  unlink(review_dir, recursive = TRUE, force = TRUE)

  res <- label_clusters(
    x = x,
    clusters = "c_1",
    model = "fake-model",
    variant = "strict_abstention_gate_v1",
    timeout_sec = 1,
    review_dir = review_dir,
    verbose = FALSE,
    request_fn = fake_request
  )

  expect_true(any(state$num_predict == 1200L))
  expect_true(any(state$num_predict == 2400L))
  expect_equal(res$summary$run_status[[1]], "success")
  expect_equal(res$summary$iterations_used[[1]], 2L)
  expect_equal(res$summary$num_predict_used[[1]], 2400L)
  expect_false(res$summary$used_placeholder[[1]])
})

test_that("label_clusters writes a placeholder review card after bounded failure", {
  x <- .build_label_clusters_test_cocktail()

  fake_request <- function(url, payload, timeout_sec) {
    list(
      status_code = 200L,
      body_text = .llm_outer(payload, "{\"status\":\"bad\"}"),
      parsed = NULL
    )
  }

  review_dir <- file.path(tempdir(), "cocktailr-label-clusters-placeholder")
  unlink(review_dir, recursive = TRUE, force = TRUE)

  res <- label_clusters(
    x = x,
    clusters = "c_1",
    model = "fake-model",
    variant = "strict_abstention_gate_v1",
    timeout_sec = 1,
    review_dir = review_dir,
    verbose = FALSE,
    request_fn = fake_request
  )

  review_text <- paste(readLines(res$summary$review_file[[1]], warn = FALSE), collapse = "\n")

  expect_equal(res$summary$run_status[[1]], "placeholder")
  expect_true(res$summary$used_placeholder[[1]])
  expect_true(file.exists(res$summary$review_file[[1]]))
  expect_match(review_text, "No data: no valid structured cluster label could be obtained", fixed = TRUE)
})

test_that("label_clusters emits progress messages when verbose = TRUE", {
  x <- .build_label_clusters_test_cocktail()
  ev <- cluster_evidence(x, "c_1", top_n_phi = 3, n_prototype_plots = 2, n_borderline_plots = 2)
  out_valid <- .build_label_clusters_valid_output(ev)

  fake_request <- function(url, payload, timeout_sec) {
    list(
      status_code = 200L,
      body_text = .llm_outer(
        payload,
        jsonlite::toJSON(out_valid, auto_unbox = TRUE, null = "null")
      ),
      parsed = NULL
    )
  }

  review_dir <- file.path(tempdir(), "cocktailr-label-clusters-verbose")
  unlink(review_dir, recursive = TRUE, force = TRUE)

  msgs <- testthat::capture_messages(
    label_clusters(
      x = x,
      clusters = "c_1",
      model = "fake-model",
      variant = "strict_abstention_gate_v1",
      timeout_sec = 1,
      review_dir = review_dir,
      verbose = TRUE,
      request_fn = fake_request
    )
  )

  expect_true(any(grepl("Auto-selected|Processing 1 requested cluster", msgs)))
  expect_true(any(grepl("Cluster c_1 \\(1/1\\): building evidence\\.", msgs)))
  expect_true(any(grepl("Cluster c_1 \\(1/1\\): LLM started", msgs)))
  expect_true(any(grepl("Cluster c_1 \\(1/1\\): results saved to", msgs)))
})

test_that("label_clusters resolves a relative review_dir against the package source root", {
  x <- .build_label_clusters_test_cocktail()
  ev <- cluster_evidence(x, "c_1", top_n_phi = 3, n_prototype_plots = 2, n_borderline_plots = 2)
  out_valid <- .build_label_clusters_valid_output(ev)

  fake_request <- function(url, payload, timeout_sec) {
    list(
      status_code = 200L,
      body_text = .llm_outer(
        payload,
        jsonlite::toJSON(out_valid, auto_unbox = TRUE, null = "null")
      ),
      parsed = NULL
    )
  }

  pkg_root <- normalizePath(
    getNamespaceInfo(asNamespace("cocktailr"), "path"),
    winslash = "/",
    mustWork = TRUE
  )
  rel_dir <- file.path("temp", "testthat_label_clusters_relative")
  expected_root <- normalizePath(
    file.path(pkg_root, rel_dir),
    winslash = "/",
    mustWork = FALSE
  )

  unlink(expected_root, recursive = TRUE, force = TRUE)
  old_wd <- setwd(tempdir())
  on.exit(setwd(old_wd), add = TRUE)
  on.exit(unlink(expected_root, recursive = TRUE, force = TRUE), add = TRUE)

  res <- label_clusters(
    x = x,
    clusters = "c_1",
    model = "fake-model",
    variant = "strict_abstention_gate_v1",
    timeout_sec = 1,
    review_dir = rel_dir,
    verbose = FALSE,
    request_fn = fake_request
  )

  expect_true(file.exists(res$summary$review_file[[1]]))
  expect_true(startsWith(res$summary$review_file[[1]], expected_root))
})
