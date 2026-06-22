.build_test_cluster_evidence <- function() {
  vm <- matrix(
    c(
      55, 40,  0,  0,
      50, 35,  0,  0,
      25, 10, 30,  0,
       0,  0, 60, 45,
       0,  0, 50, 35,
       0,  0, 40, 25
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

test_that("llm_label_cluster assembles a dry-run Ollama request", {
  ev <- .build_test_cluster_evidence()

  req <- llm_label_cluster(
    evidence = ev,
    model = "gemma4:12b",
    variant = "concise_label_v1",
    dry_run = TRUE
  )

  expect_s3_class(req, "cluster_label_request")
  expect_equal(req$provider, "ollama")
  expect_equal(req$model, "gemma4:12b")
  expect_equal(req$variant, "concise_label_v1")
  expect_equal(req$request$model, "gemma4:12b")
  expect_false(req$request$stream)
  expect_false(req$request$think)
  expect_equal(req$request$options$temperature, 0.1)
  expect_equal(req$request$options$top_p, 0.9)
  expect_equal(req$request$options$seed, 42L)
  expect_equal(req$request$options$num_predict, 1200L)
  expect_equal(req$request$messages[[1]]$role, "system")
  expect_equal(req$request$messages[[2]]$role, "user")
  expect_true(file.exists(req$prompt$catalog_path))
  expect_true(file.exists(req$prompt$system_path))
  expect_true(file.exists(req$prompt$user_path))
  expect_match(req$request$messages[[2]]$content, "Cluster id:")
  expect_match(req$request$messages[[2]]$content, ev$meta$cluster_id, fixed = TRUE)
  expect_match(
    req$request$messages[[2]]$content,
    ev$meta$cluster_id,
    fixed = TRUE
  )
  expect_match(
    req$request$messages[[2]]$content,
    "Topological species:",
    fixed = TRUE
  )
  expect_match(
    req$request$messages[[2]]$content,
    "Cluster evidence:",
    fixed = TRUE
  )
  expect_equal(req$request$format$properties$schema_version$const, "0.1.0")
})

test_that("llm_label_cluster forwards additional Ollama options in dry-run mode", {
  ev <- .build_test_cluster_evidence()

  req <- llm_label_cluster(
    evidence = ev,
    model = "gemma4:12b",
    variant = "concise_label_v1",
    ollama_options = list(num_ctx = 8192L),
    dry_run = TRUE
  )

  expect_equal(req$request$options$num_ctx, 8192L)
  expect_equal(req$request$options$num_predict, 1200L)
})

test_that("prompt interpolation preserves replacements at template end", {
  out <- cocktailr:::.replace_fixed_scalar(
    "prefix {{TOKEN}}",
    "{{TOKEN}}",
    "suffix"
  )

  expect_equal(out, "prefix suffix")
})

test_that("llm_label_cluster assembles a two-step dry-run workflow", {
  ev <- .build_test_cluster_evidence()

  req <- llm_label_cluster(
    evidence = ev,
    model = "gemma4:12b",
    variant = "concise_label_v1",
    workflow_steps = 2L,
    dry_run = TRUE
  )

  expect_s3_class(req, "cluster_label_request")
  expect_equal(req$workflow_steps, 2L)
  expect_equal(req$workflow$gate$variant, "gate_abstain_examples_v1")
  expect_equal(req$workflow$label$variant, "concise_label_v1")
  expect_equal(req$workflow$gate$prompt$task_type, "gate")
  expect_equal(req$workflow$label$prompt$task_type, "label")
  expect_match(
    req$workflow$gate$request$messages[[2]]$content,
    "Negative examples for abstention:",
    fixed = TRUE
  )
})

test_that("llm_label_cluster parses a structured Ollama reply via request_fn", {
  ev <- .build_test_cluster_evidence()
  fixture_path <- test_path(
    "fixtures", "llm", "cluster_label_output_example_labeled.json"
  )
  fixture_text <- paste(readLines(fixture_path, warn = FALSE), collapse = "\n")

  fake_request <- function(url, payload, timeout_sec) {
    expect_match(url, "/api/chat$", perl = TRUE)
    expect_true(is.list(payload$format))
    expect_equal(payload$model, "fake-model")
    expect_true(timeout_sec >= 1)

    outer <- list(
      model = payload$model,
      created_at = "2026-06-17T12:00:00Z",
      message = list(
        role = "assistant",
        content = fixture_text
      ),
      done = TRUE,
      done_reason = "stop"
    )

    list(
      status_code = 200L,
      body_text = jsonlite::toJSON(outer, auto_unbox = TRUE, null = "null"),
      parsed = outer
    )
  }

  log_dir <- file.path(tempdir(), "cocktailr-llm-test")
  unlink(log_dir, recursive = TRUE, force = TRUE)

  res <- llm_label_cluster(
    evidence = ev,
    model = "fake-model",
    variant = "conservative_interpretation_v1",
    timeout_sec = 1,
    log_dir = log_dir,
    request_fn = fake_request
  )

  expect_s3_class(res, "cluster_label_result")
  expect_equal(res$cluster_id, "c_1")
  expect_equal(res$output$cluster_id, "c_1")
  expect_equal(res$output$status, "labeled")
  expect_equal(res$attempts, 1L)
  expect_true(dir.exists(res$logs$run_dir))
  expect_true(file.exists(res$logs$request))
  expect_true(file.exists(res$logs$metadata))
  expect_true(file.exists(res$logs$evidence))
  expect_true(file.exists(res$logs$system_prompt))
  expect_true(file.exists(res$logs$user_prompt))
  expect_true(file.exists(res$logs$output))
  expect_true(file.exists(paste0(res$logs$response_prefix, "_attempt1_envelope.json")))
  expect_true(file.exists(paste0(res$logs$response_content_prefix, "_attempt1.txt")))
})

test_that("llm_label_cluster resolves a relative log_dir against the package source root", {
  ev <- .build_test_cluster_evidence()
  fixture_path <- test_path(
    "fixtures", "llm", "cluster_label_output_example_labeled.json"
  )
  fixture_text <- paste(readLines(fixture_path, warn = FALSE), collapse = "\n")

  fake_request <- function(url, payload, timeout_sec) {
    outer <- list(
      model = payload$model,
      created_at = "2026-06-17T12:00:00Z",
      message = list(
        role = "assistant",
        content = fixture_text
      ),
      done = TRUE,
      done_reason = "stop"
    )

    list(
      status_code = 200L,
      body_text = jsonlite::toJSON(outer, auto_unbox = TRUE, null = "null"),
      parsed = outer
    )
  }

  pkg_root <- normalizePath(
    getNamespaceInfo(asNamespace("cocktailr"), "path"),
    winslash = "/",
    mustWork = TRUE
  )
  rel_dir <- file.path("temp", "testthat_llm_logs_relative")
  expected_root <- normalizePath(
    file.path(pkg_root, rel_dir),
    winslash = "/",
    mustWork = FALSE
  )

  unlink(expected_root, recursive = TRUE, force = TRUE)
  old_wd <- setwd(tempdir())
  on.exit(setwd(old_wd), add = TRUE)
  on.exit(unlink(expected_root, recursive = TRUE, force = TRUE), add = TRUE)

  res <- llm_label_cluster(
    evidence = ev,
    model = "fake-model",
    variant = "conservative_interpretation_v1",
    timeout_sec = 1,
    log_dir = rel_dir,
    request_fn = fake_request
  )

  expect_true(dir.exists(res$logs$run_dir))
  expect_true(startsWith(res$logs$run_dir, expected_root))
})

test_that("two-step workflow can abstain at the gate without labeling stage", {
  ev <- .build_test_cluster_evidence()

  gate_fixture <- list(
    schema_version = "0.1.0",
    cluster_id = "c_1",
    decision = "abstain",
    decision_summary = "The cluster core is coherent, but the available evidence is not distinctive enough to justify a stable label without stronger contrast.",
    confidence = list(
      score = 0.35,
      rationale = "The evidence supports a plausible broad story, but not a distinct label-worthy one."
    ),
    abstain_reason = "Distinctiveness is insufficient at the gate stage.",
    gate_checks = list(
      list(
        check = "core_coherence",
        result = "pass",
        reason = "The dominant species are internally consistent.",
        evidence_ids = c("E7", "E8")
      ),
      list(
        check = "distinctiveness",
        result = "fail",
        reason = "The current evidence does not separate this cluster clearly from nearby possibilities.",
        evidence_ids = c("E9", "E10")
      )
    ),
    key_species = list(
      list(species = "sp1", role = "topological", evidence_ids = c("E7")),
      list(species = "sp2", role = "phi_ranked", evidence_ids = c("E9"))
    ),
    not_confirmed_by_data = list(
      list(
        statement = "The cluster represents a specific habitat subtype.",
        reason = "That would require contrastive or environmental context not present in the bundle."
      )
    ),
    checks_to_run = list(
      list(
        check = "sibling_comparison",
        priority = "high",
        reason = "A nearby-cluster contrast would help resolve the gate."
      )
    ),
    ontology_slots = list(
      physiognomy = "woodland",
      moisture = NULL,
      light = "shaded",
      fertility = NULL,
      disturbance = NULL,
      dominant_taxa = c("sp1", "sp2")
    )
  )

  state <- new.env(parent = emptyenv())
  state$n <- 0L

  fake_request <- function(url, payload, timeout_sec) {
    state$n <- state$n + 1L
    expect_equal(payload$format$title, "cluster_label_gate")

    outer <- list(
      model = payload$model,
      created_at = "2026-06-17T12:00:00Z",
      message = list(
        role = "assistant",
        content = jsonlite::toJSON(gate_fixture, auto_unbox = TRUE, null = "null")
      ),
      done = TRUE,
      done_reason = "stop"
    )

    list(
      status_code = 200L,
      body_text = jsonlite::toJSON(outer, auto_unbox = TRUE, null = "null"),
      parsed = outer
    )
  }

  res <- llm_label_cluster(
    evidence = ev,
    model = "fake-model",
    variant = "concise_label_v1",
    workflow_steps = 2L,
    request_fn = fake_request
  )

  expect_equal(state$n, 1L)
  expect_s3_class(res, "cluster_label_result")
  expect_equal(res$workflow_steps, 2L)
  expect_equal(res$output$status, "abstain")
  expect_null(res$output$display_label)
  expect_equal(res$output$abstain_reason, "Distinctiveness is insufficient at the gate stage.")
  expect_equal(res$workflow$gate$output$decision, "abstain")
  expect_null(res$workflow$label)
})

test_that("two-step workflow resolves a relative log_dir against the package source root", {
  ev <- .build_test_cluster_evidence()

  gate_fixture <- list(
    schema_version = "0.1.0",
    cluster_id = "c_1",
    decision = "abstain",
    decision_summary = "The cluster should abstain at the gate.",
    confidence = list(
      score = 0.3,
      rationale = "Test fixture."
    ),
    abstain_reason = "Test abstention.",
    gate_checks = list(
      list(
        check = "distinctiveness",
        result = "fail",
        reason = "Test fixture.",
        evidence_ids = c("E1")
      )
    ),
    key_species = list(),
    not_confirmed_by_data = list(),
    checks_to_run = list(),
    ontology_slots = list(
      physiognomy = NULL,
      moisture = NULL,
      light = NULL,
      fertility = NULL,
      disturbance = NULL,
      dominant_taxa = list()
    )
  )

  fake_request <- function(url, payload, timeout_sec) {
    outer <- list(
      model = payload$model,
      created_at = "2026-06-17T12:00:00Z",
      message = list(
        role = "assistant",
        content = jsonlite::toJSON(gate_fixture, auto_unbox = TRUE, null = "null")
      ),
      done = TRUE,
      done_reason = "stop"
    )

    list(
      status_code = 200L,
      body_text = jsonlite::toJSON(outer, auto_unbox = TRUE, null = "null"),
      parsed = outer
    )
  }

  pkg_root <- normalizePath(
    getNamespaceInfo(asNamespace("cocktailr"), "path"),
    winslash = "/",
    mustWork = TRUE
  )
  rel_dir <- file.path("temp", "testthat_llm_logs_relative_w2")
  expected_root <- normalizePath(
    file.path(pkg_root, rel_dir),
    winslash = "/",
    mustWork = FALSE
  )

  unlink(expected_root, recursive = TRUE, force = TRUE)
  old_wd <- setwd(tempdir())
  on.exit(setwd(old_wd), add = TRUE)
  on.exit(unlink(expected_root, recursive = TRUE, force = TRUE), add = TRUE)

  res <- llm_label_cluster(
    evidence = ev,
    model = "fake-model",
    variant = "concise_label_v1",
    workflow_steps = 2L,
    log_dir = rel_dir,
    request_fn = fake_request
  )

  expect_true(dir.exists(res$logs$run_dir))
  expect_true(startsWith(res$logs$run_dir, expected_root))
  expect_true(dir.exists(res$logs$stages$gate$run_dir))
  expect_true(dir.exists(res$logs$stages$label$run_dir))
})

test_that("two-step workflow can pass the gate and then label", {
  ev <- .build_test_cluster_evidence()
  fixture_path <- test_path(
    "fixtures", "llm", "cluster_label_output_example_labeled.json"
  )
  fixture_text <- paste(readLines(fixture_path, warn = FALSE), collapse = "\n")

  gate_fixture <- list(
    schema_version = "0.1.0",
    cluster_id = "c_1",
    decision = "label",
    decision_summary = "The cluster appears coherent and distinctive enough to proceed to final labeling.",
    confidence = list(
      score = 0.6,
      rationale = "The cluster passes the basic gate, but the final wording should still be checked in stage 2."
    ),
    abstain_reason = NULL,
    gate_checks = list(
      list(
        check = "core_coherence",
        result = "pass",
        reason = "Core species align.",
        evidence_ids = c("E7", "E8")
      ),
      list(
        check = "distinctiveness",
        result = "pass",
        reason = "Prototype evidence is consistent enough to proceed.",
        evidence_ids = c("E9", "E10")
      )
    ),
    key_species = list(
      list(species = "sp1", role = "topological", evidence_ids = c("E7"))
    ),
    not_confirmed_by_data = list(),
    checks_to_run = list(
      list(
        check = "final_label_review",
        priority = "medium",
        reason = "Stage 2 should confirm the safest label wording."
      )
    ),
    ontology_slots = list(
      physiognomy = "woodland",
      moisture = NULL,
      light = "shaded",
      fertility = NULL,
      disturbance = NULL,
      dominant_taxa = c("sp1")
    )
  )

  state <- new.env(parent = emptyenv())
  state$n <- 0L
  state$formats <- character(0)

  fake_request <- function(url, payload, timeout_sec) {
    state$n <- state$n + 1L
    state$formats[state$n] <- payload$format$title

    content <- if (identical(payload$format$title, "cluster_label_gate")) {
      jsonlite::toJSON(gate_fixture, auto_unbox = TRUE, null = "null")
    } else {
      fixture_text
    }

    outer <- list(
      model = payload$model,
      created_at = "2026-06-17T12:00:00Z",
      message = list(
        role = "assistant",
        content = content
      ),
      done = TRUE,
      done_reason = "stop"
    )

    list(
      status_code = 200L,
      body_text = jsonlite::toJSON(outer, auto_unbox = TRUE, null = "null"),
      parsed = outer
    )
  }

  res <- llm_label_cluster(
    evidence = ev,
    model = "fake-model",
    variant = "conservative_interpretation_v1",
    workflow_steps = 2L,
    request_fn = fake_request
  )

  expect_equal(state$n, 2L)
  expect_equal(state$formats[[1]], "cluster_label_gate")
  expect_equal(state$formats[[2]], "cluster_label_output")
  expect_equal(res$workflow$gate$output$decision, "label")
  expect_equal(res$output$status, "labeled")
  expect_equal(res$workflow$label$output$status, "labeled")
})

test_that("llm_label_cluster retries with a repair message after malformed output", {
  ev <- .build_test_cluster_evidence()
  fixture_path <- test_path(
    "fixtures", "llm", "cluster_label_output_example_labeled.json"
  )
  fixture_text <- paste(readLines(fixture_path, warn = FALSE), collapse = "\n")

  state <- new.env(parent = emptyenv())
  state$n <- 0L
  state$payloads <- list()

  fake_request <- function(url, payload, timeout_sec) {
    state$n <- state$n + 1L
    state$payloads[[state$n]] <- payload

    content <- if (state$n == 1L) {
      "{\"status\":\"bad\"}"
    } else {
      fixture_text
    }

    outer <- list(
      model = payload$model,
      created_at = "2026-06-17T12:00:00Z",
      message = list(
        role = "assistant",
        content = content
      ),
      done = TRUE,
      done_reason = "stop"
    )

    list(
      status_code = 200L,
      body_text = jsonlite::toJSON(outer, auto_unbox = TRUE, null = "null"),
      parsed = outer
    )
  }

  res <- llm_label_cluster(
    evidence = ev,
    model = "fake-model",
    max_retries = 1L,
    request_fn = fake_request
  )

  expect_equal(state$n, 2L)
  expect_equal(res$attempts, 2L)
  expect_length(state$payloads[[2]]$messages, 4L)
  expect_equal(state$payloads[[2]]$messages[[3]]$role, "assistant")
  expect_equal(state$payloads[[2]]$messages[[4]]$role, "user")
  expect_match(
    state$payloads[[2]]$messages[[4]]$content,
    "Return one repaired JSON object only.",
    fixed = TRUE
  )
})
