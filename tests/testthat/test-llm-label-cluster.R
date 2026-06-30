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
    variant = "label_primary_v1",
    dry_run = TRUE
  )

  expect_s3_class(req, "cluster_label_request")
  expect_equal(req$provider, "ollama")
  expect_equal(req$model, "gemma4:12b")
  expect_equal(req$variant, "label_primary_v1")
  expect_equal(req$request$model, "gemma4:12b")
  expect_false(req$request$stream)
  expect_false(req$request$think)
  expect_equal(req$request$options$temperature, 0.0)
  expect_equal(req$request$options$top_p, 0.8)
  expect_equal(req$request$options$seed, 42L)
  expect_equal(req$request$options$num_predict, 2400L)
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
    variant = "label_primary_v1",
    ollama_options = list(num_ctx = 8192L),
    dry_run = TRUE
  )

  expect_equal(req$request$options$num_ctx, 8192L)
  expect_equal(req$request$options$num_predict, 2400L)
})

test_that("llm_label_cluster trims evidence blocks to fit prompt_budget_chars", {
  ev <- .build_test_cluster_evidence()

  full_req <- llm_label_cluster(
    evidence = ev,
    model = "gemma4:12b",
    variant = "label_primary_v1",
    prompt_budget_chars = NULL,
    dry_run = TRUE
  )

  small_budget <- full_req$prompt$evidence_budget$fixed_overhead_chars + 80L

  req <- llm_label_cluster(
    evidence = ev,
    model = "gemma4:12b",
    variant = "label_primary_v1",
    prompt_budget_chars = small_budget,
    dry_run = TRUE
  )

  budget <- req$prompt$evidence_budget
  cover_row <- budget$blocks[budget$blocks$id == "cover_summary", , drop = FALSE]

  expect_true(budget$trimmed)
  expect_lte(budget$total_prompt_chars, small_budget)
  expect_match(req$prompt$evidence_text, "Metrics:", fixed = TRUE)
  expect_false(grepl("Cover summary:", req$prompt$evidence_text, fixed = TRUE))
  expect_equal(cover_row$status[[1]], "dropped")
})

test_that("llm_label_cluster uses a compact schema prompt note", {
  ev <- .build_test_cluster_evidence()

  req <- llm_label_cluster(
    evidence = ev,
    model = "gemma4:12b",
    variant = "label_primary_v1",
    dry_run = TRUE
  )

  expect_lt(
    req$prompt$evidence_budget$schema_prompt_chars,
    req$prompt$evidence_budget$schema_text_chars
  )
  expect_match(
    req$request$messages[[2]]$content,
    "Structured output schema is attached separately",
    fixed = TRUE
  )
  expect_false(grepl("\"\\$schema\"", req$request$messages[[2]]$content))
})

test_that("cluster label vocabulary helper reads the packaged coarse vocabulary", {
  catalog <- cocktailr:::.read_cluster_label_prompt_catalog()
  vocab <- cocktailr:::.read_cluster_label_vocabulary(
    catalog,
    list(vocabulary_path = "vocabulary/coarse_label_vocabulary_core_v1.json")
  )

  expect_true(file.exists(vocab$path))
  expect_match(vocab$rendered, "Allowed labels:", fixed = TRUE)
  expect_match(vocab$rendered, "woodland_like_assemblage", fixed = TRUE)
  expect_match(vocab$rendered, "chaotic_plant_assemblage", fixed = TRUE)
})

test_that("cluster label vocabulary helper can override the packaged path via option", {
  vocab_path <- file.path(tempdir(), "cocktailr_custom_vocab.json")
  jsonlite::write_json(
    list(
      vocabulary_version = "test-1",
      vocabulary_name = "custom_test_vocab",
      labels = list(
        list(
          canonical_label = "custom_transition_label",
          display_label = "Custom Transition Label",
          short_description = "Custom broad label for test coverage.",
          use_when = "Use when the user supplied a custom dataset-specific label list."
        )
      )
    ),
    vocab_path,
    auto_unbox = TRUE,
    pretty = TRUE
  )

  old_opt <- options(cocktailr.cluster_label_vocabulary_path = vocab_path)
  on.exit(options(old_opt), add = TRUE)
  on.exit(unlink(vocab_path, force = TRUE), add = TRUE)

  catalog <- cocktailr:::.read_cluster_label_prompt_catalog()
  vocab <- cocktailr:::.read_cluster_label_vocabulary(
    catalog,
    list(vocabulary_path = "vocabulary/coarse_label_vocabulary_core_v1.json")
  )

  expect_equal(
    vocab$path,
    normalizePath(vocab_path, winslash = "/", mustWork = TRUE)
  )
  expect_match(vocab$rendered, "custom_transition_label", fixed = TRUE)
  expect_match(vocab$rendered, "Custom Transition Label", fixed = TRUE)
})

test_that("constrained label mode injects the coarse vocabulary into the prompt", {
  ev <- .build_test_cluster_evidence()

  req <- llm_label_cluster(
    evidence = ev,
    model = "gemma4:12b",
    variant = "label_soft_v1",
    label_mode = "constrained",
    dry_run = TRUE
  )

  expect_equal(req$prompt$label_mode_requested, "constrained")
  expect_equal(req$prompt$label_mode_effective, "constrained")
  expect_match(
    req$request$messages[[2]]$content,
    "Constrained label mode is active.",
    fixed = TRUE
  )
  expect_match(
    req$request$messages[[2]]$content,
    "Allowed labels:",
    fixed = TRUE
  )
  expect_match(
    req$request$messages[[2]]$content,
    "woodland_like_assemblage",
    fixed = TRUE
  )
})

test_that("dynamic label mode requires the staged three-step workflow", {
  ev <- .build_test_cluster_evidence()

  expect_error(
    llm_label_cluster(
      evidence = ev,
      model = "gemma4:12b",
      variant = "label_primary_v1",
      label_mode = "dynamic",
      workflow_steps = 1L,
      dry_run = TRUE
    ),
    "currently requires `workflow_steps = 3`",
    fixed = TRUE
  )
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
    variant = "label_primary_v1",
    workflow_steps = 2L,
    dry_run = TRUE
  )

  expect_s3_class(req, "cluster_label_request")
  expect_equal(req$workflow_steps, 2L)
  expect_equal(req$workflow$gate$variant, "gate_abstain_examples_v1")
  expect_equal(req$workflow$label$variant, "label_primary_v1")
  expect_equal(req$workflow$gate$prompt$task_type, "gate")
  expect_equal(req$workflow$label$prompt$task_type, "label")
  expect_match(
    req$workflow$gate$request$messages[[2]]$content,
    "Negative examples for abstention:",
    fixed = TRUE
  )
})

test_that("llm_label_cluster assembles a three-step dry-run workflow", {
  ev <- .build_test_cluster_evidence()

  req <- llm_label_cluster(
    evidence = ev,
    model = "gemma4:12b",
    variant = "label_primary_v1",
    workflow_steps = 3L,
    dry_run = TRUE
  )

  expect_s3_class(req, "cluster_label_request")
  expect_equal(req$workflow_steps, 3L)
  expect_equal(req$workflow$draft$variant, "draft_analysis_v1")
  expect_length(req$workflow$label$variants, 3L)
  expect_equal(req$workflow$label$variants[[1]]$variant, "label_primary_v1")
  expect_equal(req$workflow$label$variants[[2]]$variant, "label_soft_v1")
  expect_equal(req$workflow$label$variants[[3]]$variant, "label_broad_v1")
  expect_equal(req$workflow$explanation$variant, "explanation_pass_v1")
  expect_true(is.null(req$workflow$draft$request$format))
  expect_equal(req$workflow$label$variants[[1]]$request$format$title, "cluster_label_selection")
  expect_equal(req$workflow$explanation$request$format$title, "cluster_label_output")
  expect_match(
    req$workflow$draft$request$messages[[2]]$content,
    "Possible interpretations",
    fixed = TRUE
  )
  expect_match(
    req$workflow$explanation$request$messages[[2]]$content,
    "Selected label JSON:",
    fixed = TRUE
  )
})

test_that("three-step dry-run can build a dynamic draft-derived candidate list", {
  ev <- .build_test_cluster_evidence()

  req <- llm_label_cluster(
    evidence = ev,
    model = "gemma4:12b",
    variant = "label_primary_v1",
    workflow_steps = 3L,
    label_mode = "dynamic",
    dry_run = TRUE
  )

  first_label_prompt <- req$workflow$label$variants[[1]]$prompt
  first_label_request <- req$workflow$label$variants[[1]]$request

  expect_equal(first_label_prompt$label_mode_requested, "dynamic")
  expect_equal(first_label_prompt$label_mode_effective, "dynamic")
  expect_length(first_label_prompt$dynamic_candidates, 3L)
  expect_match(
    first_label_request$messages[[2]]$content,
    "Dynamic label mode is active.",
    fixed = TRUE
  )
  expect_match(
    first_label_request$messages[[2]]$content,
    "mixed meadow assemblage",
    fixed = TRUE
  )
  expect_match(
    first_label_request$messages[[2]]$content,
    "mixed_meadow_assemblage",
    fixed = TRUE
  )
})

test_that("catalog exposes the main public prompt trio", {
  catalog <- cocktailr:::.read_cluster_label_prompt_catalog()

  expect_equal(
    cocktailr:::.as_character_vector(catalog$parsed$public_label_variants),
    c("label_primary_v1", "label_soft_v1", "label_broad_v1")
  )
  expect_equal(catalog$parsed$public_default_label_variant, "label_primary_v1")
  expect_equal(
    cocktailr:::.as_character_vector(catalog$parsed$public_speculative_ladder),
    c("label_soft_v1", "label_broad_v1")
  )
  expect_equal(
    cocktailr:::.as_character_vector(catalog$parsed$internal_variants),
    c(
      "gate_abstain_examples_v1",
      "draft_analysis_v1",
      "label_selection_primary_v1",
      "label_selection_soft_v1",
      "label_selection_broad_v1",
      "explanation_pass_v1"
    )
  )
  expect_equal(
    catalog$parsed$legacy_variant_aliases$strict_abstention_gate_v1,
    "label_primary_v1"
  )
  expect_equal(
    catalog$parsed$legacy_variant_aliases$speculative_fallback_v8,
    "label_soft_v1"
  )
})

test_that("public cluster-label prompt folder contains only the supported prompt surface", {
  prompt_dir <- test_path("..", "..", "inst", "prompts", "cluster_labeling")
  entries <- basename(
    list.files(
      prompt_dir,
      recursive = FALSE,
      full.names = TRUE,
      include.dirs = TRUE
    )
  )

  expect_setequal(
    entries,
    c(
      "README.md",
      "catalog.json",
      "system_scientific_caution_v1.md",
      "user_label_primary_v1.md",
      "user_label_soft_v1.md",
      "user_label_broad_v1.md",
      "vocabulary"
    )
  )
  expect_false(any(grepl(
    "^user_(abstain_first|concise_label|conservative_interpretation|speculative_fallback|strict_abstention_gate)",
    entries,
    perl = TRUE
  )))
})

test_that("legacy prompt aliases resolve to the current public prompt files", {
  ev <- .build_test_cluster_evidence()

  primary_req <- llm_label_cluster(
    evidence = ev,
    model = "gemma4:12b",
    variant = "strict_abstention_gate_v1",
    dry_run = TRUE
  )
  soft_req <- llm_label_cluster(
    evidence = ev,
    model = "gemma4:12b",
    variant = "speculative_fallback_v8",
    dry_run = TRUE
  )

  expect_equal(primary_req$prompt$resolved_variant, "label_primary_v1")
  expect_true(primary_req$prompt$is_legacy_alias)
  expect_match(
    basename(primary_req$prompt$user_path),
    "user_label_primary_v1\\.md$",
    perl = TRUE
  )

  expect_equal(soft_req$prompt$resolved_variant, "label_soft_v1")
  expect_true(soft_req$prompt$is_legacy_alias)
  expect_match(
    basename(soft_req$prompt$user_path),
    "user_label_soft_v1\\.md$",
    perl = TRUE
  )
})

test_that("three-step workflow can escalate label selection and then explain a fixed label", {
  ev <- .build_test_cluster_evidence()
  fixture_path <- test_path(
    "fixtures", "llm", "cluster_label_output_example_labeled.json"
  )
  fixture_text <- paste(readLines(fixture_path, warn = FALSE), collapse = "\n")

  primary_selection <- list(
    schema_version = "0.1.0",
    cluster_id = "c_1",
    status = "abstain",
    canonical_label = NULL,
    display_label = NULL,
    label_summary = "The strict selection pass abstains because the cluster is still too mixed.",
    abstain_reason = "Strict selection abstained."
  )
  soft_selection <- list(
    schema_version = "0.1.0",
    cluster_id = "c_1",
    status = "labeled",
    canonical_label = "mixed_meadow_assemblage",
    display_label = "mixed meadow assemblage",
    label_summary = "A broad meadow-like fallback label is the safest useful choice here.",
    abstain_reason = NULL
  )

  state <- new.env(parent = emptyenv())
  state$n <- 0L
  state$formats <- character(0)

  fake_request <- function(url, payload, timeout_sec) {
    state$n <- state$n + 1L
    state$formats[state$n] <- payload$format$title %||% "freeform"

    content <- if (is.null(payload$format)) {
      paste(
        "Possible interpretations:",
        "- dry meadow",
        "- mixed meadow",
        "",
        "Main signal:",
        "- mixed meadow direction",
        sep = "\n"
      )
    } else if (identical(payload$format$title, "cluster_label_selection")) {
      if (grepl("Task mode: `label_selection_primary_v1`", payload$messages[[2]]$content, fixed = TRUE)) {
        jsonlite::toJSON(primary_selection, auto_unbox = TRUE, null = "null")
      } else {
        jsonlite::toJSON(soft_selection, auto_unbox = TRUE, null = "null")
      }
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
    variant = "label_primary_v1",
    workflow_steps = 3L,
    request_fn = fake_request
  )

  expect_equal(state$n, 4L)
  expect_equal(state$formats[[1]], "freeform")
  expect_equal(state$formats[[2]], "cluster_label_selection")
  expect_equal(state$formats[[3]], "cluster_label_selection")
  expect_equal(state$formats[[4]], "cluster_label_output")
  expect_equal(res$workflow$draft$output$status, "draft_ready")
  expect_equal(res$workflow$label$selected_public_variant, "label_soft_v1")
  expect_equal(res$output$status, "labeled")
  expect_equal(res$output$canonical_label, "mixed_meadow_assemblage")
  expect_equal(res$output$display_label, "mixed meadow assemblage")
  expect_equal(res$workflow$explanation$output$display_label, "mixed meadow assemblage")
})

test_that("three-step workflow skips the explanation LLM call after an exhausted selection cascade", {
  ev <- .build_test_cluster_evidence()

  abstain_selection <- list(
    schema_version = "0.1.0",
    cluster_id = "c_1",
    status = "abstain",
    canonical_label = NULL,
    display_label = NULL,
    label_summary = "This rung abstains because the cluster remains too mixed.",
    abstain_reason = "Still too mixed."
  )

  state <- new.env(parent = emptyenv())
  state$n <- 0L
  state$formats <- character(0)

  fake_request <- function(url, payload, timeout_sec) {
    state$n <- state$n + 1L
    state$formats[state$n] <- payload$format$title %||% "freeform"

    content <- if (is.null(payload$format)) {
      paste(
        "Possible interpretations:",
        "- mixed cluster",
        "",
        "Main signal:",
        "- recurring but unresolved signal",
        sep = "\n"
      )
    } else if (identical(payload$format$title, "cluster_label_selection")) {
      jsonlite::toJSON(abstain_selection, auto_unbox = TRUE, null = "null")
    } else {
      stop("The explanation LLM call should be skipped when selection is exhausted.")
    }

    outer <- list(
      model = payload$model,
      created_at = "2026-06-30T12:00:00Z",
      message = list(role = "assistant", content = content),
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
    variant = "label_primary_v1",
    workflow_steps = 3L,
    request_fn = fake_request
  )

  expect_equal(state$n, 4L)
  expect_equal(state$formats, c("freeform", "cluster_label_selection", "cluster_label_selection", "cluster_label_selection"))
  expect_true(isTRUE(res$workflow$label$exhausted))
  expect_equal(res$workflow$label$selected_public_variant, "chaotic_cluster_fallback")
  expect_true(isTRUE(res$workflow$explanation$skipped))
  expect_equal(res$workflow$explanation$skip_reason, "label_selection_exhausted")
  expect_equal(res$output$status, "labeled")
  expect_equal(res$output$canonical_label, "chaotic_cluster")
  expect_equal(res$output$display_label, "chaotic cluster")

  val <- validate_cluster_label(res, ev)
  expect_equal(val$validation_status, "valid_with_warnings")
  expect_true(val$needs_human_review)
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
    variant = "label_soft_v1",
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
    variant = "label_soft_v1",
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
    variant = "label_primary_v1",
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
    variant = "label_primary_v1",
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
    variant = "label_soft_v1",
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

