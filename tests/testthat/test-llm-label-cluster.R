.build_test_cluster_evidence <- function() {
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

.llm_test_outer <- function(payload, content) {
  jsonlite::toJSON(
    list(
      model = payload$model,
      created_at = "2026-07-01T12:00:00Z",
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

.llm_test_selection_text <- function(
    display_label = "compact species core",
    abstain = FALSE,
    abstain_text = "ABSTAIN"
) {
  if (isTRUE(abstain)) {
    return(abstain_text)
  }

  display_label
}

.llm_test_request_stage <- function(payload) {
  user_text <- payload$messages[[2]]$content

  if (grepl("Task mode: `label_decision_", user_text, fixed = TRUE)) {
    return("selection")
  }
  if (grepl("Task mode: `draft_analysis_v1`", user_text, fixed = TRUE)) {
    return("draft")
  }
  if (grepl("Task mode: `label_summary_pass_v2`", user_text, fixed = TRUE)) {
    return("summary")
  }
  if (grepl("Task mode: `abstain_reason_pass_v2`", user_text, fixed = TRUE)) {
    return("abstain_reason")
  }

  "unknown"
}

test_that("llm_label_cluster dry-run assembles the fixed three-stage workflow", {
  ev <- .build_test_cluster_evidence()

  req <- llm_label_cluster(
    evidence = ev,
    model = "gemma4:12b",
    variant = "label_primary_v1",
    dry_run = TRUE
  )

  expect_s3_class(req, "cluster_label_request")
  expect_equal(req$workflow_steps, 3L)
  expect_equal(req$model, "gemma4:12b")
  expect_equal(req$workflow$draft$variant, "draft_analysis_v1")
  expect_null(req$workflow$label$variants[[1]]$request$format)
  expect_equal(req$workflow$summary$variant, "label_summary_pass_v2")
  expect_equal(req$workflow$abstain_reason$variant, "abstain_reason_pass_v2")
  expect_null(req$workflow$draft$request$format)
  expect_equal(
    vapply(req$workflow$label$variants, function(x) x$variant, character(1)),
    c("label_primary_v1", "label_soft_v1", "label_broad_v1")
  )
  expect_match(
    req$workflow$label$variants[[1]]$prompt$user,
    "Code-extracted candidate labels:",
    fixed = TRUE
  )
  expect_match(
    req$request$messages[[2]]$content,
    "Chosen short label (fixed; do not replace it):",
    fixed = TRUE
  )
  expect_match(
    req$request$messages[[2]]$content,
    "placeholder label",
    fixed = TRUE
  )
  expect_false(grepl("Cluster evidence:", req$request$messages[[2]]$content, fixed = TRUE))
  expect_match(
    req$prompt$evidence_text,
    "Plants that regularly occur in this cluster:",
    fixed = TRUE
  )
  expect_match(
    req$prompt$evidence_text,
    "Species with the strongest cluster association:",
    fixed = TRUE
  )
  expect_match(
    req$prompt$evidence_text,
    "Dataset context:",
    fixed = TRUE
  )
  expect_match(
    req$prompt$evidence_text,
    "How to read these cluster metrics:",
    fixed = TRUE
  )
  expect_match(
    req$prompt$evidence_text,
    "merge-phi value for this cluster",
    fixed = TRUE
  )
  expect_match(
    req$prompt$evidence_text,
    "member plots out of",
    fixed = TRUE
  )
  expect_match(
    req$prompt$evidence_text,
    "a plot must contain at least",
    fixed = TRUE
  )
  expect_match(
    req$prompt$evidence_text,
    "/100 on the original percentage-cover scale",
    fixed = TRUE
  )
  expect_match(
    req$prompt$evidence_text,
    "occurs in ",
    fixed = TRUE
  )
  expect_false(grepl("Cover summary:", req$prompt$evidence_text, fixed = TRUE))
  expect_false(grepl("mean_cover=", req$prompt$evidence_text, fixed = TRUE))
  expect_false(grepl("\\[E[0-9]+\\]", req$prompt$evidence_text))
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

  expect_true(budget$trimmed)
  expect_lte(budget$total_prompt_chars, small_budget)
  expect_match(req$prompt$evidence_text, "Dataset context:", fixed = TRUE)
  expect_false(grepl("Cover summary:", req$prompt$evidence_text, fixed = TRUE))
  expect_false("cover_summary" %in% budget$blocks$id)
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

test_that("constrained label mode injects the coarse vocabulary into the selection prompt", {
  ev <- .build_test_cluster_evidence()

  req <- llm_label_cluster(
    evidence = ev,
    model = "gemma4:12b",
    variant = "label_primary_v1",
    label_mode = "constrained",
    dry_run = TRUE
  )

  first_label_prompt <- req$workflow$label$variants[[1]]$prompt

  expect_equal(first_label_prompt$label_mode_requested, "constrained")
  expect_equal(first_label_prompt$label_mode_effective, "constrained")
  expect_match(first_label_prompt$user, "Constrained label mode is active.", fixed = TRUE)
  expect_match(first_label_prompt$user, "Allowed labels:", fixed = TRUE)
  expect_match(first_label_prompt$user, "woodland_like_assemblage", fixed = TRUE)
})

test_that("deprecated dynamic label mode falls back to open without dropping code-extracted candidates", {
  ev <- .build_test_cluster_evidence()

  expect_warning(
    req <- llm_label_cluster(
      evidence = ev,
      model = "gemma4:12b",
      variant = "label_primary_v1",
      label_mode = "dynamic",
      dry_run = TRUE
    ),
    "deprecated"
  )

  first_label_prompt <- req$workflow$label$variants[[1]]$prompt

  expect_equal(first_label_prompt$label_mode_requested, "open")
  expect_equal(first_label_prompt$label_mode_effective, "open")
  expect_match(first_label_prompt$user, "Code-extracted candidate labels:", fixed = TRUE)
  expect_match(first_label_prompt$user, "mixed meadow assemblage", fixed = TRUE)
})

test_that("llm_label_cluster assembles labeled output from label-only decision and summary-only text", {
  ev <- .build_test_cluster_evidence()

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .llm_test_request_stage(payload)

    content <- if (identical(stage, "selection")) {
      .llm_test_selection_text(
        display_label = "compact species core"
      )
    } else if (identical(stage, "summary")) {
      "The same compact species core recurs across the evidence bundle."
    } else {
      stop("Unexpected stage in test request.")
    }

    list(
      status_code = 200L,
      body_text = .llm_test_outer(payload, content),
      parsed = NULL
    )
  }

  res <- llm_label_cluster(
    evidence = ev,
    model = "fake-model",
    variant = "label_primary_v1",
    use_brainstorm = FALSE,
    timeout_sec = 1,
    request_fn = fake_request
  )

  expect_s3_class(res, "cluster_label_result")
  expect_equal(res$output$status, "labeled")
  expect_equal(res$output$canonical_label, "compact_species_core")
  expect_equal(res$output$display_label, "compact species core")
  expect_true(.is_non_empty_scalar_character(res$output$label_summary))
  expect_identical(res$output$explanation, res$output$label_summary)
  expect_true(isTRUE(res$workflow$summary$attempts >= 1L))
  expect_true(isTRUE(res$workflow$abstain_reason$skipped))
})

test_that("llm_label_cluster preserves label-only abstain decisions and falls back on abstain-reason text", {
  ev <- .build_test_cluster_evidence()
  state <- new.env(parent = emptyenv())
  state$selection_calls <- 0L
  state$abstain_reason_calls <- 0L

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .llm_test_request_stage(payload)

    content <- switch(
      stage,
      selection = {
        state$selection_calls <- state$selection_calls + 1L
        .llm_test_selection_text(
          abstain = TRUE,
          abstain_text = "ABSTAIN"
        )
      },
      abstain_reason = {
        state$abstain_reason_calls <- state$abstain_reason_calls + 1L
        ""
      },
      stop("Unexpected stage in test request.")
    )

    list(
      status_code = 200L,
      body_text = .llm_test_outer(payload, content),
      parsed = NULL
    )
  }

  res <- llm_label_cluster(
    evidence = ev,
    model = "fake-model",
    variant = "label_primary_v1",
    use_brainstorm = FALSE,
    timeout_sec = 1,
    request_fn = fake_request
  )

  expect_s3_class(res, "cluster_label_result")
  expect_equal(state$selection_calls, 3L)
  expect_equal(state$abstain_reason_calls, 1L)
  expect_equal(res$output$status, "abstain")
  expect_null(res$output$canonical_label)
  expect_null(res$output$display_label)
  expect_null(res$output$label_summary)
  expect_match(res$output$abstain_reason, "too mixed or weak", fixed = TRUE)
  expect_true(.is_non_empty_scalar_character(res$output$explanation))
  expect_equal(res$workflow$label$selected_public_variant, "selection_all_abstain")
  expect_true(isTRUE(res$workflow$label$exhausted))
  expect_true(isTRUE(res$workflow$abstain_reason$fallback_used))
  expect_true(isTRUE(res$workflow$explanation$fallback_used))
})

test_that("llm_label_cluster repairs an invalid label-decision reply with one local retry", {
  ev <- .build_test_cluster_evidence()
  state <- new.env(parent = emptyenv())
  state$selection_calls <- 0L

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .llm_test_request_stage(payload)

    content <- switch(
      stage,
      selection = {
        state$selection_calls <- state$selection_calls + 1L
        if (state$selection_calls == 1L) {
          "This answer is too long and does not follow the short label decision contract at all."
        } else {
          .llm_test_selection_text(
            display_label = "compact species core"
          )
        }
      },
      summary = "The same compact species core recurs across the evidence bundle.",
      stop("Unexpected stage in test request.")
    )

    list(
      status_code = 200L,
      body_text = .llm_test_outer(payload, content),
      parsed = NULL
    )
  }

  res <- llm_label_cluster(
    evidence = ev,
    model = "fake-model",
    variant = "label_primary_v1",
    use_brainstorm = FALSE,
    timeout_sec = 1,
    request_fn = fake_request
  )

  expect_equal(state$selection_calls, 2L)
  expect_equal(res$output$status, "labeled")
  expect_equal(res$output$canonical_label, "compact_species_core")
  expect_equal(length(res$workflow$label$attempts), 1L)
  expect_equal(res$workflow$label$attempts[[1]]$attempts, 2L)
  expect_equal(res$workflow$label$attempts[[1]]$result, "labeled")
})

test_that("llm_label_cluster enforces constrained label mode after text parsing", {
  ev <- .build_test_cluster_evidence()
  state <- new.env(parent = emptyenv())
  state$selection_calls <- 0L

  vocab_path <- file.path(tempdir(), "cocktailr_constrained_text_only_vocab.json")
  jsonlite::write_json(
    list(
      vocabulary_version = "test-1",
      vocabulary_name = "constrained_text_only_test_vocab",
      labels = list(
        list(
          canonical_label = "allowed_test_label",
          display_label = "Allowed Test Label",
          short_description = "Broad test label for constrained selection coverage.",
          use_when = "Use when the test needs a single allowed label."
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

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .llm_test_request_stage(payload)

    content <- switch(
      stage,
      selection = {
        state$selection_calls <- state$selection_calls + 1L
        if (state$selection_calls <= 2L) {
          .llm_test_selection_text(
            display_label = "Not In Vocab"
          )
        } else {
          .llm_test_selection_text(
            display_label = "allowed test label"
          )
        }
      },
      summary = "This matches the configured constrained vocabulary.",
      stop("Unexpected stage in test request.")
    )

    list(
      status_code = 200L,
      body_text = .llm_test_outer(payload, content),
      parsed = NULL
    )
  }

  res <- llm_label_cluster(
    evidence = ev,
    model = "fake-model",
    variant = "label_primary_v1",
    label_mode = "constrained",
    use_brainstorm = FALSE,
    timeout_sec = 1,
    request_fn = fake_request
  )

  expect_equal(state$selection_calls, 3L)
  expect_equal(res$output$status, "labeled")
  expect_equal(res$output$canonical_label, "allowed_test_label")
  expect_equal(res$output$display_label, "Allowed Test Label")
  expect_equal(length(res$workflow$label$attempts), 2L)
  expect_equal(res$workflow$label$attempts[[1]]$result, "failed_after_retry")
  expect_true(isTRUE(res$workflow$label$attempts[[1]]$retry_exhausted))
  expect_match(
    res$workflow$label$attempts[[1]]$error,
    "must choose canonical_label from the configured vocabulary",
    fixed = TRUE
  )
  expect_equal(res$workflow$label$attempts[[2]]$result, "labeled")
  expect_equal(res$workflow$label$selected_public_variant, "label_soft_v1")
})

test_that("llm_label_cluster writes text-stage artifacts and parse diagnostics for the fixed pipeline", {
  ev <- .build_test_cluster_evidence()
  log_dir <- file.path(tempdir(), "cocktailr_text_only_stage_logs")
  unlink(log_dir, recursive = TRUE, force = TRUE)

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .llm_test_request_stage(payload)

    content <- switch(
      stage,
      draft = paste(
        "Possible interpretations:",
        "- mixed meadow assemblage",
        "",
        "Main signal:",
        "- mixed meadow direction",
        "",
        "Noise or conflicts:",
        "- no clean narrow habitat split",
        "",
        "Candidate labels:",
        "- mixed meadow assemblage",
        "- compact species core",
        "",
        "What not to overclaim:",
        "- narrow habitat naming",
        sep = "\n"
      ),
      selection = paste(
        "```text",
        .llm_test_selection_text(
          display_label = "compact species core"
        ),
        "```",
        sep = "\n"
      ),
      summary = "The same compact species core recurs across the evidence bundle.",
      stop("Unexpected stage in test request.")
    )

    list(
      status_code = 200L,
      body_text = .llm_test_outer(payload, content),
      parsed = NULL
    )
  }

  res <- llm_label_cluster(
    evidence = ev,
    model = "fake-model",
    variant = "label_primary_v1",
    use_brainstorm = TRUE,
    debug = TRUE,
    timeout_sec = 1,
    log_dir = log_dir,
    request_fn = fake_request
  )

  draft_artifact <- res$workflow$draft$logs$parsed_text_fields
  selection_artifact <- res$workflow$label$attempts[[1]]$logs$parsed_text_fields
  summary_artifact <- res$workflow$summary$logs$parsed_text_fields
  selection_diag <- paste0(
    res$workflow$label$attempts[[1]]$logs$attempt_diagnostics_prefix,
    "_attempt1.json"
  )

  expect_true(file.exists(draft_artifact))
  expect_true(file.exists(selection_artifact))
  expect_true(file.exists(summary_artifact))
  expect_true(file.exists(selection_diag))

  draft_json <- jsonlite::fromJSON(draft_artifact, simplifyVector = FALSE)
  selection_json <- jsonlite::fromJSON(selection_artifact, simplifyVector = FALSE)
  summary_json <- jsonlite::fromJSON(summary_artifact, simplifyVector = FALSE)
  selection_diag_json <- jsonlite::fromJSON(selection_diag, simplifyVector = FALSE)

  expect_equal(draft_json$stage_name, "draft_analysis")
  expect_equal(draft_json$parser_type, "draft_text_v1")
  expect_gte(as.integer(draft_json$candidate_count), 1L)

  expect_equal(selection_json$stage_name, "label_decision")
  expect_equal(selection_json$parser_type, "label_decision_text_v2")
  expect_true(isTRUE(selection_json$code_fence_salvaged))
  expect_equal(selection_json$status, "labeled")
  expect_true(isTRUE(selection_json$extracted_label_decision_text))
  expect_true(isTRUE(selection_json$extracted_canonical_label))
  expect_true(isTRUE(selection_json$extracted_display_label))
  expect_false(isTRUE(selection_json$extracted_inline_abstain_reason))

  expect_equal(summary_json$stage_name, "label_summary")
  expect_equal(summary_json$parser_type, "label_summary_text_v2")
  expect_match(summary_json$label_summary, "compact species core", fixed = TRUE)

  expect_true(isTRUE(selection_diag_json$parse_code_fence_salvaged))
  expect_equal(
    selection_diag_json$parse_parsing_rule,
    "single_short_answer"
  )
})

test_that("llm_label_cluster does not write debug logs unless debug=TRUE", {
  ev <- .build_test_cluster_evidence()
  log_dir <- file.path(tempdir(), "cocktailr_debug_logs_disabled")
  unlink(log_dir, recursive = TRUE, force = TRUE)

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .llm_test_request_stage(payload)

    content <- switch(
      stage,
      selection = .llm_test_selection_text(
        display_label = "compact species core"
      ),
      summary = "The same compact species core recurs across the evidence bundle.",
      stop("Unexpected stage in test request.")
    )

    list(
      status_code = 200L,
      body_text = .llm_test_outer(payload, content),
      parsed = NULL
    )
  }

  res <- llm_label_cluster(
    evidence = ev,
    model = "fake-model",
    variant = "label_primary_v1",
    use_brainstorm = FALSE,
    debug = FALSE,
    timeout_sec = 1,
    log_dir = log_dir,
    request_fn = fake_request
  )

  expect_null(res$logs$run_dir)
  expect_false(dir.exists(log_dir))
  expect_null(res$workflow$summary$logs$run_dir)
  expect_null(res$workflow$abstain_reason$logs$run_dir)
})
