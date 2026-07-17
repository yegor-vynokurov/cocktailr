.build_phase3_test_cluster_object <- function() {
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

.build_phase3_test_cluster_evidence <- function(...) {
  cluster_evidence(
    .build_phase3_test_cluster_object(),
    cluster = "c_1",
    top_n_phi = 3,
    n_prototype_plots = 2,
    n_borderline_plots = 2,
    ...
  )
}

.phase3_llm_outer <- function(payload, content) {
  jsonlite::toJSON(
    list(
      model = payload$model,
      created_at = "2026-06-30T13:00:00Z",
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

.phase3_selection_text <- function(
    display_label = NULL,
    abstain = FALSE,
    abstain_text = "ABSTAIN"
) {
  if (isTRUE(abstain)) {
    return(abstain_text)
  }

  display_label %||% "compact species core"
}

.phase3_request_stage <- function(payload) {
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

test_that("selection rung preserves overlong open labels when optional shortening is enabled", {
  ev <- .build_phase3_test_cluster_evidence()
  state <- new.env(parent = emptyenv())
  state$selection_calls <- 0L

  fake_request <- function(url, payload, timeout_sec) {
    content <- if (identical(.phase3_request_stage(payload), "selection")) {
      state$selection_calls <- state$selection_calls + 1L
      if (identical(state$selection_calls, 1L)) {
        "Dry base-rich grassland with sedge and thyme core"
      } else {
        .phase3_selection_text(
          display_label = "dry base-rich grassland"
        )
      }
    } else if (identical(.phase3_request_stage(payload), "summary")) {
      "The same dry base-rich grassland signal recurs across the evidence bundle."
    } else {
      stop("Unexpected stage in test request.")
    }

    list(
      status_code = 200L,
      body_text = .phase3_llm_outer(payload, content),
      parsed = NULL
    )
  }

  res <- llm_label_cluster(
    evidence = ev,
    model = "fake-model",
    variant = "label_primary_v1",
    internal_prompt_version = "v2",
    use_brainstorm = FALSE,
    short_label_with_llm = TRUE,
    max_retries = 5L,
    timeout_sec = 1,
    request_fn = fake_request
  )

  expect_equal(state$selection_calls, 1L)
  expect_equal(res$output$status, "labeled")
  expect_equal(
    res$output$display_label,
    "Dry base-rich grassland with sedge and thyme core"
  )
  expect_match(
    res$output$canonical_label,
    "^dry_base_rich_grassland",
    perl = TRUE
  )
  expect_equal(res$workflow$label$selected_public_variant, "label_primary_v1")
  expect_equal(length(res$workflow$label$attempts), 1L)
  expect_null(res$workflow$label$repair_source)
  expect_null(res$workflow$label$repair_variant)
  expect_true(.is_non_empty_scalar_character(res$output$label_summary))
  expect_equal(res$workflow$label$attempts[[1]]$attempts, 1L)
})

test_that("label-decision parser derives canonical_label programmatically from short raw label text", {
  parsed <- cocktailr:::.parse_cluster_label_label_decision_text(
    content = "Dry Meadow Edge",
    cluster_id = "c_1"
  )

  expect_equal(parsed$cluster_id, "c_1")
  expect_equal(parsed$status, "labeled")
  expect_equal(parsed$label_decision_text, "Dry Meadow Edge")
  expect_equal(parsed$display_label, "Dry Meadow Edge")
  expect_equal(parsed$canonical_label, "dry_meadow_edge")
})

test_that("label-decision parser treats the plain ABSTAIN token as abstain without label fields", {
  parsed <- cocktailr:::.parse_cluster_label_label_decision_text(
    content = "ABSTAIN",
    cluster_id = "c_1"
  )

  expect_equal(parsed$cluster_id, "c_1")
  expect_equal(parsed$status, "abstain")
  expect_equal(parsed$label_decision_text, "ABSTAIN")
  expect_null(parsed$display_label)
  expect_null(parsed$canonical_label)
})

test_that("summary rung retries empty text before accepting the rung", {
  ev <- .build_phase3_test_cluster_evidence()
  state <- new.env(parent = emptyenv())
  state$summary_calls <- 0L

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .phase3_request_stage(payload)
    content <- if (identical(stage, "selection")) {
      .phase3_selection_text(display_label = "compact species core")
    } else if (identical(stage, "summary")) {
      state$summary_calls <- state$summary_calls + 1L
      if (identical(state$summary_calls, 1L)) {
        "```text\n\n```"
      } else {
        "The same compact species core recurs across the evidence bundle."
      }
    } else {
      stop("Unexpected stage in test request.")
    }

    list(
      status_code = 200L,
      body_text = .phase3_llm_outer(payload, content),
      parsed = NULL
    )
  }

  res <- llm_label_cluster(
    evidence = ev,
    model = "fake-model",
    variant = "label_primary_v1",
    use_brainstorm = FALSE,
    short_label_with_llm = TRUE,
    max_retries = 1L,
    timeout_sec = 1,
    request_fn = fake_request
  )

  expect_equal(state$summary_calls, 2L)
  expect_equal(res$output$status, "labeled")
  expect_equal(res$workflow$summary$attempts, 2L)
})

test_that("summary rung rejects abstain tokens and falls back programmatically", {
  ev <- .build_phase3_test_cluster_evidence()

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .phase3_request_stage(payload)
    content <- if (identical(stage, "selection")) {
      .phase3_selection_text(display_label = "compact species core")
    } else if (identical(stage, "summary")) {
      "ABSTAIN"
    } else {
      stop("Unexpected stage in test request.")
    }

    list(
      status_code = 200L,
      body_text = .phase3_llm_outer(payload, content),
      parsed = NULL
    )
  }

  res <- llm_label_cluster(
    evidence = ev,
    model = "fake-model",
    variant = "label_primary_v1",
    use_brainstorm = FALSE,
    max_retries = 0L,
    timeout_sec = 1,
    request_fn = fake_request
  )

  expect_equal(res$output$status, "labeled")
  expect_true(.is_non_empty_scalar_character(res$output$label_summary))
  expect_false(identical(res$output$label_summary, "ABSTAIN"))
  expect_true(isTRUE(res$workflow$summary$fallback_used))
  expect_match(
    res$output$label_summary,
    "fallback summary was assembled programmatically",
    fixed = TRUE
  )
})

test_that("summary parser rejects the plain ABSTAIN token for labeled outputs", {
  expect_error(
    cocktailr:::.parse_cluster_label_summary_text(
      content = "ABSTAIN",
      cluster_id = "c_1"
    ),
    "label-summary output must not be the plain abstain token",
    fixed = TRUE
  )
})

test_that("all-abstain path stays abstain and assembles fallback abstain reason programmatically", {
  ev <- .build_phase3_test_cluster_evidence()
  state <- new.env(parent = emptyenv())
  state$abstain_reason_calls <- 0L

  fake_request <- function(url, payload, timeout_sec) {
    task_type <- .phase3_request_stage(payload)
    content <- if (identical(task_type, "draft")) {
      paste(
        "Possible interpretations:",
        "- mixed meadow assemblage",
        "",
        "Main signal:",
        "- mixed meadow direction",
        "",
        "Noise or conflicts:",
        "- strong mixing across prototype plots",
        "- no clean narrow habitat split",
        "",
        "Candidate labels:",
        "- mixed meadow assemblage",
        "",
        "What not to overclaim:",
        "- narrow habitat naming",
        sep = "\n"
      )
    } else if (identical(task_type, "selection")) {
      .phase3_selection_text(abstain = TRUE, abstain_text = "ABSTAIN")
    } else {
      state$abstain_reason_calls <- state$abstain_reason_calls + 1L
      "```text\n\n```"
    }

    list(
      status_code = 200L,
      body_text = .phase3_llm_outer(payload, content),
      parsed = NULL
    )
  }

  res <- llm_label_cluster(
    evidence = ev,
    model = "fake-model",
    variant = "label_primary_v1",
    use_brainstorm = TRUE,
    max_retries = 1L,
    timeout_sec = 1,
    request_fn = fake_request
  )

  expect_equal(state$abstain_reason_calls, 2L)
  expect_equal(res$output$status, "abstain")
  expect_null(res$output$canonical_label)
  expect_null(res$output$display_label)
  expect_null(res$output$label_summary)
  expect_true(.is_non_empty_scalar_character(res$output$abstain_reason))
  expect_true(.is_non_empty_scalar_character(res$output$explanation))
  expect_match(res$output$abstain_reason, "fallback reason was assembled programmatically", fixed = TRUE)
  expect_equal(res$workflow$label$selected_public_variant, "selection_all_abstain")
  expect_equal(res$workflow$abstain_reason$attempts, 2L)
  expect_true(isTRUE(res$workflow$abstain_reason$fallback_used))
})

test_that("overlong labels that still fail shortening remain transparent after ladder exhaustion", {
  ev <- .build_phase3_test_cluster_evidence()
  state <- new.env(parent = emptyenv())
  state$selection_calls <- 0L

  fake_request <- function(url, payload, timeout_sec) {
    task_type <- .phase3_request_stage(payload)
    content <- if (identical(task_type, "selection")) {
      state$selection_calls <- state$selection_calls + 1L
      "Semi-open cool woodland on base-rich soils with Vincetoxicum Galium understorey"
    } else {
      ""
    }

    list(
      status_code = 200L,
      body_text = .phase3_llm_outer(payload, content),
      parsed = NULL
    )
  }

  res <- llm_label_cluster(
    evidence = ev,
    model = "fake-model",
    variant = "label_primary_v1",
    use_brainstorm = FALSE,
    max_retries = 1L,
    timeout_sec = 1,
    request_fn = fake_request
  )

  expect_equal(state$selection_calls, 6L)
  expect_equal(res$output$status, "abstain")
  expect_equal(res$workflow$label$selected_public_variant, "selection_all_abstain")
  expect_null(res$workflow$label$repair_source %||% NULL)
  expect_equal(
    res$workflow$label$attempts[[1]]$repair_source,
    "shortening_branch"
  )
  expect_equal(res$workflow$label$attempts[[1]]$result, "failed_after_retry")
  expect_equal(res$workflow$label$attempts[[2]]$result, "failed_after_retry")
  expect_equal(res$workflow$label$attempts[[3]]$result, "failed_after_retry")
  expect_length(res$workflow$label$attempts[[1]]$repair_history, 2L)
  expect_equal(
    res$workflow$label$attempts[[1]]$repair_history[[1]]$response_content,
    "Semi-open cool woodland on base-rich soils with Vincetoxicum Galium understorey"
  )
  expect_equal(
    res$workflow$label$attempts[[1]]$repair_history[[2]]$response_content,
    "Semi-open cool woodland on base-rich soils with Vincetoxicum Galium understorey"
  )
  expect_match(
    res$workflow$label$failure_messages[[1]],
    "invalid label-decision output after 2 attempt(s)",
    fixed = TRUE
  )
  expect_match(
    res$workflow$label$failure_messages[[1]],
    "Label-decision output failed text validation",
    fixed = TRUE
  )
})

test_that("validator accepts final assembled outputs when evidence contains user_added_data", {
  tmp_dir <- file.path(tempdir(), "cluster_user_added_phase3")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c("supplemental note", "keep broad interpretation"),
    file.path(tmp_dir, "notes.txt"),
    useBytes = TRUE
  )

  ev <- .build_phase3_test_cluster_evidence(user_added_data = tmp_dir)

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .phase3_request_stage(payload)
    content <- if (identical(stage, "selection")) {
      .phase3_selection_text(
        display_label = "compact species core"
      )
    } else if (identical(stage, "summary")) {
      "The same compact species core recurs across the evidence bundle."
    } else {
      stop("Unexpected stage in test request.")
    }

    list(
      status_code = 200L,
      body_text = .phase3_llm_outer(payload, content),
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
  val <- validate_cluster_label(res, ev)

  expect_true(isTRUE(ev$meta$user_added_data_present))
  expect_true(isTRUE(val$is_valid))
  expect_true(val$validation_status %in% c("valid", "valid_with_warnings"))
})
