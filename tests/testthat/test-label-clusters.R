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

.build_label_clusters_test_evidence <- function(...) {
  cluster_evidence(
    .build_label_clusters_test_cocktail(),
    cluster = "c_1",
    top_n_phi = 3,
    n_prototype_plots = 2,
    n_borderline_plots = 2,
    ...
  )
}

.label_clusters_test_selection_output <- function(
    display_label = "compact species core",
    abstain = FALSE,
    abstain_text = "ABSTAIN"
) {
  if (isTRUE(abstain)) {
    return(abstain_text)
  }

  display_label
}

.label_clusters_test_draft_text <- function() {
  paste(
    "Possible interpretations:",
    "- mixed meadow assemblage",
    "- dry meadow assemblage",
    "",
    "Main signal:",
    "- mixed meadow direction",
    "",
    "Noise or conflicts:",
    "- no clean narrow habitat split",
    "",
    "Candidate labels:",
    "- mixed meadow assemblage",
    "- dry meadow assemblage",
    "",
    "What not to overclaim:",
    "- precise habitat naming",
    sep = "\n"
  )
}

.label_clusters_test_outer <- function(payload, content) {
  jsonlite::toJSON(
    list(
      model = payload$model,
      created_at = "2026-07-01T12:30:00Z",
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

.label_clusters_request_stage <- function(payload) {
  user_text <- payload$messages[[2]]$content

  if (grepl("Task mode: `label_decision_", user_text, fixed = TRUE)) {
    return("selection")
  }
  if (grepl("Task mode: `category_decision_", user_text, fixed = TRUE)) {
    return("category")
  }
  if (grepl("Task mode: `subcategory_decision_", user_text, fixed = TRUE)) {
    return("subcategory")
  }
  if (grepl("Task mode: `general_name_decision_", user_text, fixed = TRUE)) {
    return("general_name")
  }
  if (grepl("Task mode: `uniqueness_detail_decision_", user_text, fixed = TRUE)) {
    return("uniqueness_detail")
  }
  if (grepl("Task mode: `post_label_category_v1`", user_text, fixed = TRUE)) {
    return("post_label_category")
  }
  if (grepl("Task mode: `post_label_uniqueness_v1`", user_text, fixed = TRUE)) {
    return("post_label_uniqueness")
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

test_that("label_clusters runs the fixed pipeline, writes a review card, and saves a registry", {
  x <- .build_label_clusters_test_cocktail()
  ev <- .build_label_clusters_test_evidence()

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .label_clusters_request_stage(payload)
    content <- switch(
    stage,
    draft = .label_clusters_test_draft_text(),
    selection = .label_clusters_test_selection_output(),
    summary = "The same compact species core recurs across the evidence bundle.",
      stop("Unexpected stage in test request.")
    )

    list(
      status_code = 200L,
      body_text = .label_clusters_test_outer(payload, content),
      parsed = NULL
    )
  }

  review_dir <- file.path(tempdir(), "cocktailr-label-clusters-basic")
  unlink(review_dir, recursive = TRUE, force = TRUE)

  res <- label_clusters(
    x = x,
    clusters = "c_1",
    model = "fake-model",
    variant = "label_primary_v1",
    timeout_sec = 1,
    review_dir = review_dir,
    verbose = FALSE,
    labels_for_imgs = TRUE,
    request_fn = fake_request
  )

  expect_s3_class(res, "cluster_label_batch_result")
  expect_equal(nrow(res$summary), 1L)
  expect_equal(res$summary$cluster[[1]], "c_1")
  expect_equal(res$summary$run_status[[1]], "success")
  expect_true(res$summary$validation_status[[1]] %in% c("valid", "valid_with_warnings"))
  expect_false(res$summary$used_placeholder[[1]])
  expect_true(file.exists(res$summary$review_file[[1]]))
  expect_s3_class(res$label_registry, "cluster_label_registry")
  expect_true(file.exists(res$label_registry_file))
  expect_match(
    paste(readLines(res$summary$review_file[[1]], warn = FALSE), collapse = "\n"),
    "- Label summary: ",
    fixed = TRUE
  )
  expect_false(grepl(
    "## Final explanation",
    paste(readLines(res$summary$review_file[[1]], warn = FALSE), collapse = "\n"),
    fixed = TRUE
  ))
})

test_that("label_clusters preserves experimental category fields from v3 prompts", {
  x <- .build_label_clusters_test_cocktail()

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .label_clusters_request_stage(payload)
    content <- switch(
      stage,
      draft = .label_clusters_test_draft_text(),
      selection = paste(
        "LABEL: dry base-rich grassland",
        "CATEGORY_LABEL: dry grassland",
        "SUBCATEGORY_LABELS: base-rich; open",
        sep = "\n"
      ),
      summary = {
        expect_match(
          payload$messages[[2]]$content,
          "Chosen category label:",
          fixed = TRUE
        )
        expect_match(payload$messages[[2]]$content, "dry grassland", fixed = TRUE)
        expect_match(payload$messages[[2]]$content, "base-rich; open", fixed = TRUE)
        "The dry grassland category is refined by base-rich and open modifiers."
      },
      stop("Unexpected stage in v3 category-field test request.")
    )

    list(
      status_code = 200L,
      body_text = .label_clusters_test_outer(payload, content),
      parsed = NULL
    )
  }

  review_dir <- file.path(tempdir(), "cocktailr-label-clusters-v3-category-fields")
  unlink(review_dir, recursive = TRUE, force = TRUE)

  res <- label_clusters(
    x = x,
    clusters = "c_1",
    model = "fake-model",
    variant = "label_primary_v1",
    timeout_sec = 1,
    review_dir = review_dir,
    verbose = FALSE,
    labels_for_imgs = TRUE,
    internal_prompt_version = "v3",
    request_fn = fake_request
  )

  expect_equal(res$summary$category_label[[1]], "dry grassland")
  expect_equal(res$summary$subcategory_labels[[1]], "base-rich; open")
  expect_equal(res$label_registry$category_label[[1]], "dry grassland")
  expect_equal(res$label_registry$subcategory_labels[[1]], "base-rich; open")
  expect_match(
    paste(readLines(res$summary$review_file[[1]], warn = FALSE), collapse = "\n"),
    "- Category label: `dry grassland`",
    fixed = TRUE
  )
})

test_that("label_clusters propagates decomposed v4 category fields", {
  x <- .build_label_clusters_test_cocktail()

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .label_clusters_request_stage(payload)
    content <- switch(
      stage,
      draft = paste(
        "The evidence supports dry grassland as the broad category.",
        "Base-rich and open are plausible subcategory modifiers."
      ),
      category = "dry grassland",
      subcategory = "base-rich; open",
      summary = "The fixed dry grassland category is refined by base-rich and open modifiers.",
      stop("Unexpected stage in v4 label_clusters category-field test request.")
    )

    list(
      status_code = 200L,
      body_text = .label_clusters_test_outer(payload, content),
      parsed = NULL
    )
  }

  review_dir <- file.path(tempdir(), "cocktailr-label-clusters-v4-category-fields")
  unlink(review_dir, recursive = TRUE, force = TRUE)

  res <- label_clusters(
    x = x,
    clusters = "c_1",
    model = "fake-model",
    variant = "label_primary_v1",
    timeout_sec = 1,
    review_dir = review_dir,
    verbose = FALSE,
    labels_for_imgs = TRUE,
    internal_prompt_version = "v4",
    request_fn = fake_request
  )

  expect_equal(res$summary$category_label[[1]], "dry grassland")
  expect_equal(res$summary$subcategory_labels[[1]], "base-rich; open")
  expect_equal(res$label_registry$category_label[[1]], "dry grassland")
  expect_equal(res$label_registry$subcategory_labels[[1]], "base-rich; open")
  expect_match(
    paste(readLines(res$summary$review_file[[1]], warn = FALSE), collapse = "\n"),
    "- Category label: `dry grassland`",
    fixed = TRUE
  )
})

test_that("label_clusters forwards and summarizes post-label subcategorization", {
  x <- .build_label_clusters_test_cocktail()

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .label_clusters_request_stage(payload)
    content <- switch(
      stage,
      selection = .label_clusters_test_selection_output(
        display_label = "dry base-rich grassland"
      ),
      summary = "The dry base-rich grassland signal recurs across the evidence bundle.",
      post_label_category = "dry grassland",
      post_label_uniqueness = "base-rich open",
      stop("Unexpected stage in v5 subcategorization label_clusters test request.")
    )

    list(
      status_code = 200L,
      body_text = .label_clusters_test_outer(payload, content),
      parsed = NULL
    )
  }

  review_dir <- file.path(tempdir(), "cocktailr-label-clusters-v5-subcategorization")
  unlink(review_dir, recursive = TRUE, force = TRUE)

  res <- label_clusters(
    x = x,
    clusters = "c_1",
    model = "fake-model",
    variant = "label_primary_v1",
    timeout_sec = 1,
    review_dir = review_dir,
    verbose = FALSE,
    labels_for_imgs = TRUE,
    internal_prompt_version = "v5",
    use_brainstorm = FALSE,
    use_subcategorization = TRUE,
    request_fn = fake_request
  )

  expect_equal(res$summary$category_label[[1]], "dry grassland")
  expect_equal(res$summary$subcategory_labels[[1]], "base-rich open")
  expect_equal(res$summary$subcategorization_enabled[[1]], TRUE)
  expect_equal(res$summary$subcategorization_strategy[[1]], "post_label")
  expect_equal(res$label_registry$category_label[[1]], "dry grassland")
  expect_equal(res$label_registry$subcategory_labels[[1]], "base-rich open")
  expect_match(
    paste(readLines(res$summary$review_file[[1]], warn = FALSE), collapse = "\n"),
    "- Category label: `dry grassland`",
    fixed = TRUE
  )
})

test_that("label_clusters forwards and summarizes staged general-name subcategorization", {
  x <- .build_label_clusters_test_cocktail()

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .label_clusters_request_stage(payload)
    content <- switch(
      stage,
      general_name = "dry grassland",
      uniqueness_detail = "base-rich open",
      summary = "The dry grassland general name is refined by base-rich open detail.",
      stop("Unexpected stage in v6 staged general-name label_clusters test request.")
    )

    list(
      status_code = 200L,
      body_text = .label_clusters_test_outer(payload, content),
      parsed = NULL
    )
  }

  review_dir <- file.path(tempdir(), "cocktailr-label-clusters-v6-staged-general-name")
  unlink(review_dir, recursive = TRUE, force = TRUE)

  res <- label_clusters(
    x = x,
    clusters = "c_1",
    model = "fake-model",
    variant = "label_primary_v1",
    timeout_sec = 1,
    review_dir = review_dir,
    verbose = FALSE,
    labels_for_imgs = TRUE,
    internal_prompt_version = "v6",
    use_brainstorm = FALSE,
    use_subcategorization = TRUE,
    request_fn = fake_request
  )

  expect_equal(res$summary$category_label[[1]], "dry grassland")
  expect_equal(res$summary$subcategory_labels[[1]], "base-rich open")
  expect_equal(res$summary$subcategorization_enabled[[1]], TRUE)
  expect_equal(res$summary$subcategorization_strategy[[1]], "staged_general_name")
  expect_equal(res$label_registry$category_label[[1]], "dry grassland")
  expect_equal(res$label_registry$subcategory_labels[[1]], "base-rich open")
  expect_match(
    paste(readLines(res$summary$review_file[[1]], warn = FALSE), collapse = "\n"),
    "- Category label: `dry grassland`",
    fixed = TRUE
  )
})

test_that("label_clusters writes review-adjacent model logs for each stage, including brainstorm", {
  x <- .build_label_clusters_test_cocktail()

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .label_clusters_request_stage(payload)
    content <- switch(
      stage,
      draft = .label_clusters_test_draft_text(),
      selection = .label_clusters_test_selection_output(),
      summary = "The same compact species core recurs across the evidence bundle.",
      stop("Unexpected stage in adjacent-model-log test request.")
    )

    list(
      status_code = 200L,
      body_text = .label_clusters_test_outer(payload, content),
      parsed = NULL
    )
  }

  review_dir <- file.path(tempdir(), "cocktailr-label-clusters-adjacent-model-logs")
  unlink(review_dir, recursive = TRUE, force = TRUE)

  res <- label_clusters(
    x = x,
    clusters = "c_1",
    model = "fake-model",
    variant = "label_primary_v1",
    timeout_sec = 1,
    review_dir = review_dir,
    verbose = FALSE,
    request_fn = fake_request
  )

  review_file <- res$summary$review_file[[1]]
  model_logs_dir <- res$results$c_1$review$model_logs_dir

  expect_true(file.exists(review_file))
  expect_true(dir.exists(model_logs_dir))
  expect_equal(dirname(model_logs_dir), dirname(review_file))

  brainstorm_prompt <- file.path(
    model_logs_dir,
    "stage1_brainstorm",
    "user_prompt.md"
  )
  label_answer <- file.path(
    model_logs_dir,
    "stage2_label_decision",
    "attempt1_label_primary_v1",
    "response_content.txt"
  )
  summary_answer <- file.path(
    model_logs_dir,
    "stage3_label_summary",
    "response_content.txt"
  )
  abstain_info <- file.path(
    model_logs_dir,
    "stage3_abstain_reason",
    "stage_info.json"
  )

  expect_true(file.exists(brainstorm_prompt))
  expect_true(file.exists(label_answer))
  expect_true(file.exists(summary_answer))
  expect_true(file.exists(abstain_info))

  expect_match(
    paste(readLines(brainstorm_prompt, warn = FALSE), collapse = "\n"),
    "Task mode: `draft_analysis_v1`",
    fixed = TRUE
  )
  expect_equal(
    paste(readLines(label_answer, warn = FALSE), collapse = "\n"),
    "compact species core"
  )
  expect_match(
    paste(readLines(summary_answer, warn = FALSE), collapse = "\n"),
    "compact species core",
    fixed = TRUE
  )

  abstain_info_json <- jsonlite::fromJSON(abstain_info, simplifyVector = FALSE)
  expect_true(isTRUE(abstain_info_json$skipped))
  expect_equal(abstain_info_json$skip_reason, "label_selected")
})

test_that("label_clusters uses text-only validator repair without falling back to JSON prompts", {
  x <- .build_label_clusters_test_cocktail()
  state <- new.env(parent = emptyenv())
  state$saw_validator_repair <- FALSE
  state$saw_validator_repair_outside_selection <- FALSE
  state$repair_iteration_started <- FALSE
  state$validation_calls <- 0L

  original_validate <- get("validate_cluster_label", envir = asNamespace("cocktailr"))
  mocked_validate <- function(x, evidence, ...) {
    state$validation_calls <- state$validation_calls + 1L
    base <- original_validate(x, evidence, ...)

    if (!identical(x$repair$source %||% NULL, "validator_text_only")) {
      issues <- .new_cluster_label_issue_table()
      issues <- rbind(
        issues,
        data.frame(
          severity = "error",
          category = "unsupported_claims",
          code = "forced_validator_repair_test",
          message = "Forced validator failure to exercise validator_text_only repair.",
          location = "label_summary",
          stringsAsFactors = FALSE
        )
      )
      base$validation_status <- "unsupported_claims"
      base$is_valid <- FALSE
      base$needs_human_review <- TRUE
      base$issues <- issues
    }

    base
  }

  assignInNamespace("validate_cluster_label", mocked_validate, ns = "cocktailr")
  withr::defer(
    assignInNamespace("validate_cluster_label", original_validate, ns = "cocktailr")
  )

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .label_clusters_request_stage(payload)
    user_text <- payload$messages[[2]]$content
    is_validator_repair <- grepl(
      "Validator feedback from the previous attempt:",
      user_text,
      fixed = TRUE
    )

    if (is_validator_repair && identical(stage, "selection")) {
      state$saw_validator_repair <- TRUE
      state$repair_iteration_started <- TRUE
      expect_false(grepl("Previous JSON output:", user_text, fixed = TRUE))
      expect_false(grepl("Return one complete corrected JSON object only.", user_text, fixed = TRUE))
      expect_false(grepl("\"schema_version\"", user_text, fixed = TRUE))
      expect_match(user_text, "Do not return JSON", fixed = TRUE)
    } else if (is_validator_repair) {
      state$saw_validator_repair_outside_selection <- TRUE
    }

    content <- switch(
      stage,
      draft = .label_clusters_test_draft_text(),
      selection = "compact species core",
      summary = if (isTRUE(state$repair_iteration_started)) {
        "A compact recurring species core is visible in the evidence bundle."
      } else {
        "This looks like a dry forest habitat core."
      },
      abstain_reason = "The evidence remains too mixed for a stable short label.",
      stop("Unexpected stage in validator-repair test request.")
    )

    list(
      status_code = 200L,
      body_text = .label_clusters_test_outer(payload, content),
      parsed = NULL
    )
  }

  review_dir <- file.path(tempdir(), "cocktailr-label-clusters-validator-repair")
  unlink(review_dir, recursive = TRUE, force = TRUE)

  res <- label_clusters(
    x = x,
    clusters = "c_1",
    model = "fake-model",
    variant = "label_primary_v1",
    timeout_sec = 1,
    max_iterations = 2L,
    review_dir = review_dir,
    verbose = FALSE,
    request_fn = fake_request
  )

  expect_true(isTRUE(state$saw_validator_repair))
  expect_false(isTRUE(state$saw_validator_repair_outside_selection))
  expect_gte(state$validation_calls, 2L)
  expect_equal(res$summary$run_status[[1]], "success")
  expect_true(res$summary$validation_status[[1]] %in% c("valid", "valid_with_warnings"))
  expect_true(isTRUE(res$summary$repair_used[[1]]))
  expect_equal(
    res$results$c_1$llm_result$output$label_summary,
    "A compact recurring species core is visible in the evidence bundle."
  )
  expect_equal(
    res$results$c_1$llm_result$repair$source %||% NA_character_,
    "validator_text_only"
  )
})

test_that("label_clusters keeps a valid long full label even when optional shortening is enabled", {
  x <- .build_label_clusters_test_cocktail()
  state <- new.env(parent = emptyenv())
  state$selection_calls <- 0L

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .label_clusters_request_stage(payload)

    content <- switch(
      stage,
      draft = .label_clusters_test_draft_text(),
      selection = {
        state$selection_calls <- state$selection_calls + 1L
        if (identical(state$selection_calls, 1L)) {
          "Dry base-rich grassland with sedge and thyme core"
        } else {
          "dry base-rich grassland"
        }
      },
      summary = "A dry base-rich grassland signal is consistently visible in the evidence bundle.",
      abstain_reason = "ABSTAIN",
      stop("Unexpected stage in shortening-repair test request.")
    )

    list(
      status_code = 200L,
      body_text = .label_clusters_test_outer(payload, content),
      parsed = NULL
    )
  }

  review_dir <- file.path(tempdir(), "cocktailr-label-clusters-shortening-repair")
  unlink(review_dir, recursive = TRUE, force = TRUE)

  res <- label_clusters(
    x = x,
    clusters = "c_1",
    model = "fake-model",
    variant = "label_primary_v1",
    internal_prompt_version = "v2",
    short_label_with_llm = TRUE,
    timeout_sec = 1,
    review_dir = review_dir,
    verbose = FALSE,
    request_fn = fake_request
  )

  expect_equal(state$selection_calls, 1L)
  expect_equal(res$summary$run_status[[1]], "success")
  expect_false(isTRUE(res$summary$repair_used[[1]]))
  expect_equal(
    res$results$c_1$llm_result$output$display_label,
    "Dry base-rich grassland with sedge and thyme core"
  )
  expect_match(
    res$results$c_1$llm_result$output$canonical_label,
    "^dry_base_rich_grassland",
    perl = TRUE
  )
  expect_equal(res$results$c_1$llm_result$workflow$label$selected_public_variant, "label_primary_v1")
  expect_null(res$results$c_1$llm_result$workflow$label$repair_source)
  expect_null(res$results$c_1$llm_result$workflow$label$repair_variant)
})

test_that("label_clusters review logs preserve normalized long labels without repair-history artifacts", {
  x <- .build_label_clusters_test_cocktail()
  state <- new.env(parent = emptyenv())
  state$selection_calls <- 0L

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .label_clusters_request_stage(payload)

    content <- switch(
      stage,
      draft = .label_clusters_test_draft_text(),
      selection = {
        state$selection_calls <- state$selection_calls + 1L
        "Semi-open cool woodland on base-rich soils with Vincetoxicum Galium understorey"
      },
      abstain_reason = "Signal remains too mixed for a stable short label.",
      summary = "",
      stop("Unexpected stage in shortening-log test request.")
    )

    list(
      status_code = 200L,
      body_text = .label_clusters_test_outer(payload, content),
      parsed = NULL
    )
  }

  review_dir <- file.path(tempdir(), "cocktailr-label-clusters-shortening-log-capture")
  unlink(review_dir, recursive = TRUE, force = TRUE)

  res <- label_clusters(
    x = x,
    clusters = "c_1",
    model = "fake-model",
    variant = "label_primary_v1",
    internal_prompt_version = "v2",
    short_label_with_llm = TRUE,
    timeout_sec = 1,
    review_dir = review_dir,
    verbose = FALSE,
    request_fn = fake_request
  )

  model_logs_dir <- res$results$c_1$review$model_logs_dir
  attempt_dir <- file.path(
    model_logs_dir,
    "stage2_label_decision",
    "attempt1_label_primary_v1"
  )
  repair_history_path <- file.path(attempt_dir, "repair_history.json")
  parsed_output_path <- file.path(attempt_dir, "parsed_output.json")

  expect_equal(state$selection_calls, 1L)
  expect_equal(res$summary$output_status[[1]], "labeled")
  expect_false(file.exists(repair_history_path))
  expect_true(file.exists(parsed_output_path))
  parsed_output <- jsonlite::fromJSON(parsed_output_path, simplifyVector = TRUE)
  expect_equal(
    parsed_output$display_label,
    "Semi-open cool woodland on base-rich soils with Vincetoxicum Galium understorey"
  )
  expect_equal(parsed_output$canonical_label, "semi_open_cool_woodland_")
  expect_equal(parsed_output$status, "labeled")
})

test_that("label_clusters keeps optional shortening disabled by default and falls back to softer prompts", {
  x <- .build_label_clusters_test_cocktail()
  state <- new.env(parent = emptyenv())
  state$selection_calls <- 0L

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .label_clusters_request_stage(payload)

    content <- switch(
      stage,
      draft = .label_clusters_test_draft_text(),
      selection = {
        state$selection_calls <- state$selection_calls + 1L
        if (identical(state$selection_calls, 1L)) {
          "Dry base-rich grassland with sedge and thyme core"
        } else {
          "dry base-rich grassland"
        }
      },
      summary = "A dry base-rich grassland signal is consistently visible in the evidence bundle.",
      abstain_reason = "ABSTAIN",
      stop("Unexpected stage in default-off shortening test request.")
    )

    list(
      status_code = 200L,
      body_text = .label_clusters_test_outer(payload, content),
      parsed = NULL
    )
  }

  review_dir <- file.path(tempdir(), "cocktailr-label-clusters-shortening-default-off")
  unlink(review_dir, recursive = TRUE, force = TRUE)

  res <- label_clusters(
    x = x,
    clusters = "c_1",
    model = "fake-model",
    variant = "label_primary_v1",
    internal_prompt_version = "v2",
    timeout_sec = 1,
    review_dir = review_dir,
    verbose = FALSE,
    request_fn = fake_request
  )

  expect_equal(state$selection_calls, 1L)
  expect_equal(res$summary$run_status[[1]], "success")
  expect_false(isTRUE(res$summary$repair_used[[1]]))
  expect_equal(res$results$c_1$llm_result$workflow$label$selected_public_variant, "label_primary_v1")
  expect_null(res$results$c_1$llm_result$workflow$label$repair_source)
  expect_null(res$results$c_1$llm_result$workflow$label$repair_variant)
})

test_that("cluster_evidence loads single txt, json, and yaml user_added_data files", {
  skip_if_not_installed("yaml")

  tmp_dir <- file.path(tempdir(), "cluster_user_added_single_files")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  txt_path <- file.path(tmp_dir, "notes.txt")
  json_path <- file.path(tmp_dir, "meta.json")
  yaml_path <- file.path(tmp_dir, "meta.yaml")

  writeLines(c("alpha note", "beta note"), txt_path, useBytes = TRUE)
  jsonlite::write_json(
    list(source = "manual", confidence = "low"),
    json_path,
    auto_unbox = TRUE,
    pretty = TRUE
  )
  writeLines(
    c("source: manual", "confidence: low"),
    yaml_path,
    useBytes = TRUE
  )

  specs <- list(
    list(path = txt_path, format = "txt", needle = "alpha note"),
    list(path = json_path, format = "json", needle = "\"source\": \"manual\""),
    list(path = yaml_path, format = "yaml", needle = "\"confidence\": \"low\"")
  )

  for (spec in specs) {
    ev <- .build_label_clusters_test_evidence(user_added_data = spec$path)
    prompt_text <- .serialize_cluster_evidence_prompt(ev)$text

    expect_true(isTRUE(ev$meta$user_added_data_present))
    expect_equal(ev$meta$user_added_data_source_type, "file")
    expect_equal(length(ev$user_added_data$entries), 1L)
    expect_equal(ev$user_added_data$entries[[1]]$name, basename(spec$path))
    expect_equal(ev$user_added_data$entries[[1]]$format, spec$format)
    expect_match(prompt_text, basename(spec$path), fixed = TRUE)
    expect_match(prompt_text, spec$needle, fixed = TRUE)
  }
})

test_that("label_clusters can combine constrained mode with directory-based user_added_data", {
  skip_if_not_installed("yaml")

  x <- .build_label_clusters_test_cocktail()
  ev <- .build_label_clusters_test_evidence()
  tmp_dir <- file.path(tempdir(), "cluster_user_added_dir")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  writeLines(c("alpha note", "second line"), file.path(tmp_dir, "01_notes.txt"), useBytes = TRUE)
  jsonlite::write_json(
    list(source = "manual", confidence = "low"),
    file.path(tmp_dir, "02_meta.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )
  writeLines(
    c("source: manual", "scope: broad"),
    file.path(tmp_dir, "03_meta.yaml"),
    useBytes = TRUE
  )
  writeBin(charToRaw("ignored"), file.path(tmp_dir, "04_skip.bin"))

  state <- new.env(parent = emptyenv())
  state$checked_prompt <- FALSE

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .label_clusters_request_stage(payload)

    if (identical(stage, "selection")) {
      state$checked_prompt <- TRUE
      user_text <- payload$messages[[2]]$content
      expect_match(user_text, "Constrained label mode is active.", fixed = TRUE)
      expect_match(user_text, "User-added data:", fixed = TRUE)
      expect_match(user_text, "01_notes.txt", fixed = TRUE)
      expect_match(user_text, "02_meta.json", fixed = TRUE)
      expect_match(user_text, "03_meta.yaml", fixed = TRUE)
    }

    content <- switch(
      stage,
      draft = .label_clusters_test_draft_text(),
      selection = .label_clusters_test_selection_output(),
      summary = "The short label stays broad and evidence-safe even after reading the user-added notes.",
      stop("Unexpected stage in test request.")
    )

    list(
      status_code = 200L,
      body_text = .label_clusters_test_outer(payload, content),
      parsed = NULL
    )
  }

  review_dir <- file.path(tempdir(), "cocktailr-label-clusters-user-added")
  unlink(review_dir, recursive = TRUE, force = TRUE)

  expect_warning(
    res <- label_clusters(
      x = x,
      clusters = "c_1",
      model = "fake-model",
      variant = "label_primary_v1",
      label_mode = "constrained",
      user_added_data = tmp_dir,
      timeout_sec = 1,
      review_dir = review_dir,
      verbose = FALSE,
      request_fn = fake_request
    ),
    "Ignored unsupported `user_added_data` files"
  )

  expect_true(isTRUE(state$checked_prompt))
  expect_equal(res$summary$run_status[[1]], "success")
  expect_equal(
    vapply(res$results$c_1$evidence$user_added_data$entries, `[[`, character(1), "name"),
    c("01_notes.txt", "02_meta.json", "03_meta.yaml")
  )
})

test_that("label_clusters warns and continues when the user_added_data directory is missing", {
  x <- .build_label_clusters_test_cocktail()
  ev <- .build_label_clusters_test_evidence()
  missing_dir <- file.path(tempdir(), "missing_cluster_user_added_dir")
  unlink(missing_dir, recursive = TRUE, force = TRUE)

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .label_clusters_request_stage(payload)
    content <- switch(
      stage,
      draft = .label_clusters_test_draft_text(),
      selection = .label_clusters_test_selection_output(),
      summary = "The label stays grounded in the cluster evidence alone.",
      stop("Unexpected stage in test request.")
    )

    list(
      status_code = 200L,
      body_text = .label_clusters_test_outer(payload, content),
      parsed = NULL
    )
  }

  expect_warning(
    res <- label_clusters(
      x = x,
      clusters = "c_1",
      model = "fake-model",
      variant = "label_primary_v1",
      user_added_data = missing_dir,
      timeout_sec = 1,
      verbose = FALSE,
      request_fn = fake_request
    ),
    "directory was not found"
  )

  expect_equal(res$summary$run_status[[1]], "success")
  expect_false(isTRUE(res$results$c_1$evidence$meta$user_added_data_present))
})

test_that("cluster_evidence warns and continues when a directory has no supported files", {
  tmp_dir <- file.path(tempdir(), "cluster_user_added_unsupported_only")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  writeBin(charToRaw("ignored"), file.path(tmp_dir, "notes.bin"))

  expect_warning(
    expect_warning(
      ev <- .build_label_clusters_test_evidence(user_added_data = tmp_dir),
      "Ignored unsupported `user_added_data` files"
    ),
    "No supported `user_added_data` files were found"
  )

  expect_false(isTRUE(ev$meta$user_added_data_present))
  expect_null(ev$user_added_data)
})

test_that("label_clusters keeps honest abstain output and separate public chaotic-cluster fallback", {
  x <- .build_label_clusters_test_cocktail()
  ev <- .build_label_clusters_test_evidence()
  state <- new.env(parent = emptyenv())
  state$selection_calls <- 0L

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .label_clusters_request_stage(payload)
    content <- switch(
      stage,
      draft = .label_clusters_test_draft_text(),
      selection = {
        state$selection_calls <- state$selection_calls + 1L
        .label_clusters_test_selection_output(
          abstain = TRUE,
          abstain_text = "ABSTAIN"
        )
      },
      abstain_reason = "",
      stop("Unexpected stage in test request.")
    )

    list(
      status_code = 200L,
      body_text = .label_clusters_test_outer(payload, content),
      parsed = NULL
    )
  }

  review_dir <- file.path(tempdir(), "cocktailr-label-clusters-all-abstain")
  unlink(review_dir, recursive = TRUE, force = TRUE)

  res <- label_clusters(
    x = x,
    clusters = "c_1",
    model = "fake-model",
    variant = "label_primary_v1",
    timeout_sec = 1,
    review_dir = review_dir,
    verbose = FALSE,
    labels_for_imgs = TRUE,
    request_fn = fake_request
  )

  expect_equal(state$selection_calls, 3L)
  expect_equal(res$summary$run_status[[1]], "success")
  expect_equal(res$summary$output_status[[1]], "abstain")
  expect_equal(res$results$c_1$llm_result$output$status, "abstain")
  expect_null(res$results$c_1$llm_result$output$canonical_label)
  expect_true(.is_non_empty_scalar_character(res$results$c_1$llm_result$output$explanation))
  expect_equal(res$label_registry$public_display_label[[1]], "Chaotic Cluster")
  expect_equal(res$label_registry$public_canonical_label[[1]], "chaotic_cluster")
  expect_equal(res$label_registry$public_label_source[[1]], "post_abstain_fallback")
  expect_equal(res$label_registry$selected_label_variant[[1]], "selection_all_abstain")
})
