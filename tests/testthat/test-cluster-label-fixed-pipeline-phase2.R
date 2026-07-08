.build_phase2_test_cluster_object <- function() {
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

.build_phase2_test_cluster_evidence <- function(...) {
  cluster_evidence(
    .build_phase2_test_cluster_object(),
    cluster = "c_1",
    top_n_phi = 3,
    n_prototype_plots = 2,
    n_borderline_plots = 2,
    ...
  )
}

.phase2_llm_outer <- function(payload, content) {
  jsonlite::toJSON(
    list(
      model = payload$model,
      created_at = "2026-06-30T12:00:00Z",
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

.phase2_selection_text <- function(
    display_label = NULL,
    abstain = FALSE,
    abstain_text = "ABSTAIN"
) {
  if (isTRUE(abstain)) {
    return(abstain_text)
  }

  display_label %||% "compact species core"
}

.phase2_request_stage <- function(payload) {
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

test_that("llm_label_cluster always uses the fixed pipeline and can skip brainstorm", {
  ev <- .build_phase2_test_cluster_evidence()

  expect_warning(
    req <- llm_label_cluster(
      evidence = ev,
      model = "gemma4:12b",
      variant = "label_primary_v1",
      workflow_steps = 1L,
      use_brainstorm = FALSE,
      dry_run = TRUE
    ),
    "deprecated"
  )

  expect_s3_class(req, "cluster_label_request")
  expect_equal(req$workflow_steps, 3L)
  expect_true(isTRUE(req$workflow$draft$skipped))
  expect_match(
    req$workflow$label$variants[[1]]$prompt$user,
    "Brainstorm was disabled for this run.",
    fixed = TRUE
  )
  expect_match(
    req$workflow$summary$prompt$user,
    "Brainstorm was disabled for this run.",
    fixed = TRUE
  )
  expect_match(
    req$workflow$summary$prompt$user,
    "Chosen short label (fixed; do not replace it):",
    fixed = TRUE
  )
})

test_that("selection dry-run receives compact candidate labels extracted in code", {
  ev <- .build_phase2_test_cluster_evidence()

  req <- llm_label_cluster(
    evidence = ev,
    model = "gemma4:12b",
    variant = "label_primary_v1",
    use_brainstorm = TRUE,
    dry_run = TRUE
  )

  selection_user_prompt <- req$workflow$label$variants[[1]]$prompt$user

  expect_match(
    selection_user_prompt,
    "Code-extracted candidate labels:",
    fixed = TRUE
  )
  expect_match(
    selection_user_prompt,
    "mixed meadow assemblage",
    fixed = TRUE
  )
})

test_that("summary dry-run prompt uses chosen label plus brainstorm text without raw cluster evidence", {
  ev <- .build_phase2_test_cluster_evidence()

  req <- llm_label_cluster(
    evidence = ev,
    model = "gemma4:12b",
    variant = "label_primary_v1",
    use_brainstorm = TRUE,
    dry_run = TRUE
  )

  summary_user_prompt <- req$workflow$summary$prompt$user

  expect_match(
    summary_user_prompt,
    "Chosen short label (fixed; do not replace it):",
    fixed = TRUE
  )
  expect_match(
    summary_user_prompt,
    "placeholder label",
    fixed = TRUE
  )
  expect_match(
    summary_user_prompt,
    "Brainstorm output:",
    fixed = TRUE
  )
  expect_match(
    summary_user_prompt,
    "Possible interpretations:",
    fixed = TRUE
  )
  expect_match(
    summary_user_prompt,
    "mixed meadow assemblage",
    fixed = TRUE
  )
  expect_false(grepl("Cluster evidence:", summary_user_prompt, fixed = TRUE))
})

test_that("cluster_evidence loads directory-based user_added_data in stable order", {
  tmp_dir <- file.path(tempdir(), "cluster_user_added_phase2")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  writeLines(
    c("alpha note", "second line"),
    file.path(tmp_dir, "01_notes.txt"),
    useBytes = TRUE
  )
  writeLines(
    '{\"source\":\"manual\",\"confidence\":\"low\"}',
    file.path(tmp_dir, "02_meta.json"),
    useBytes = TRUE
  )
  writeBin(
    charToRaw("ignored"),
    file.path(tmp_dir, "03_skip.bin")
  )

  expect_warning(
    ev <- .build_phase2_test_cluster_evidence(user_added_data = tmp_dir),
    "Ignored unsupported `user_added_data` files"
  )

  expect_true(isTRUE(ev$meta$user_added_data_present))
  expect_equal(ev$meta$user_added_data_source_type, "directory")
  expect_equal(
    vapply(ev$user_added_data$entries, `[[`, character(1), "name"),
    c("01_notes.txt", "02_meta.json")
  )
  expect_lte(ev$user_added_data$total_chars, 1000L)

  prompt_text <- .serialize_cluster_evidence_prompt(ev)$text
  expect_match(prompt_text, "User-added data:", fixed = TRUE)
  expect_match(prompt_text, "01_notes.txt", fixed = TRUE)
  expect_match(prompt_text, "02_meta.json", fixed = TRUE)
})

test_that("label_clusters skips draft stage when use_brainstorm is FALSE", {
  x <- .build_phase2_test_cluster_object()
  ev <- .build_phase2_test_cluster_evidence()
  state <- new.env(parent = emptyenv())
  state$stages <- character(0)
  state$n <- 0L

  fake_request <- function(url, payload, timeout_sec) {
    state$n <- state$n + 1L
    stage <- .phase2_request_stage(payload)
    state$stages[state$n] <- stage

    content <- if (identical(stage, "selection")) {
      .phase2_selection_text(
        display_label = "compact species core"
      )
    } else if (identical(stage, "summary")) {
      "The evidence shows a stable short species-core signal."
    } else {
      stop("Unexpected stage in test request.")
    }

    list(
      status_code = 200L,
      body_text = .phase2_llm_outer(payload, content),
      parsed = NULL
    )
  }

  review_dir <- file.path(tempdir(), "cluster_label_phase2_review")
  unlink(review_dir, recursive = TRUE, force = TRUE)

  res <- label_clusters(
    x = x,
    clusters = "c_1",
    model = "fake-model",
    variant = "label_primary_v1",
    use_brainstorm = FALSE,
    review_dir = review_dir,
    timeout_sec = 1,
    verbose = FALSE,
    request_fn = fake_request
  )

  expect_equal(state$stages, c("selection", "summary"))
  expect_equal(res$summary$run_status[[1]], "success")
  expect_equal(res$results$c_1$llm_result$workflow_steps, 3L)
  expect_true(isTRUE(res$results$c_1$llm_result$workflow$draft$skipped))
  expect_equal(res$results$c_1$llm_result$output$status, "labeled")
})
