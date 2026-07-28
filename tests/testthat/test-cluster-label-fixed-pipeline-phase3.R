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
  if (grepl("Task mode: `category_decision_", user_text, fixed = TRUE)) {
    return("category")
  }
  if (grepl("Task mode: `subcategory_decision_", user_text, fixed = TRUE)) {
    return("subcategory")
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
  expect_equal(
    res$output$canonical_label,
    "dry_base_rich_grassland_"
  )
  expect_equal(res$workflow$label$selected_public_variant, "label_primary_v1")
  expect_equal(length(res$workflow$label$attempts), 1L)
  expect_null(res$workflow$label$repair_source)
  expect_null(res$workflow$label$repair_variant)
  expect_true(.is_non_empty_scalar_character(res$output$label_summary))
  expect_equal(res$workflow$label$attempts[[1]]$attempts, 1L)
})

test_that("decomposed v4 flow retries malformed clean category answers", {
  ev <- .build_phase3_test_cluster_evidence()
  state <- new.env(parent = emptyenv())
  state$category_calls <- 0L
  state$stages <- character(0)

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .phase3_request_stage(payload)
    state$stages <- c(state$stages, stage)
    content <- switch(
      stage,
      draft = paste(
        "The evidence points toward a dry grassland category.",
        "A base-rich and open subcategory is plausible from the species signal."
      ),
      category = {
        state$category_calls <- state$category_calls + 1L
        if (identical(state$category_calls, 1L)) {
          "CATEGORY_LABEL: dry grassland"
        } else {
          "dry grassland"
        }
      },
      subcategory = "base-rich; open",
      summary = "The fixed dry grassland category is refined by base-rich and open modifiers.",
      stop("Unexpected stage in decomposed v4 test request.")
    )

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
    internal_prompt_version = "v4",
    use_brainstorm = TRUE,
    short_label_with_llm = FALSE,
    max_retries = 1L,
    timeout_sec = 1,
    request_fn = fake_request
  )

  expect_equal(res$output$status, "labeled")
  expect_equal(res$output$category_label, "dry grassland")
  expect_equal(res$output$subcategory_labels, c("base-rich", "open"))
  expect_equal(res$output$display_label, "dry grassland")
  expect_equal(state$category_calls, 2L)
  expect_true(any(state$stages == "draft"))
  expect_true(any(state$stages == "category"))
  expect_true(any(state$stages == "subcategory"))
  expect_true(any(state$stages == "summary"))
  expect_equal(res$workflow$category$attempts[[1]]$attempts, 2L)
})

test_that("decomposed v4 clean category honors max_retries zero", {
  ev <- .build_phase3_test_cluster_evidence()
  state <- new.env(parent = emptyenv())
  state$category_calls <- 0L

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .phase3_request_stage(payload)
    content <- switch(
      stage,
      draft = "The evidence points toward a dry grassland category.",
      category = {
        state$category_calls <- state$category_calls + 1L
        if (identical(state$category_calls, 1L)) {
          "CATEGORY_LABEL: dry grassland"
        } else {
          "dry grassland"
        }
      },
      subcategory = "base-rich; open",
      summary = "The fixed dry grassland category is refined by base-rich and open modifiers.",
      stop("Unexpected stage in decomposed v4 max_retries zero test request.")
    )

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
    internal_prompt_version = "v4",
    use_brainstorm = TRUE,
    short_label_with_llm = FALSE,
    max_retries = 0L,
    timeout_sec = 1,
    request_fn = fake_request
  )

  expect_equal(res$output$status, "labeled")
  expect_equal(state$category_calls, 2L)
  expect_equal(res$workflow$category$attempts[[1]]$result, "failed_after_retry")
  expect_equal(res$workflow$category$attempts[[1]]$attempts, 1L)
  expect_equal(res$workflow$category$attempts[[2]]$result, "category_ready")
  expect_equal(res$workflow$category$attempts[[2]]$attempts, 1L)
})

test_that("decomposed v4 metadata counts no-brainstorm labeled stages", {
  ev <- .build_phase3_test_cluster_evidence()
  log_dir <- file.path(tempdir(), "cocktailr_v4_no_brainstorm_labeled_metadata")
  unlink(log_dir, recursive = TRUE, force = TRUE)
  state <- new.env(parent = emptyenv())
  state$stages <- character(0)

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .phase3_request_stage(payload)
    state$stages <- c(state$stages, stage)
    content <- switch(
      stage,
      category = "dry grassland",
      subcategory = "base-rich; open",
      summary = "The fixed dry grassland category is refined by base-rich and open modifiers.",
      stop("Unexpected stage in decomposed v4 no-brainstorm labeled metadata test request.")
    )

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
    internal_prompt_version = "v4",
    use_brainstorm = FALSE,
    short_label_with_llm = FALSE,
    max_retries = 0L,
    debug = TRUE,
    log_dir = log_dir,
    timeout_sec = 1,
    request_fn = fake_request
  )
  metadata <- jsonlite::fromJSON(res$logs$metadata, simplifyVector = TRUE)

  expect_equal(res$output$status, "labeled")
  expect_false(any(state$stages == "draft"))
  expect_equal(state$stages, c("category", "subcategory", "summary"))
  expect_equal(metadata$executed_stages, 3L)
})

test_that("decomposed v4 metadata counts no-brainstorm abstain stages", {
  ev <- .build_phase3_test_cluster_evidence()
  log_dir <- file.path(tempdir(), "cocktailr_v4_no_brainstorm_abstain_metadata")
  unlink(log_dir, recursive = TRUE, force = TRUE)
  state <- new.env(parent = emptyenv())
  state$stages <- character(0)

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .phase3_request_stage(payload)
    state$stages <- c(state$stages, stage)
    content <- switch(
      stage,
      category = "ABSTAIN",
      abstain_reason = "The evidence does not support a stable vegetation category.",
      stop("Unexpected stage in decomposed v4 no-brainstorm abstain metadata test request.")
    )

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
    internal_prompt_version = "v4",
    use_brainstorm = FALSE,
    short_label_with_llm = FALSE,
    max_retries = 0L,
    debug = TRUE,
    log_dir = log_dir,
    timeout_sec = 1,
    request_fn = fake_request
  )
  metadata <- jsonlite::fromJSON(res$logs$metadata, simplifyVector = TRUE)

  expect_equal(res$output$status, "abstain")
  expect_false(any(state$stages == "draft"))
  expect_false(any(state$stages == "subcategory"))
  expect_false(any(state$stages == "summary"))
  expect_equal(tail(state$stages, 1L), "abstain_reason")
  expect_equal(metadata$executed_stages, 2L)
})

test_that("canonical contract projection keeps three words and a trailing underscore", {
  expect_equal(
    cocktailr:::.cluster_label_project_canonical_label_for_contract(
      "Semi-open cool woodland on base-rich soils with Vincetoxicum Galium understorey"
    ),
    "semi_open_cool_woodland_"
  )

  expect_equal(
    cocktailr:::.cluster_label_project_canonical_label_for_contract(
      "Dry Meadow Edge"
    ),
    "dry_meadow_edge"
  )
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

test_that("label-decision parser preserves category and subcategory fields", {
  parsed <- cocktailr:::.parse_cluster_label_label_decision_text(
    content = paste(
      "LABEL: dry base-rich grassland",
      "CATEGORY_LABEL: dry grassland",
      "SUBCATEGORY_LABELS: base-rich; open",
      sep = "\n"
    ),
    cluster_id = "c_1"
  )
  parse_info <- attr(parsed, "cluster_label_parse_info")

  expect_equal(parsed$cluster_id, "c_1")
  expect_equal(parsed$status, "labeled")
  expect_equal(parsed$display_label, "dry base-rich grassland")
  expect_equal(parsed$canonical_label, "dry_base_rich_grassland")
  expect_equal(parsed$category_label, "dry grassland")
  expect_equal(parsed$subcategory_labels, c("base-rich", "open"))
  expect_equal(parse_info$parsing_rule, "experimental_category_fields")
})

test_that("clean category parser rejects technical prefixes and explanations", {
  expect_error(
    cocktailr:::.parse_cluster_label_category_decision_text(
      content = "CATEGORY_LABEL: dry grassland",
      cluster_id = "c_1"
    ),
    "technical prefixes"
  )
  expect_error(
    cocktailr:::.parse_cluster_label_category_decision_text(
      content = "dry grassland because the plots are open",
      cluster_id = "c_1"
    ),
    "explanatory prose"
  )

  parsed <- cocktailr:::.parse_cluster_label_category_decision_text(
    content = "dry grassland",
    cluster_id = "c_1"
  )
  expect_equal(parsed$status, "category_ready")
  expect_equal(parsed$category_label, "dry grassland")
})

test_that("clean subcategory parser accepts semicolon names and rejects prefixed lists", {
  expect_error(
    cocktailr:::.parse_cluster_label_subcategory_decision_text(
      content = "SUBCATEGORY_LABELS: base-rich; open",
      cluster_id = "c_1"
    ),
    "technical prefixes"
  )

  parsed <- cocktailr:::.parse_cluster_label_subcategory_decision_text(
    content = "base-rich; open",
    cluster_id = "c_1"
  )
  expect_equal(parsed$status, "subcategory_ready")
  expect_equal(parsed$subcategory_labels, c("base-rich", "open"))

  none <- cocktailr:::.parse_cluster_label_subcategory_decision_text(
    content = "none",
    cluster_id = "c_1"
  )
  expect_equal(none$subcategory_labels, character(0))
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

test_that("fixed pipeline leaves post-label subcategorization disabled by default", {
  ev <- .build_phase3_test_cluster_evidence()
  state <- new.env(parent = emptyenv())
  state$stages <- character(0)

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .phase3_request_stage(payload)
    state$stages <- c(state$stages, stage)
    content <- switch(
      stage,
      selection = .phase3_selection_text(display_label = "compact species core"),
      summary = "The same compact species core recurs across the evidence bundle.",
      post_label_category = stop("Subcategorization stage should be disabled."),
      post_label_uniqueness = stop("Subcategorization stage should be disabled."),
      stop("Unexpected stage in disabled subcategorization test request.")
    )

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
    use_subcategorization = FALSE,
    max_retries = 0L,
    timeout_sec = 1,
    request_fn = fake_request
  )

  expect_equal(state$stages, c("selection", "summary"))
  expect_null(res$workflow$subcategorization %||% NULL)
  expect_false(isTRUE(res$metadata$subcategorization_enabled %||% FALSE))
  expect_null(res$output$category_label %||% NULL)
  expect_equal(res$output$subcategory_labels %||% character(0), character(0))
})

test_that("fixed pipeline can classify completed labels after summary", {
  ev <- .build_phase3_test_cluster_evidence()
  log_dir <- file.path(tempdir(), "cocktailr_v5_post_label_subcategorization")
  unlink(log_dir, recursive = TRUE, force = TRUE)
  state <- new.env(parent = emptyenv())
  state$stages <- character(0)

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .phase3_request_stage(payload)
    state$stages <- c(state$stages, stage)
    content <- switch(
      stage,
      selection = .phase3_selection_text(display_label = "dry base-rich grassland"),
      summary = "The dry base-rich grassland signal recurs across the evidence bundle.",
      post_label_category = {
        expect_match(payload$messages[[2]]$content, "Fixed label:", fixed = TRUE)
        expect_match(payload$messages[[2]]$content, "dry base-rich grassland", fixed = TRUE)
        expect_match(payload$messages[[2]]$content, "Fixed description:", fixed = TRUE)
        expect_false(grepl("Cluster:", payload$messages[[2]]$content, fixed = TRUE))
        expect_false(grepl("Dataset context:", payload$messages[[2]]$content, fixed = TRUE))
        expect_false(grepl("Draft", payload$messages[[2]]$content, fixed = TRUE))
        "dry grassland"
      },
      post_label_uniqueness = {
        expect_match(payload$messages[[2]]$content, "General name:", fixed = TRUE)
        expect_match(payload$messages[[2]]$content, "dry grassland", fixed = TRUE)
        expect_match(payload$messages[[2]]$content, "Fixed label:", fixed = TRUE)
        expect_match(payload$messages[[2]]$content, "Fixed description:", fixed = TRUE)
        expect_false(grepl("Cluster:", payload$messages[[2]]$content, fixed = TRUE))
        expect_false(grepl("Dataset context:", payload$messages[[2]]$content, fixed = TRUE))
        expect_false(grepl("Draft", payload$messages[[2]]$content, fixed = TRUE))
        "base-rich; open\nmoisture: low"
      },
      stop("Unexpected stage in enabled subcategorization test request.")
    )

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
    internal_prompt_version = "v5",
    use_brainstorm = FALSE,
    use_subcategorization = TRUE,
    max_retries = 0L,
    debug = TRUE,
    log_dir = log_dir,
    timeout_sec = 1,
    request_fn = fake_request
  )
  metadata <- jsonlite::fromJSON(res$logs$metadata, simplifyVector = TRUE)

  expect_equal(state$stages, c("selection", "summary", "post_label_category", "post_label_uniqueness"))
  expect_equal(res$output$status, "labeled")
  expect_equal(res$output$display_label, "dry base-rich grassland")
  expect_equal(
    res$output$label_summary,
    "The dry base-rich grassland signal recurs across the evidence bundle."
  )
  expect_equal(res$output$category_label, "dry grassland")
  expect_equal(res$output$subcategory_labels, "base-rich open moisture low")
  expect_true(isTRUE(res$metadata$subcategorization_enabled))
  expect_equal(res$metadata$subcategorization_strategy, "post_label")
  expect_equal(res$workflow$subcategorization$output$status, "subcategorization_ready")
  expect_equal(res$workflow$subcategorization$category$output$status, "category_ready")
  expect_equal(res$workflow$subcategorization$uniqueness$output$status, "uniqueness_ready")
  expect_equal(metadata$subcategorization_enabled, TRUE)
  expect_equal(metadata$subcategorization_strategy, "post_label")
  expect_equal(metadata$executed_stages, 4L)
})

test_that("post-label uniqueness normalizes weak model punctuation and line breaks", {
  parsed <- cocktailr:::.parse_cluster_label_clean_name_text(
    content = "dry; base-rich\ncool: shaded (edge), meadow & scrub.",
    cluster_id = "c_1",
    stage_name = "post_label_uniqueness",
    field_name = "subcategory_labels",
    allow_semicolon = FALSE,
    allow_none = TRUE,
    allow_abstain = FALSE
  )

  expect_equal(parsed$value, "dry base-rich cool shaded edge meadow scrub")
  expect_equal(parsed$parse_info$nonempty_line_count, 2L)

  long <- cocktailr:::.parse_cluster_label_clean_name_text(
    content = "open sunny moderately dry base-rich cool meadow transition edge",
    cluster_id = "c_1",
    stage_name = "post_label_uniqueness",
    field_name = "subcategory_labels",
    allow_semicolon = FALSE,
    allow_none = TRUE,
    allow_abstain = FALSE
  )

  expect_equal(
    long$value,
    "open sunny moderately dry base-rich cool meadow transition edge"
  )

  none <- cocktailr:::.parse_cluster_label_clean_name_text(
    content = "none.",
    cluster_id = "c_1",
    stage_name = "post_label_uniqueness",
    field_name = "subcategory_labels",
    allow_semicolon = FALSE,
    allow_none = TRUE,
    allow_abstain = FALSE
  )

  expect_equal(tolower(none$value), "none")
})

test_that("fixed pipeline preserves labels when post-label subcategorization fails", {
  ev <- .build_phase3_test_cluster_evidence()

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .phase3_request_stage(payload)
    content <- switch(
      stage,
      selection = .phase3_selection_text(display_label = "compact species core"),
      summary = "The same compact species core recurs across the evidence bundle.",
      post_label_category = "CATEGORY: broad group",
      post_label_uniqueness = stop("Uniqueness stage should not run after category failure."),
      stop("Unexpected stage in failing subcategorization test request.")
    )

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
    internal_prompt_version = "v5",
    use_brainstorm = FALSE,
    use_subcategorization = TRUE,
    max_retries = 0L,
    timeout_sec = 1,
    request_fn = fake_request
  )

  expect_equal(res$output$status, "labeled")
  expect_equal(res$output$display_label, "compact species core")
  expect_equal(
    res$output$label_summary,
    "The same compact species core recurs across the evidence bundle."
  )
  expect_null(res$output$category_label %||% NULL)
  expect_equal(res$output$subcategory_labels, character(0))
  expect_equal(res$workflow$subcategorization$output$status, "subcategorization_failed")
  expect_true(isTRUE(res$workflow$subcategorization$fallback_used))
  expect_true(isTRUE(res$workflow$subcategorization$skipped))
})

test_that("fixed pipeline preserves category when post-label uniqueness fails", {
  ev <- .build_phase3_test_cluster_evidence()

  fake_request <- function(url, payload, timeout_sec) {
    stage <- .phase3_request_stage(payload)
    content <- switch(
      stage,
      selection = .phase3_selection_text(display_label = "compact species core"),
      summary = "The same compact species core recurs across the evidence bundle.",
      post_label_category = "dry grassland",
      post_label_uniqueness = "SUBCATEGORY: base-rich open",
      stop("Unexpected stage in partial subcategorization test request.")
    )

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
    internal_prompt_version = "v5",
    use_brainstorm = FALSE,
    use_subcategorization = TRUE,
    max_retries = 0L,
    timeout_sec = 1,
    request_fn = fake_request
  )

  expect_equal(res$output$status, "labeled")
  expect_equal(res$output$category_label, "dry grassland")
  expect_equal(res$output$subcategory_labels, character(0))
  expect_equal(res$workflow$subcategorization$output$status, "subcategorization_partial")
  expect_true(isTRUE(res$workflow$subcategorization$fallback_used))
  expect_true(isTRUE(res$workflow$subcategorization$uniqueness$skipped))
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

test_that("overlong labels are normalized into compact canonical labels instead of exhausting the ladder", {
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

  expect_equal(state$selection_calls, 1L)
  expect_equal(res$output$status, "labeled")
  expect_equal(
    res$output$display_label,
    "Semi-open cool woodland on base-rich soils with Vincetoxicum Galium understorey"
  )
  expect_equal(res$output$canonical_label, "semi_open_cool_woodland_")
  expect_equal(res$workflow$label$selected_public_variant, "label_primary_v1")
  expect_null(res$workflow$label$repair_source %||% NULL)
  expect_null(res$workflow$label$attempts[[1]]$repair_source %||% NULL)
  expect_equal(res$workflow$label$attempts[[1]]$result, "labeled")
  expect_equal(res$workflow$label$attempts[[1]]$attempts, 1L)
  expect_length(res$workflow$label$failure_messages, 0L)
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
