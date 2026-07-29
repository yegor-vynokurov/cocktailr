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
  expect_match(
    req$workflow$draft$prompt$user_path,
    "inst/prompts/internal_cluster_labeling/v1/user_draft_analysis_v1.md",
    fixed = TRUE
  )
  expect_match(
    req$workflow$summary$prompt$user_path,
    "inst/prompts/internal_cluster_labeling/v1/user_label_summary_pass_v2.md",
    fixed = TRUE
  )
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

test_that("llm_label_cluster validates the selected internal prompt version folder", {
  ev <- .build_test_cluster_evidence()

  expect_error(
    llm_label_cluster(
      evidence = ev,
      model = "gemma4:12b",
      variant = "label_primary_v1",
      internal_prompt_version = "v999",
      dry_run = TRUE
    ),
    "Internal prompt version folder does not exist",
    fixed = TRUE
  )
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

test_that("post-label subcategorization prompt bundle is isolated in v5", {
  ev <- .build_test_cluster_evidence()
  v1_dir <- system.file(
    "prompts",
    "internal_cluster_labeling",
    "v1",
    package = "cocktailr"
  )
  v5_dir <- system.file(
    "prompts",
    "internal_cluster_labeling",
    "v5",
    package = "cocktailr"
  )

  ordinary_files <- c(
    "user_draft_analysis_v1.md",
    "user_label_decision_primary_v2.md",
    "user_label_decision_soft_v2.md",
    "user_label_decision_broad_v2.md",
    "user_abstain_reason_pass_v2.md"
  )
  for (ordinary_file in ordinary_files) {
    expect_true(file.exists(file.path(v5_dir, ordinary_file)))
    expect_equal(
      readLines(file.path(v5_dir, ordinary_file), warn = FALSE),
      readLines(file.path(v1_dir, ordinary_file), warn = FALSE)
    )
  }

  req <- llm_label_cluster(
    evidence = ev,
    model = "fake-model",
    internal_prompt_version = "v5",
    use_brainstorm = FALSE,
    use_subcategorization = TRUE,
    dry_run = TRUE
  )

  expect_equal(req$workflow$subcategorization$category$prompt$task_type, "post_label_category")
  expect_equal(req$workflow$subcategorization$uniqueness$prompt$task_type, "post_label_uniqueness")
  expect_match(
    req$workflow$subcategorization$category$prompt$user_path,
    "inst/prompts/internal_cluster_labeling/v5/user_post_label_subcategorization_v1.md",
    fixed = TRUE
  )
  expect_match(
    req$workflow$subcategorization$uniqueness$prompt$user_path,
    "inst/prompts/internal_cluster_labeling/v5/user_post_label_uniqueness_v1.md",
    fixed = TRUE
  )
  category_prompt_text <- req$workflow$subcategorization$category$prompt$user
  uniqueness_prompt_text <- req$workflow$subcategorization$uniqueness$prompt$user
  expect_match(category_prompt_text, "general name", fixed = TRUE)
  expect_match(uniqueness_prompt_text, "could differ", fixed = TRUE)
  expect_false(grepl("Cluster:", category_prompt_text, fixed = TRUE))
  expect_false(grepl("Dataset context:", category_prompt_text, fixed = TRUE))
  expect_false(grepl("Draft", category_prompt_text, fixed = TRUE))
  expect_false(grepl("Cluster:", uniqueness_prompt_text, fixed = TRUE))
  expect_false(grepl("Dataset context:", uniqueness_prompt_text, fixed = TRUE))
  expect_false(grepl("Draft", uniqueness_prompt_text, fixed = TRUE))
})

test_that("staged general-name prompt bundle is isolated in v6", {
  ev <- .build_test_cluster_evidence()
  v1_dir <- system.file(
    "prompts",
    "internal_cluster_labeling",
    "v1",
    package = "cocktailr"
  )
  v6_dir <- system.file(
    "prompts",
    "internal_cluster_labeling",
    "v6",
    package = "cocktailr"
  )

  ordinary_files <- c(
    "user_draft_analysis_v1.md",
    "user_label_decision_primary_v2.md",
    "user_label_decision_soft_v2.md",
    "user_label_decision_broad_v2.md",
    "user_abstain_reason_pass_v2.md"
  )
  for (ordinary_file in ordinary_files) {
    expect_true(file.exists(file.path(v6_dir, ordinary_file)))
    expect_equal(
      readLines(file.path(v6_dir, ordinary_file), warn = FALSE),
      readLines(file.path(v1_dir, ordinary_file), warn = FALSE)
    )
  }
  expect_true(file.exists(file.path(v6_dir, "user_label_summary_pass_v2.md")))

  req <- llm_label_cluster(
    evidence = ev,
    model = "fake-model",
    internal_prompt_version = "v6",
    use_brainstorm = FALSE,
    use_subcategorization = TRUE,
    dry_run = TRUE
  )

  expect_equal(req$workflow$category$prompt$task_type, "general_name_decision")
  expect_equal(req$workflow$subcategory$prompt$task_type, "uniqueness_detail_decision")
  expect_match(
    req$workflow$category$prompt$user_path,
    "inst/prompts/internal_cluster_labeling/v6/user_general_name_decision_v1.md",
    fixed = TRUE
  )
  expect_match(
    req$workflow$subcategory$prompt$user_path,
    "inst/prompts/internal_cluster_labeling/v6/user_uniqueness_detail_decision_v1.md",
    fixed = TRUE
  )

  general_prompt_text <- req$workflow$category$prompt$user
  uniqueness_prompt_text <- req$workflow$subcategory$prompt$user
  expect_match(general_prompt_text, "general name", fixed = TRUE)
  expect_match(general_prompt_text, "vegetation cluster", fixed = TRUE)
  expect_match(uniqueness_prompt_text, "could differ", fixed = TRUE)
  expect_match(uniqueness_prompt_text, "General name:", fixed = TRUE)
  summary_prompt_text <- req$workflow$summary$prompt$user
  expect_match(summary_prompt_text, "Fixed general name:", fixed = TRUE)
  expect_match(summary_prompt_text, "Fixed uniqueness detail:", fixed = TRUE)
  expect_false(grepl("category", general_prompt_text, ignore.case = TRUE))
  expect_false(grepl("subcategory", general_prompt_text, ignore.case = TRUE))
  expect_false(grepl("category", uniqueness_prompt_text, ignore.case = TRUE))
  expect_false(grepl("subcategory", uniqueness_prompt_text, ignore.case = TRUE))
  expect_false(grepl("Cluster:", general_prompt_text, fixed = TRUE))
  expect_false(grepl("Dataset context:", general_prompt_text, fixed = TRUE))
  expect_false(grepl("Draft", general_prompt_text, fixed = TRUE))
  expect_false(grepl("Cluster:", uniqueness_prompt_text, fixed = TRUE))
  expect_false(grepl("Dataset context:", uniqueness_prompt_text, fixed = TRUE))
  expect_false(grepl("Draft", uniqueness_prompt_text, fixed = TRUE))
})

test_that("staged general-name prompt bundle is isolated in v7", {
  ev <- .build_test_cluster_evidence()
  v6_dir <- system.file(
    "prompts",
    "internal_cluster_labeling",
    "v6",
    package = "cocktailr"
  )
  v7_dir <- system.file(
    "prompts",
    "internal_cluster_labeling",
    "v7",
    package = "cocktailr"
  )

  copied_files <- c(
    "user_abstain_reason_pass_v2.md",
    "user_draft_analysis_v1.md",
    "user_general_name_decision_v1.md",
    "user_label_decision_broad_v2.md",
    "user_label_decision_primary_v2.md",
    "user_label_decision_soft_v2.md",
    "user_label_summary_pass_v2.md"
  )
  for (copied_file in copied_files) {
    expect_true(file.exists(file.path(v7_dir, copied_file)))
    expect_equal(
      readLines(file.path(v7_dir, copied_file), warn = FALSE),
      readLines(file.path(v6_dir, copied_file), warn = FALSE)
    )
  }
  expect_true(file.exists(file.path(v7_dir, "user_uniqueness_detail_decision_v1.md")))
  expect_false(identical(
    readLines(file.path(v7_dir, "user_uniqueness_detail_decision_v1.md"), warn = FALSE),
    readLines(file.path(v6_dir, "user_uniqueness_detail_decision_v1.md"), warn = FALSE)
  ))

  req <- llm_label_cluster(
    evidence = ev,
    model = "fake-model",
    internal_prompt_version = "v7",
    use_brainstorm = FALSE,
    use_subcategorization = TRUE,
    dry_run = TRUE
  )

  expect_equal(req$workflow$category$prompt$task_type, "general_name_decision")
  expect_equal(req$workflow$subcategory$prompt$task_type, "uniqueness_detail_decision")
  expect_match(
    req$workflow$category$prompt$user_path,
    "inst/prompts/internal_cluster_labeling/v7/user_general_name_decision_v1.md",
    fixed = TRUE
  )
  expect_match(
    req$workflow$subcategory$prompt$user_path,
    "inst/prompts/internal_cluster_labeling/v7/user_uniqueness_detail_decision_v1.md",
    fixed = TRUE
  )

  uniqueness_prompt_text <- req$workflow$subcategory$prompt$user
  expect_match(uniqueness_prompt_text, "DRY means do not repeat yourself", fixed = TRUE)
  expect_match(uniqueness_prompt_text, "fixed display label", fixed = TRUE)
  expect_match(uniqueness_prompt_text, "fixed general name", fixed = TRUE)
  expect_match(uniqueness_prompt_text, "bakery", ignore.case = TRUE)
  expect_match(uniqueness_prompt_text, "car", ignore.case = TRUE)
  expect_match(uniqueness_prompt_text, "could differ", fixed = TRUE)
  expect_match(uniqueness_prompt_text, "General name:", fixed = TRUE)
  expect_false(grepl("Cluster:", uniqueness_prompt_text, fixed = TRUE))
  expect_false(grepl("Dataset context:", uniqueness_prompt_text, fixed = TRUE))
  expect_false(grepl("Draft", uniqueness_prompt_text, fixed = TRUE))
})

test_that("v6 prompt version keeps the standard flow when subcategorization is off", {
  ev <- .build_test_cluster_evidence()

  req <- llm_label_cluster(
    evidence = ev,
    model = "fake-model",
    internal_prompt_version = "v6",
    use_brainstorm = FALSE,
    use_subcategorization = FALSE,
    dry_run = TRUE
  )

  expect_equal(req$workflow$label$variants[[1]]$prompt$task_type, "label_decision")
  expect_null(req$workflow$subcategorization %||% NULL)
  expect_false("category" %in% names(req$workflow))
  expect_match(
    req$workflow$summary$prompt$user_path,
    "inst/prompts/internal_cluster_labeling/v6/user_label_summary_pass_v2.md",
    fixed = TRUE
  )
})

test_that("llm_label_cluster keeps double brainstorm off by default and guards opt-in scope", {
  ev <- .build_test_cluster_evidence()

  req <- llm_label_cluster(
    evidence = ev,
    model = "gemma4:12b",
    variant = "label_primary_v1",
    internal_prompt_version = "v8a",
    use_brainstorm = FALSE,
    use_subcategorization = TRUE,
    dry_run = TRUE
  )

  expect_null(req$workflow$draft_evidence %||% NULL)
  expect_false(grepl("Draft A: exploratory", req$workflow$subcategory$prompt$user, fixed = TRUE))

  expect_error(
    llm_label_cluster(
      evidence = ev,
      model = "gemma4:12b",
      variant = "label_primary_v1",
      internal_prompt_version = "v9",
      use_brainstorm = FALSE,
      use_subcategorization = TRUE,
      use_double_brainstorm = TRUE,
      dry_run = TRUE
    ),
    "`use_double_brainstorm = TRUE` requires `use_brainstorm = TRUE`.",
    fixed = TRUE
  )
  expect_error(
    llm_label_cluster(
      evidence = ev,
      model = "gemma4:12b",
      variant = "label_primary_v1",
      internal_prompt_version = "v8a",
      use_brainstorm = TRUE,
      use_subcategorization = TRUE,
      use_double_brainstorm = TRUE,
      dry_run = TRUE
    ),
    "`use_double_brainstorm = TRUE` currently requires `internal_prompt_version = \"v9\"`.",
    fixed = TRUE
  )
})

test_that("v9 prompt bundle isolates double brainstorm assets and renders both draft sections", {
  ev <- .build_test_cluster_evidence()
  v8a_uniqueness <- readLines(
    test_path("../../inst/prompts/internal_cluster_labeling/v8a/user_uniqueness_detail_decision_v1.md"),
    warn = FALSE
  )
  v9_uniqueness <- readLines(
    test_path("../../inst/prompts/internal_cluster_labeling/v9/user_uniqueness_detail_decision_v1.md"),
    warn = FALSE
  )

  expect_false(identical(v8a_uniqueness, v9_uniqueness))
  expect_true(any(grepl("Two brainstorm drafts:", v9_uniqueness, fixed = TRUE)))
  expect_false(any(grepl("Two brainstorm drafts:", v8a_uniqueness, fixed = TRUE)))

  req <- llm_label_cluster(
    evidence = ev,
    model = "gemma4:12b",
    variant = "label_primary_v1",
    internal_prompt_version = "v9",
    use_brainstorm = TRUE,
    use_subcategorization = TRUE,
    use_double_brainstorm = TRUE,
    dry_run = TRUE
  )

  expect_equal(req$workflow$draft$prompt$variant, "draft_analysis_exploratory_v1")
  expect_equal(req$workflow$draft_evidence$variant, "draft_analysis_evidence_focused_v1")
  expect_match(
    req$workflow$draft$prompt$user_path,
    "inst/prompts/internal_cluster_labeling/v9/user_draft_analysis_exploratory_v1.md",
    fixed = TRUE
  )
  expect_match(
    req$workflow$draft_evidence$prompt$user_path,
    "inst/prompts/internal_cluster_labeling/v9/user_draft_analysis_evidence_focused_v1.md",
    fixed = TRUE
  )

  downstream_prompts <- c(
    req$workflow$category$prompt$user,
    req$workflow$pre_subcategory_summary$prompt$user,
    req$workflow$subcategory$prompt$user,
    req$workflow$summary$prompt$user
  )
  expect_true(all(grepl("Draft A: exploratory", downstream_prompts, fixed = TRUE)))
  expect_true(all(grepl("Draft B: evidence-focused", downstream_prompts, fixed = TRUE)))
  expect_match(req$workflow$subcategory$prompt$user, "Two brainstorm drafts:", fixed = TRUE)
})

test_that("A001 subgroup-name prompt bundle is isolated with reduced uniqueness inputs", {
  ev <- .build_test_cluster_evidence()
  v8a_dir <- system.file(
    "prompts",
    "internal_cluster_labeling",
    "v8a",
    package = "cocktailr"
  )

  expect_true(dir.exists(v8a_dir))
  expect_true(file.exists(file.path(v8a_dir, "user_uniqueness_detail_decision_v1.md")))

  req <- llm_label_cluster(
    evidence = ev,
    model = "fake-model",
    internal_prompt_version = "v8a",
    use_brainstorm = FALSE,
    use_subcategorization = TRUE,
    dry_run = TRUE
  )

  expect_equal(req$workflow$category$prompt$task_type, "general_name_decision")
  expect_equal(req$workflow$subcategory$prompt$task_type, "uniqueness_detail_decision")
  expect_match(
    req$workflow$subcategory$prompt$user_path,
    "inst/prompts/internal_cluster_labeling/v8a/user_uniqueness_detail_decision_v1.md",
    fixed = TRUE
  )

  uniqueness_prompt_text <- req$workflow$subcategory$prompt$user
  expect_match(uniqueness_prompt_text, "short subgroup name", fixed = TRUE)
  expect_match(uniqueness_prompt_text, "Fixed display label:", fixed = TRUE)
  expect_match(uniqueness_prompt_text, "General name:", fixed = TRUE)
  expect_match(uniqueness_prompt_text, "Label description:", fixed = TRUE)
  expect_false(grepl("another similar", uniqueness_prompt_text, fixed = TRUE))
  expect_false(grepl("Text:", uniqueness_prompt_text, fixed = TRUE))
  expect_false(grepl("Draft", uniqueness_prompt_text, fixed = TRUE))
  expect_false(grepl("Cluster:", uniqueness_prompt_text, fixed = TRUE))
  expect_false(grepl("Dataset context:", uniqueness_prompt_text, fixed = TRUE))
})

test_that("A001 qualifier and subsection prompt bundles are isolated with reduced uniqueness inputs", {
  ev <- .build_test_cluster_evidence()
  cases <- list(
    list(version = "v8b", phrase = "main short qualifier"),
    list(version = "v8c", phrase = "short subsection heading")
  )

  for (case in cases) {
    candidate_dir <- system.file(
      "prompts",
      "internal_cluster_labeling",
      case$version,
      package = "cocktailr"
    )

    expect_true(dir.exists(candidate_dir))
    expect_true(file.exists(file.path(
      candidate_dir,
      "user_uniqueness_detail_decision_v1.md"
    )))

    req <- llm_label_cluster(
      evidence = ev,
      model = "fake-model",
      internal_prompt_version = case$version,
      use_brainstorm = FALSE,
      use_subcategorization = TRUE,
      dry_run = TRUE
    )

    uniqueness_prompt_text <- req$workflow$subcategory$prompt$user
    expect_match(
      req$workflow$subcategory$prompt$user_path,
      paste0(
        "inst/prompts/internal_cluster_labeling/",
        case$version,
        "/user_uniqueness_detail_decision_v1.md"
      ),
      fixed = TRUE
    )
    expect_match(uniqueness_prompt_text, case$phrase, fixed = TRUE)
    expect_match(uniqueness_prompt_text, "Fixed display label:", fixed = TRUE)
    expect_match(uniqueness_prompt_text, "General name:", fixed = TRUE)
    expect_match(uniqueness_prompt_text, "Label description:", fixed = TRUE)
    expect_false(grepl("another similar", uniqueness_prompt_text, fixed = TRUE))
    expect_false(grepl("Text:", uniqueness_prompt_text, fixed = TRUE))
    expect_false(grepl("Draft", uniqueness_prompt_text, fixed = TRUE))
    expect_false(grepl("Cluster:", uniqueness_prompt_text, fixed = TRUE))
    expect_false(grepl("Dataset context:", uniqueness_prompt_text, fixed = TRUE))
  }
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

test_that("llm_label_cluster assembles labeled output and coerces label-only text to the final contract", {
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
  expect_equal(res$output$canonical_label, "compact_species")
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

test_that("llm_label_cluster retries a malformed long label-decision reply and keeps the repaired full label", {
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
  expect_equal(res$output$canonical_label, "compact_species")
  expect_equal(res$output$display_label, "compact species core")
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
