.build_review_test_cluster_evidence <- function() {
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

.build_review_test_cluster_evidence_with_dataset <- function() {
  syn <- generate_synthetic_vegetation_data(
    n_plots_per_community = 4,
    n_transition_plots = 2,
    seed = 42
  )

  x <- suppressWarnings(cocktail_cluster(
    syn$wide_matrix,
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

.build_review_label_output <- function(ev) {
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
    label_summary = "A compact recurring species core supports a short compositional label.",
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
    abstain_reason = NULL,
    explanation = "The short compositional label is safe because the same species core recurs across the evidence bundle."
  )
}

test_that("render_cluster_review builds a readable compact markdown artifact by default", {
  ev <- .build_review_test_cluster_evidence()
  output <- .build_review_label_output(ev)

  art <- render_cluster_review(output, ev)

  expect_s3_class(art, "cluster_review_artifact")
  expect_equal(art$metadata$review_status, "accepted")
  expect_false(art$metadata$needs_human_review)
  expect_false(startsWith(art$markdown, "---"))
  expect_match(art$markdown, "## Dataset", fixed = TRUE)
  expect_match(art$markdown, "## Generation setup", fixed = TRUE)
  expect_match(art$markdown, "Model: not recorded.", fixed = TRUE)
  expect_match(art$markdown, "Prompt files: not recorded.", fixed = TRUE)
  expect_match(art$markdown, "## Proposed label", fixed = TRUE)
  expect_match(art$markdown, "- Label summary: ", fixed = TRUE)
  expect_match(art$markdown, "## Evidence-backed claims", fixed = TRUE)
  expect_match(art$markdown, "## Key species", fixed = TRUE)
  expect_match(art$markdown, "## What is not confirmed", fixed = TRUE)
  expect_match(art$markdown, "## Confidence", fixed = TRUE)
  expect_false(grepl("## Final explanation", art$markdown, fixed = TRUE))
  expect_false(grepl("## Interpretation summary", art$markdown, fixed = TRUE))
  expect_false(grepl("## Review summary", art$markdown, fixed = TRUE))
  expect_false(grepl("## External knowledge used", art$markdown, fixed = TRUE))
  expect_false(grepl("## Validation warnings", art$markdown, fixed = TRUE))
  expect_false(grepl("## Checks to run", art$markdown, fixed = TRUE))
  expect_false(grepl("## Evidence snapshot", art$markdown, fixed = TRUE))
  expect_false(grepl("## Manual review notes", art$markdown, fixed = TRUE))
})

test_that("render_cluster_review shows full display label and separate plot preview when they differ", {
  ev <- .build_review_test_cluster_evidence()
  output <- .build_review_label_output(ev)
  output$display_label <- "dry base-rich shrubland with ligustrum and oak canopy"
  output$canonical_label <- "dry_base_rich_shrubland"

  art <- render_cluster_review(output, ev)

  expect_match(
    art$markdown,
    "- Display label: `dry base-rich shrubland with ligustrum and oak canopy`",
    fixed = TRUE
  )
  expect_match(
    art$markdown,
    "- Plot preview label: `dry base-rich shrubland ...`",
    fixed = TRUE
  )
})

test_that("render_cluster_review full=TRUE builds an expanded markdown artifact", {
  ev <- .build_review_test_cluster_evidence()
  ev$summaries$life_form_summary <- data.frame(
    raw_flag = "tree",
    label = "Tree",
    phrase = "tree-form species are present among the matched cluster plants",
    priority = 10L,
    matched_species_count = 1L,
    matched_species = "Abies alba",
    evidence_id = "E10",
    stringsAsFactors = FALSE
  )
  ev$summaries$life_form_unmatched_species <- "Unknown plant"
  ev$meta$source$has_life_form_layer <- TRUE
  ev$summaries$life_form_overlay_species <- data.frame(
    species = "Abies alba",
    phi = 0.91,
    assignment_state = "mixed",
    matched_labels = "Tree; Phanerophyte",
    stringsAsFactors = FALSE
  )
  ev$summaries$life_form_overlay_metrics <- data.frame(
    metric = "dominant_life_form_share",
    metric_label = "Dominant life-form share",
    value = 0.67,
    value_text = "66.7%",
    bucket_label = "concentrated",
    bucket_phrase = "one life form dominates a clear majority of the matched species core",
    stringsAsFactors = FALSE
  )
  ev$summaries$life_form_overlay_diagnosis <- data.frame(
    label = "Structure balance",
    phrase = "Dominance cue: one life form dominates a clear majority of the matched species core",
    stringsAsFactors = FALSE
  )
  ev$meta$enrichment_layers$life_form_layer <- list(
    enabled = TRUE,
    mode = "complex",
    status = "enriched",
    error = NA_character_
  )
  output <- .build_review_label_output(ev)

  art <- render_cluster_review(output, ev, full = TRUE)

  expect_match(art$markdown, "^---", perl = TRUE)
  expect_match(art$markdown, "cluster_id: \"c_1\"", fixed = TRUE)
  expect_match(art$markdown, "## Review summary", fixed = TRUE)
  expect_match(art$markdown, "## Life-form context", fixed = TRUE)
  expect_match(art$markdown, "- Layer mode: `complex`", fixed = TRUE)
  expect_match(art$markdown, "Species-first overlay rows: `1`", fixed = TRUE)
  expect_match(art$markdown, "Abies alba (phi=0.91): mixed assignment; Tree; Phanerophyte", fixed = TRUE)
  expect_match(art$markdown, "Metric Dominant life-form share: 66.7% (concentrated); one life form dominates a clear majority of the matched species core", fixed = TRUE)
  expect_match(art$markdown, "Diagnosis Structure balance: Dominance cue: one life form dominates a clear majority of the matched species core", fixed = TRUE)
  expect_match(art$markdown, "Tree: tree-form species are present among the matched cluster plants; matched species: Abies alba", fixed = TRUE)
  expect_match(art$markdown, "Unmatched life-form species: Unknown plant", fixed = TRUE)
  expect_match(art$markdown, "- Label summary: ", fixed = TRUE)
  expect_match(art$markdown, "## External knowledge used", fixed = TRUE)
  expect_match(art$markdown, "## Validation warnings", fixed = TRUE)
  expect_match(art$markdown, "## Checks to run", fixed = TRUE)
  expect_match(art$markdown, "## Evidence snapshot", fixed = TRUE)
  expect_match(art$markdown, "Topological species:", fixed = TRUE)
  expect_match(art$markdown, "[E", fixed = TRUE)
  expect_match(art$markdown, "## Manual review notes", fixed = TRUE)
  expect_false(grepl("## Final explanation", art$markdown, fixed = TRUE))
  expect_false(grepl("## Interpretation summary", art$markdown, fixed = TRUE))
})

test_that("render_cluster_review writes compact markdown by default and metadata only in full mode", {
  ev <- .build_review_test_cluster_evidence()
  output <- .build_review_label_output(ev)

  res <- list(
    cluster_id = ev$meta$cluster_id,
    provider = "ollama",
    model = "fake-model",
    variant = "label_primary_v1",
    workflow_steps = 1L,
    logs = list(run_dir = "temp/fake-run"),
    prompt = list(
      catalog_path = "inst/prompts/cluster_labeling/catalog.json",
      system_path = "inst/prompts/cluster_labeling/system_scientific_caution_v1.md",
      user_path = "inst/prompts/cluster_labeling/user_label_primary_v1.md",
      schema_path = "inst/schemas/cluster_label_output_schema.json"
    ),
    output = output
  )
  class(res) <- c("cluster_label_result", "list")

  out_dir <- file.path(tempdir(), "cocktailr-review-artifact")
  unlink(out_dir, recursive = TRUE, force = TRUE)
  out_file <- file.path(out_dir, "compact", "c_1_review.md")
  out_file_full <- file.path(out_dir, "full", "c_1_review.md")

  art <- render_cluster_review(res, ev, file = out_file)

  expect_true(file.exists(art$file))
  expect_null(art$metadata_file)
  expect_match(art$markdown, "Model: `fake-model`", fixed = TRUE)
  expect_match(
    art$markdown,
    "User prompt: `inst/prompts/cluster_labeling/user_label_primary_v1.md`",
    fixed = TRUE
  )

  art_full <- render_cluster_review(res, ev, file = out_file_full, full = TRUE)

  expect_true(file.exists(art_full$file))
  expect_true(file.exists(art_full$metadata_file))

  meta <- jsonlite::fromJSON(art_full$metadata_file, simplifyVector = TRUE)
  expect_equal(meta$cluster_id, "c_1")
  expect_equal(meta$model, "fake-model")
  expect_equal(meta$variant, "label_primary_v1")
  expect_equal(meta$review_status, "accepted")
})

test_that("render_cluster_review surfaces exhausted staged-fallback provenance", {
  ev <- .build_review_test_cluster_evidence()
  selection_output <- cocktailr:::.cluster_label_selection_all_abstain_output(
    evidence = ev,
    abstain_reasons = c("The signal remains too mixed for a stable short label."),
    failure_messages = c(
      "label_primary_v1: abstained",
      "label_soft_v1: abstained",
      "label_broad_v1: abstained"
    )
  )
  output <- cocktailr:::.assemble_cluster_label_final_output(
    evidence = ev,
    selection_output = selection_output,
    explanation_text = paste(
      "The evidence remains mixed across the available prototype plots,",
      "so the selection ladder abstained instead of forcing a short label."
    )
  )

  res <- list(
    cluster_id = ev$meta$cluster_id,
    provider = "ollama",
    model = "fake-model",
    variant = "label_primary_v1",
    workflow_steps = 3L,
    logs = list(run_dir = "temp/fake-run"),
    prompt = list(
      catalog_path = "inst/prompts/cluster_labeling/catalog.json",
      system_path = "inst/prompts/cluster_labeling/system_scientific_caution_v1.md",
      user_path = "inst/prompts/internal_cluster_labeling/v1/user_explanation_pass_v1.md",
      schema_path = "inst/schemas/cluster_label_output_schema.json"
    ),
    output = output,
    workflow = list(
      draft = list(
        output = list(
          draft_analysis = paste(
            "Possible interpretations:",
            "- mixed meadow assemblage",
            "",
            "Main signal:",
            "- recurring but mixed meadow signal",
            "",
            "Noise or conflicts:",
            "- no clean narrow habitat split",
            "",
            "Candidate labels:",
            "- mixed meadow assemblage",
            "",
            "What not to overclaim:",
            "- narrow habitat naming",
            sep = "\n"
          )
        ),
        skipped = FALSE
      ),
      label = list(
        selected_public_variant = "selection_all_abstain",
        exhausted = TRUE,
        selection_output = selection_output,
        failure_messages = c(
          "label_primary_v1: abstained",
          "label_soft_v1: abstained",
          "label_broad_v1: abstained"
        )
      )
    )
  )
  class(res) <- c("cluster_label_result", "list")

  art <- render_cluster_review(res, ev, full = TRUE)

  expect_true(isTRUE(art$metadata$label_stage_exhausted))
  expect_equal(art$metadata$selected_label_variant, "selection_all_abstain")
  expect_equal(art$metadata$public_display_label, "Chaotic Cluster")
  expect_match(art$markdown, "Selected label rung: `selection_all_abstain`", fixed = TRUE)
  expect_match(art$markdown, "Label-stage exhausted: `true`", fixed = TRUE)
  expect_match(art$markdown, "Programmatic public fallback display label: `Chaotic Cluster`", fixed = TRUE)
  expect_match(art$markdown, "Model output: no stable short label was produced.", fixed = TRUE)
  expect_match(art$markdown, "## Brainstorm trace", fixed = TRUE)
  expect_match(art$markdown, "Candidate label: `mixed meadow assemblage` -> `mixed_meadow_assemblage`", fixed = TRUE)
  expect_false(grepl("## Final explanation", art$markdown, fixed = TRUE))
  expect_false(grepl("## Interpretation summary", art$markdown, fixed = TRUE))
})

test_that("render_cluster_review full=TRUE shows the text-only workflow trace for labeled output", {
  ev <- .build_review_test_cluster_evidence()
  output <- .build_review_label_output(ev)
  label_decision_text <- "sp1-sp2 cluster"
  summary_text <- "A compact recurring species core supports a short compositional label."

  res <- list(
    cluster_id = ev$meta$cluster_id,
    provider = "ollama",
    model = "fake-model",
    variant = "label_primary_v1",
    workflow_steps = 3L,
    logs = list(run_dir = "temp/fake-run"),
    prompt = list(
      catalog_path = "inst/prompts/cluster_labeling/catalog.json",
      system_path = "inst/prompts/cluster_labeling/system_scientific_caution_v1.md",
      user_path = "inst/prompts/internal_cluster_labeling/v1/user_explanation_pass_v1.md",
      schema_path = NULL
    ),
    output = output,
    workflow = list(
      draft = list(
        output = list(
          draft_analysis = paste(
            "Possible interpretations:",
            "- compact species core",
            "",
            "Main signal:",
            "- recurring compositional core",
            sep = "\n"
          )
        ),
        skipped = FALSE
      ),
      label = list(
        selected_public_variant = "label_primary_v1",
        exhausted = FALSE,
        selection_output = list(
          status = "labeled",
          label_decision_text = "sp1-sp2 cluster",
          canonical_label = "sp1_sp2_cluster",
          display_label = "sp1-sp2 cluster",
          label_summary = "A compact recurring species core supports a short compositional label.",
          abstain_reason = NULL
        ),
        attempts = list(
          list(
            public_variant = "label_primary_v1",
            selection_variant = "label_decision_primary_v2",
            result = "labeled",
            attempts = 1L,
            response = list(content = label_decision_text),
            output = list(
              status = "labeled",
              label_decision_text = "sp1-sp2 cluster",
              canonical_label = "sp1_sp2_cluster",
              display_label = "sp1-sp2 cluster",
              abstain_reason = NULL
            )
          )
        )
      ),
      summary = list(
        variant = "label_summary_pass_v2",
        skipped = FALSE,
        prompt = list(
          user = paste(
            "Task mode: `label_summary_pass_v2`",
            "Chosen short label (fixed; do not replace it):",
            "sp1-sp2 cluster",
            sep = "\n"
          ),
          evidence_text = ""
        ),
        response = list(content = summary_text),
        output = list(
          status = "summary_ready",
          label_summary = summary_text
        )
      ),
      abstain_reason = list(
        variant = "abstain_reason_pass_v2",
        skipped = TRUE,
        skip_reason = "label_selected"
      )
    )
  )
  class(res) <- c("cluster_label_result", "list")

  art <- render_cluster_review(res, ev, full = TRUE)

  expect_match(art$markdown, "## Workflow trace", fixed = TRUE)
  expect_match(art$markdown, "### `label_primary_v1`", fixed = TRUE)
  expect_match(art$markdown, "#### Raw label-only answer", fixed = TRUE)
  expect_match(art$markdown, "sp1-sp2 cluster", fixed = TRUE)
  expect_match(art$markdown, "#### Parsed label decision", fixed = TRUE)
  expect_match(art$markdown, "- Label decision text: `sp1-sp2 cluster`", fixed = TRUE)
  expect_match(art$markdown, "- Canonical label: `sp1_sp2_cluster`", fixed = TRUE)
  expect_match(art$markdown, "### `label_summary_pass_v2`", fixed = TRUE)
  expect_match(art$markdown, "- Raw cluster evidence re-read: `false`", fixed = TRUE)
  expect_match(art$markdown, "- Summary input scope: `label + brainstorm only`", fixed = TRUE)
  expect_match(art$markdown, "#### Raw summary answer", fixed = TRUE)
  expect_match(art$markdown, summary_text, fixed = TRUE)
})

test_that("render_cluster_review full=TRUE distinguishes model abstain from downstream public fallback", {
  ev <- .build_review_test_cluster_evidence()
  selection_output <- cocktailr:::.cluster_label_selection_all_abstain_output(
    evidence = ev,
    abstain_reasons = c("ABSTAIN"),
    failure_messages = c(
      "label_primary_v1: abstained (ABSTAIN)",
      "label_soft_v1: abstained (ABSTAIN)",
      "label_broad_v1: abstained (ABSTAIN)"
    )
  )
  output <- cocktailr:::.assemble_cluster_label_final_output(
    evidence = ev,
    selection_output = selection_output,
    abstain_reason_text = "The evidence remains too mixed for a stable short label.",
    explanation_text = "The evidence remains too mixed for a stable short label."
  )

  res <- list(
    cluster_id = ev$meta$cluster_id,
    provider = "ollama",
    model = "fake-model",
    variant = "label_primary_v1",
    workflow_steps = 3L,
    logs = list(run_dir = "temp/fake-run"),
    prompt = list(
      catalog_path = "inst/prompts/cluster_labeling/catalog.json",
      system_path = "inst/prompts/cluster_labeling/system_scientific_caution_v1.md",
      user_path = "inst/prompts/internal_cluster_labeling/v1/user_abstain_reason_pass_v2.md",
      schema_path = NULL
    ),
    output = output,
    workflow = list(
      label = list(
        selected_public_variant = "selection_all_abstain",
        exhausted = TRUE,
        selection_output = selection_output,
        attempts = list(
          list(
            public_variant = "label_primary_v1",
            selection_variant = "label_decision_primary_v2",
            result = "abstain",
            attempts = 1L,
            response = list(content = "ABSTAIN"),
            output = list(
              status = "abstain",
              label_decision_text = "ABSTAIN",
              canonical_label = NULL,
              display_label = NULL
            )
          )
        )
      ),
      abstain_reason = list(
        variant = "abstain_reason_pass_v2",
        skipped = FALSE,
        response = list(
          content = "The evidence remains too mixed for a stable short label."
        ),
        output = list(
          status = "abstain_reason_ready",
          abstain_reason = "The evidence remains too mixed for a stable short label."
        )
      )
    )
  )
  class(res) <- c("cluster_label_result", "list")

  art <- render_cluster_review(res, ev, full = TRUE)

  expect_match(art$markdown, "## Workflow trace", fixed = TRUE)
  expect_match(art$markdown, "#### Raw label-only answer", fixed = TRUE)
  expect_match(art$markdown, "ABSTAIN", fixed = TRUE)
  expect_match(
    art$markdown,
    "This is the model's abstain decision at the label-only step",
    fixed = TRUE
  )
  expect_match(art$markdown, "#### Raw abstain-reason answer", fixed = TRUE)
  expect_match(
    art$markdown,
    "does not create the downstream public fallback label",
    fixed = TRUE
  )
  expect_match(art$markdown, "### Model abstain vs public fallback", fixed = TRUE)
})

test_that("render_cluster_review review_dir organizes files by dataset when known", {
  ev <- .build_review_test_cluster_evidence_with_dataset()
  output <- .build_review_label_output(ev)

  out_dir <- file.path(tempdir(), "cocktailr-review-by-dataset")
  unlink(out_dir, recursive = TRUE, force = TRUE)

  art <- render_cluster_review(
    output,
    ev,
    review_dir = out_dir
  )

  expect_true(file.exists(art$file))
  expect_null(art$metadata_file)
  expect_equal(basename(dirname(art$file)), ev$meta$dataset$label)
  expect_match(basename(art$file), "^c_1_review", perl = TRUE)
})

test_that("render_cluster_review resolves relative review_dir against the package source root", {
  ev <- .build_review_test_cluster_evidence_with_dataset()
  output <- .build_review_label_output(ev)

  pkg_root <- normalizePath(
    getNamespaceInfo(asNamespace("cocktailr"), "path"),
    winslash = "/",
    mustWork = TRUE
  )
  rel_dir <- file.path("temp", "testthat_relative_review_root")
  expected_root <- normalizePath(
    file.path(pkg_root, rel_dir),
    winslash = "/",
    mustWork = FALSE
  )

  unlink(expected_root, recursive = TRUE, force = TRUE)
  old_wd <- setwd(tempdir())
  on.exit(setwd(old_wd), add = TRUE)
  on.exit(unlink(expected_root, recursive = TRUE, force = TRUE), add = TRUE)

  art <- render_cluster_review(
    output,
    ev,
    review_dir = rel_dir
  )

  expect_true(file.exists(art$file))
  expect_true(startsWith(art$file, expected_root))
})

test_that("render_cluster_review visibly flags risky outputs", {
  ev <- .build_review_test_cluster_evidence()
  output <- .build_review_label_output(ev)
  output$external_knowledge <- list(
    list(
      statement = "The data show this cluster is a wet meadow.",
      knowledge_type = "habitat_hint",
      confidence = "medium"
    )
  )

  art <- render_cluster_review(output, ev, full = TRUE)

  expect_equal(art$metadata$review_status, "review_required")
  expect_true(art$metadata$needs_human_review)
  expect_match(art$markdown, "external_knowledge_poses_as_data", fixed = TRUE)
  expect_match(art$markdown, "Review status: `review_required`", fixed = TRUE)
})

test_that("render_cluster_review makes speculative fallback cards visibly distinct", {
  ev <- .build_review_test_cluster_evidence()
  output <- .build_review_label_output(ev)
  output$confidence$score <- 0
  output$confidence$rationale <- "Tentative only; the cluster has direction but not enough stability."
  output$not_confirmed_by_data <- list(
    list(
      statement = "A habitat-level label is not confirmed.",
      reason = "The evidence bundle does not contain enough contrast against neighboring clusters."
    )
  )

  validation <- validate_cluster_label(output, ev)
  validation <- cocktailr:::.mark_speculative_validation(
    validation,
    strict_validation_status = "unsupported_claims"
  )

  art <- render_cluster_review(output, ev, validation = validation, full = TRUE)

  expect_equal(art$metadata$review_status, "speculative")
  expect_true(art$metadata$is_speculative)
  expect_equal(art$metadata$label_tier, "speculative")
  expect_match(art$markdown, "Label tier: `speculative`", fixed = TRUE)
  expect_match(art$markdown, "Display label: `sp1-sp2 cluster\\*`")
  expect_match(art$markdown, "## Why tentative", fixed = TRUE)
  expect_match(art$markdown, "What prevents full confidence", fixed = TRUE)
  expect_match(art$markdown, "Strict outcome before fallback", fixed = TRUE)
})

