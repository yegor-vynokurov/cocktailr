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
  expect_match(art$markdown, "## Evidence-backed claims", fixed = TRUE)
  expect_match(art$markdown, "## Key species", fixed = TRUE)
  expect_match(art$markdown, "## What is not confirmed", fixed = TRUE)
  expect_match(art$markdown, "## Confidence", fixed = TRUE)
  expect_false(grepl("## Review summary", art$markdown, fixed = TRUE))
  expect_false(grepl("## External knowledge used", art$markdown, fixed = TRUE))
  expect_false(grepl("## Validation warnings", art$markdown, fixed = TRUE))
  expect_false(grepl("## Checks to run", art$markdown, fixed = TRUE))
  expect_false(grepl("## Evidence snapshot", art$markdown, fixed = TRUE))
  expect_false(grepl("## Manual review notes", art$markdown, fixed = TRUE))
})

test_that("render_cluster_review full=TRUE builds an expanded markdown artifact", {
  ev <- .build_review_test_cluster_evidence()
  output <- .build_review_label_output(ev)

  art <- render_cluster_review(output, ev, full = TRUE)

  expect_match(art$markdown, "^---", perl = TRUE)
  expect_match(art$markdown, "cluster_id: \"c_1\"", fixed = TRUE)
  expect_match(art$markdown, "## Review summary", fixed = TRUE)
  expect_match(art$markdown, "## External knowledge used", fixed = TRUE)
  expect_match(art$markdown, "## Validation warnings", fixed = TRUE)
  expect_match(art$markdown, "## Checks to run", fixed = TRUE)
  expect_match(art$markdown, "## Evidence snapshot", fixed = TRUE)
  expect_match(art$markdown, "## Manual review notes", fixed = TRUE)
})

test_that("render_cluster_review writes compact markdown by default and metadata only in full mode", {
  ev <- .build_review_test_cluster_evidence()
  output <- .build_review_label_output(ev)

  res <- list(
    cluster_id = ev$meta$cluster_id,
    provider = "ollama",
    model = "fake-model",
    variant = "strict_abstention_gate_v1",
    workflow_steps = 1L,
    logs = list(run_dir = "temp/fake-run"),
    prompt = list(
      catalog_path = "inst/prompts/cluster_labeling/catalog.json",
      system_path = "inst/prompts/cluster_labeling/system_v1.md",
      user_path = "inst/prompts/cluster_labeling/user_strict_abstention_gate_v1.md",
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
    "User prompt: `inst/prompts/cluster_labeling/user_strict_abstention_gate_v1.md`",
    fixed = TRUE
  )

  art_full <- render_cluster_review(res, ev, file = out_file_full, full = TRUE)

  expect_true(file.exists(art_full$file))
  expect_true(file.exists(art_full$metadata_file))

  meta <- jsonlite::fromJSON(art_full$metadata_file, simplifyVector = TRUE)
  expect_equal(meta$cluster_id, "c_1")
  expect_equal(meta$model, "fake-model")
  expect_equal(meta$variant, "strict_abstention_gate_v1")
  expect_equal(meta$review_status, "accepted")
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
  output$canonical_label <- "dry_grassland_cluster"
  output$display_label <- "dry grassland cluster"
  output$interpretation_summary <- "The cluster looks like a dry grassland assemblage based on its compact species core."

  art <- render_cluster_review(output, ev, full = TRUE)

  expect_equal(art$metadata$review_status, "review_required")
  expect_true(art$metadata$needs_human_review)
  expect_match(art$markdown, "unsupported_habitat_overreach", fixed = TRUE)
  expect_match(art$markdown, "Review status: `review_required`", fixed = TRUE)
})
