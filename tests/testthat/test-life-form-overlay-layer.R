.build_life_form_overlay_test_cluster_species <- function() {
  tibble::tibble(
    cluster = rep("c_1", 4L),
    species = c(
      "Abies alba",
      "Acacia cultriformis",
      "Acanthus mollis",
      "Unknown species"
    ),
    phi = c(0.91, 0.73, 0.66, 0.21)
  )
}

test_that("life-form overlay dictionary loads from extdata with the expected structure", {
  dict <- cocktailr:::.read_life_form_overlay_dictionary()

  expect_true(is.data.frame(dict))
  expect_equal(
    names(dict),
    c("metric", "lower", "upper", "label", "phrase")
  )
  expect_equal(
    sort(unique(dict$metric)),
    sort(c(
      "dominant_life_form_share",
      "life_form_richness",
      "fixed_assignment_share",
      "mixed_assignment_share",
      "unmatched_species_share",
      "species_to_life_form_compression"
    ))
  )
  expect_true(file.exists(attr(dict, "source_path")))
  expect_equal(attr(dict, "version"), "v1")
})

test_that("life-form glossary dictionary loads the short definitions used by synopsis references", {
  dict <- cocktailr:::.read_life_form_glossary_dictionary()

  expect_true(is.data.frame(dict))
  expect_equal(
    names(dict),
    c("label", "short_definition", "interpretation_hint", "source_name", "source_url")
  )
  expect_true(all(c(
    "Tree", "Shrub", "Chamaephyte", "Epiphyte",
    "Geophyte", "Hemicryptophyte", "Phanerophyte", "Therophyte"
  ) %in% dict$label))
  expect_true(file.exists(attr(dict, "source_path")))
  expect_equal(attr(dict, "version"), "v1")
})

test_that("life-form overlay species and metrics stay deterministic on matched, mixed, and unmatched species", {
  cluster_species <- .build_life_form_overlay_test_cluster_species()
  lookup <- cocktailr:::get_species_life_form_lookup(
    species = cluster_species$species,
    force = TRUE,
    force_reference = TRUE
  )
  dict <- cocktailr:::.read_life_form_dictionary()

  overlay_species <- cocktailr:::.build_cluster_life_form_overlay_species(
    cluster_species = cluster_species,
    lookup = lookup,
    dictionary = dict
  )
  overlay_metrics <- cocktailr:::.interpret_cluster_life_form_overlay_metrics(
    cocktailr:::.summarise_cluster_life_form_overlay_metrics(overlay_species)
  )

  expect_equal(nrow(overlay_species), 4L)

  abies <- overlay_species[overlay_species$species == "Abies alba", , drop = FALSE]
  acacia <- overlay_species[overlay_species$species == "Acacia cultriformis", , drop = FALSE]
  acanthus <- overlay_species[overlay_species$species == "Acanthus mollis", , drop = FALSE]
  unknown <- overlay_species[overlay_species$species == "Unknown species", , drop = FALSE]

  expect_equal(abies$assignment_state[[1]], "mixed")
  expect_equal(abies$matched_labels[[1]], "Tree; Phanerophyte")
  expect_equal(acacia$assignment_state[[1]], "mixed")
  expect_equal(acanthus$assignment_state[[1]], "fixed")
  expect_equal(acanthus$matched_labels[[1]], "Hemicryptophyte")
  expect_equal(unknown$assignment_state[[1]], "unmatched")

  metric_values <- setNames(overlay_metrics$value, overlay_metrics$metric)
  expect_equal(metric_values[["dominant_life_form_share"]], 2 / 3, tolerance = 1e-8)
  expect_equal(metric_values[["life_form_richness"]], 4)
  expect_equal(metric_values[["fixed_assignment_share"]], 0.25, tolerance = 1e-8)
  expect_equal(metric_values[["mixed_assignment_share"]], 0.50, tolerance = 1e-8)
  expect_equal(metric_values[["unmatched_species_share"]], 0.25, tolerance = 1e-8)
  expect_equal(
    metric_values[["species_to_life_form_compression"]],
    1.25,
    tolerance = 1e-8
  )

  expect_equal(
    overlay_metrics$bucket_label[overlay_metrics$metric == "dominant_life_form_share"][[1]],
    "concentrated"
  )
  expect_equal(
    overlay_metrics$bucket_label[overlay_metrics$metric == "species_to_life_form_compression"][[1]],
    "mild_compression"
  )
})

test_that("synopsis life-form collector collapses overlapping woody labels into one structural group", {
  ev <- list(
    summaries = list(
      life_form_overlay_species = data.frame(
        species = c("sp1", "sp2", "sp3", "sp4"),
        assignment_state = c("mixed", "fixed", "mixed", "fixed"),
        life_form_count = c(2L, 1L, 2L, 1L),
        primary_raw_flag = c("tree", "phanerophyte", "hemicryptophyte", "therophyte"),
        matched_labels = c(
          "Tree; Phanerophyte",
          "Phanerophyte",
          "Hemicryptophyte; Therophyte",
          "Therophyte"
        ),
        stringsAsFactors = FALSE
      )
    )
  )

  summary_tbl <- cocktailr:::.cluster_evidence_synopsis_life_form_group_summary(ev)

  expect_equal(summary_tbl$group_label[[1L]], "woody trees and shrubs")
  expect_equal(summary_tbl$count[[1L]], 2L)
  expect_equal(summary_tbl$share[[1L]], 0.5, tolerance = 1e-8)
  expect_equal(summary_tbl$group_label[[2L]], "near-ground perennial herbs")
  expect_equal(summary_tbl$count[[2L]], 1L)
  expect_equal(summary_tbl$group_label[[3L]], "annual herbs")
  expect_equal(summary_tbl$count[[3L]], 1L)
})

test_that("complex life-form overlay appears in prompt output ahead of the coarse summary", {
  vm <- matrix(
    c(
      60, 35, 25, 0,
      55, 30, 20, 0,
      30, 10, 20, 5,
      0, 0, 5, 55,
      0, 0, 0, 50,
      0, 0, 0, 45
    ),
    nrow = 6,
    byrow = TRUE,
    dimnames = list(
      paste0("plot", 1:6),
      c(
        "Abies alba",
        "Acacia cultriformis",
        "Acanthus mollis",
        "Abelmoschus esculentus"
      )
    )
  )

  x <- suppressWarnings(cocktail_cluster(
    vm,
    progress = FALSE,
    plot_values = "rel_cover",
    species_cluster_phi = TRUE,
    save_vegmatrix = TRUE
  ))

  ev <- cluster_evidence(
    x,
    cluster = "c_1",
    top_n_phi = 10,
    n_prototype_plots = 2,
    n_borderline_plots = 2
  )
  overlay_result <- cocktailr:::score_cluster_life_form_overlay(
    x = x,
    clusters = "c_1"
  )
  ev <- cocktailr:::.augment_cluster_evidence_with_life_form_overlay_layer(
    evidence = ev,
    life_form_overlay_result = overlay_result
  )

  llm_text <- cocktailr:::.format_cluster_evidence_llm_prompt(ev)
  review_text <- cocktailr:::.format_cluster_evidence_review_prompt(ev)

  expect_match(llm_text, "Species-first plant life-form overlay:", fixed = TRUE)
  expect_match(llm_text, "Life-form structure metrics:", fixed = TRUE)
  expect_match(llm_text, "Life-form overlay diagnosis:", fixed = TRUE)
  expect_match(llm_text, "Plant life-form context for the cluster:", fixed = TRUE)
  expect_lt(
    regexpr("Species-first plant life-form overlay:", llm_text, fixed = TRUE)[[1]],
    regexpr("Plant life-form context for the cluster:", llm_text, fixed = TRUE)[[1]]
  )

  expect_match(review_text, "Species-first life-form overlay:", fixed = TRUE)
  expect_match(review_text, "Life-form structure metrics:", fixed = TRUE)
  expect_match(review_text, "Life-form overlay diagnosis:", fixed = TRUE)
  expect_match(review_text, "Life-form evidence: ", fixed = TRUE)
})
