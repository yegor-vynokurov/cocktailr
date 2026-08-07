.build_life_form_test_cluster_species <- function() {
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

test_that("life-form dictionary loads from extdata with the expected structure", {
  dict <- cocktailr:::.read_life_form_dictionary()

  expect_true(is.data.frame(dict))
  expect_equal(
    names(dict),
    c("raw_flag", "label", "phrase", "priority")
  )
  expect_true(all(c("tree", "shrub", "hemicryptophyte", "therophyte") %in% dict$raw_flag))
  expect_true(all(is.finite(dict$priority)))
  expect_true(file.exists(attr(dict, "source_path")))
  expect_equal(attr(dict, "version"), "v1")
})

test_that("life-form workbook parser resolves taxon names and packaged flag columns", {
  dict <- cocktailr:::.read_life_form_dictionary()
  table <- cocktailr:::.read_life_form_workbook_table(
    path = cocktailr:::.life_form_workbook_path(),
    dictionary = dict
  )

  expect_true(is.data.frame(table))
  expect_true(all(c("reference_name", "taxon_key", "binomial_key") %in% names(table)))
  expect_true(all(unique(dict$raw_flag) %in% names(table)))
  expect_true("Abies alba" %in% table$reference_name)

  abies <- table[table$reference_name == "Abies alba", , drop = FALSE]
  expect_equal(nrow(abies), 1L)
  expect_equal(abies$tree[[1]], 1L)
  expect_equal(abies$phanerophyte[[1]], 1L)
  expect_equal(abies$therophyte[[1]], 0L)
})

test_that("life-form lookup and summary keep matched and unmatched species deterministic in sibling cache", {
  cluster_species <- .build_life_form_test_cluster_species()
  root <- cocktailr_project_root()
  paths <- cocktailr:::.life_form_layer_paths(root)

  if (file.exists(paths$species_cache)) {
    unlink(paths$species_cache, force = TRUE)
  }

  lookup <- cocktailr:::get_species_life_form_lookup(
    species = cluster_species$species,
    root = root,
    force = TRUE,
    force_reference = TRUE
  )
  dict <- cocktailr:::.read_life_form_dictionary()
  species_evidence <- cocktailr:::.build_species_life_form_evidence(
    cluster_species = cluster_species,
    lookup = lookup,
    dictionary = dict
  )
  summary <- cocktailr:::.summarise_cluster_life_forms(species_evidence)

  expect_true(file.exists(paths$reference_cache))
  expect_true(file.exists(paths$species_cache))
  expect_true(grepl("cache/life_form_layer$", gsub("\\\\", "/", paths$cache_root)))

  expect_equal(
    lookup$match_method[match("Unknown species", lookup$input_species)],
    "unmatched"
  )
  expect_true(all(c("Tree", "Shrub", "Hemicryptophyte", "Phanerophyte") %in% summary$label))

  tree_row <- summary[summary$label == "Tree", , drop = FALSE]
  shrub_row <- summary[summary$label == "Shrub", , drop = FALSE]

  expect_equal(tree_row$matched_species_count[[1]], 1L)
  expect_match(tree_row$matched_species[[1]], "Abies alba", fixed = TRUE)
  expect_equal(shrub_row$matched_species_count[[1]], 1L)
  expect_match(shrub_row$matched_species[[1]], "Acacia cultriformis", fixed = TRUE)
})
