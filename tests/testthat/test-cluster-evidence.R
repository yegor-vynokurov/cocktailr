test_that("cluster_evidence returns structured evidence for one cluster", {
  vm <- matrix(
    c(
      55, 40,  0,  0,
      50, 35,  0,  0,
      25, 10, 30,  0,
       0,  0, 60, 45,
       0,  0, 50, 35,
       0,  0, 40, 25
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

  ev <- cluster_evidence(
    x,
    cluster = "c_1",
    top_n_phi = 3,
    n_prototype_plots = 2,
    n_borderline_plots = 2
  )

  expect_s3_class(ev, "cluster_evidence")
  expect_equal(
    names(ev),
    c("meta", "context", "summaries", "evidence", "limitations", "future")
  )
  expect_equal(ev$meta$cluster_id, "c_1")
  expect_true(is.numeric(ev$context$cluster_metrics$h))
  expect_true(is.data.frame(ev$summaries$species_topological))
  expect_true(nrow(ev$summaries$species_topological) >= 1L)
  expect_true(is.data.frame(ev$summaries$species_phi))
  expect_true(is.list(ev$summaries$plots_membership))
  expect_true(is.data.frame(ev$summaries$plots_prototype))
  expect_true(is.data.frame(ev$summaries$plots_borderline))
  expect_true(is.data.frame(ev$summaries$cover_summary))

  ids <- names(ev$evidence$items)
  expect_true(length(ids) > 0L)
  expect_equal(ids, paste0("E", seq_along(ids)))
  expect_true(all(c(
    "cluster_metrics", "topology", "species_topological", "species_phi",
    "plots_membership", "plots_prototype", "plots_borderline",
    "cover_summary", "limitations"
  ) %in% names(ev$evidence$index)))

  printed <- paste(capture_output(print(ev)), collapse = "\n")
  expect_match(printed, "Cluster evidence for c_1")
})

test_that("cluster_evidence handles missing phi and vegmatrix predictably", {
  vm <- matrix(
    c(
      55, 40,  0,  0,
      50, 35,  0,  0,
      25, 10, 30,  0,
       0,  0, 60, 45,
       0,  0, 50, 35,
       0,  0, 40, 25
    ),
    nrow = 6,
    byrow = TRUE,
    dimnames = list(
      paste0("plot", 1:6),
      paste0("sp", 1:4)
    )
  )

  x <- cocktail_cluster(
    vm,
    progress = FALSE,
    plot_values = "binary",
    species_cluster_phi = FALSE,
    save_vegmatrix = FALSE
  )

  ev <- cluster_evidence(x, cluster = 1)

  expect_null(ev$summaries$species_phi)
  expect_null(ev$summaries$cover_summary)
  expect_true(all(c("Species.cluster.phi", "vegmatrix") %in% ev$limitations$missing_components))
  expect_true(length(ev$limitations$warnings) >= 2L)
  expect_true(is.data.frame(ev$summaries$plots_prototype))
})

test_that("cluster_evidence rejects invalid cluster IDs", {
  vm <- matrix(
    c(
      1, 0, 0,
      1, 1, 0,
      0, 1, 1,
      0, 0, 1
    ),
    nrow = 4,
    byrow = TRUE,
    dimnames = list(
      paste0("plot", 1:4),
      paste0("sp", 1:3)
    )
  )

  x <- cocktail_cluster(vm, progress = FALSE)

  expect_error(
    cluster_evidence(x, cluster = "c_999"),
    "does not refer to a valid node"
  )
})
