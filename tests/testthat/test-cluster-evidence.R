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
  expect_true(is.list(ev$context$quantity_context))
  expect_equal(ev$context$quantity_context$dataset_species_total, 4L)
  expect_equal(ev$context$quantity_context$dataset_plot_total, 6L)
  expect_equal(ev$context$quantity_context$dataset_cluster_total, 3L)
  expect_equal(
    ev$context$quantity_context$cluster_species_count,
    ev$context$cluster_metrics$k
  )
  expect_equal(
    ev$context$quantity_context$cluster_plot_count,
    ev$summaries$plots_membership$n_member_plots
  )
  expect_equal(
    ev$context$quantity_context$cover_scale_type,
    "percentage_cover"
  )
  expect_equal(
    ev$context$quantity_context$cover_scale_label,
    "original percentage-cover scale"
  )
  expect_equal(
    unname(ev$context$quantity_context$cover_scale_bounds),
    c(0, 100)
  )
  expect_true(all(c(
    "species_freq_count", "species_freq_pct", "n_member_plots",
    "mean_plot_cover_share_pct", "cover_scale_type", "cover_scale_label",
    "cover_scale_min", "cover_scale_max"
  ) %in% names(ev$summaries$cover_summary)))
  expect_true(all(
    ev$summaries$cover_summary$species_freq_count >= 0L
  ))
  expect_equal(
    ev$summaries$cover_summary$species_freq_pct,
    100 * ev$summaries$cover_summary$species_freq_count /
      ev$summaries$cover_summary$n_member_plots
  )
  expect_true(all(is.finite(ev$summaries$cover_summary$mean_plot_cover_share_pct)))

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
  expect_equal(
    ev$context$quantity_context$cover_scale_type,
    "numeric_unknown_scale"
  )
})

test_that("cluster_evidence keeps debug evidence IDs out of the LLM serializer only", {
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

  review_text <- cocktailr:::.format_cluster_evidence_review_prompt(ev)
  llm_text <- cocktailr:::.format_cluster_evidence_llm_prompt(ev)

  expect_match(review_text, "[E", fixed = TRUE)
  expect_false(grepl("\\[E[0-9]+\\]", llm_text))
  expect_match(
    llm_text,
    "Plants that regularly occur in this cluster:",
    fixed = TRUE
  )
  expect_match(
    llm_text,
    "Species with the strongest cluster association:",
    fixed = TRUE
  )
  expect_match(
    llm_text,
    "Dataset context:",
    fixed = TRUE
  )
  expect_match(
    llm_text,
    "How to read these cluster metrics:",
    fixed = TRUE
  )
  expect_match(
    llm_text,
    "h=",
    fixed = TRUE
  )
  expect_false(grepl("Plot membership: n=", llm_text, fixed = TRUE))
  expect_match(
    llm_text,
    "the cluster core contains",
    fixed = TRUE
  )
  expect_match(
    llm_text,
    "member plots out of",
    fixed = TRUE
  )
  expect_match(
    llm_text,
    "a plot must contain at least",
    fixed = TRUE
  )
  expect_match(
    llm_text,
    "/100 on the original percentage-cover scale",
    fixed = TRUE
  )
  expect_match(
    llm_text,
    "occurs in ",
    fixed = TRUE
  )
  expect_false(grepl("mean_cover=", llm_text, fixed = TRUE))
  expect_false(grepl("Cover summary:", llm_text, fixed = TRUE))
})

test_that("cluster_evidence strips evidence IDs from model-facing user-added lines", {
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

  ev$user_added_data <- list(
    entries = list(
      list(
        name = "note.txt",
        format = "txt",
        text = "Field note [E777]\nSecondary line [E12]",
        truncated = FALSE
      )
    )
  )
  ev$meta$user_added_data_present <- TRUE

  review_text <- cocktailr:::.format_cluster_evidence_review_prompt(ev)
  llm_text <- cocktailr:::.format_cluster_evidence_llm_prompt(ev)

  expect_match(review_text, "Field note [E777]", fixed = TRUE)
  expect_match(review_text, "Secondary line [E12]", fixed = TRUE)
  expect_false(grepl("\\[E[0-9]+\\]", llm_text))
  expect_match(llm_text, "Field note", fixed = TRUE)
  expect_match(llm_text, "Secondary line", fixed = TRUE)
})

test_that("cluster_evidence renders cluster metrics with fixed literal definitions", {
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

  llm_text <- cocktailr:::.format_cluster_evidence_llm_prompt(ev)
  llm_lines <- strsplit(llm_text, "\n", fixed = TRUE)[[1]]
  k_line <- llm_lines[grepl("^- k=", llm_lines)]
  n_line <- llm_lines[grepl("^- n=", llm_lines)]

  expect_match(
    llm_text,
    "How to read these cluster metrics:",
    fixed = TRUE
  )
  expect_match(
    llm_text,
    paste0(
      "h=",
      formatC(ev$context$cluster_metrics$h, digits = 3L, format = "f"),
      ": merge-phi value for this cluster."
    ),
    fixed = TRUE
  )
  expect_match(
    llm_text,
    paste0(
      "k=",
      ev$context$cluster_metrics$k,
      ": the cluster core contains ",
      ev$context$cluster_metrics$k,
      " species out of ",
      ev$context$quantity_context$dataset_species_total,
      " recorded species in the dataset"
    ),
    fixed = TRUE
  )
  expect_match(
    llm_text,
    paste0(
      "m=",
      ev$context$cluster_metrics$m,
      ": a plot must contain at least ",
      ev$context$cluster_metrics$m,
      " of these ",
      ev$context$cluster_metrics$k,
      " species to count as a member of the cluster."
    ),
    fixed = TRUE
  )
  expect_match(
    llm_text,
    paste0(
      "n=",
      ev$summaries$plots_membership$n_member_plots,
      ": this cluster contains ",
      ev$summaries$plots_membership$n_member_plots,
      " member plots out of ",
      ev$context$quantity_context$dataset_plot_total,
      " plots in the dataset"
    ),
    fixed = TRUE
  )
  expect_length(k_line, 1L)
  expect_length(n_line, 1L)
  expect_false(grepl("member plots", k_line, fixed = TRUE))
  expect_false(grepl("species out of", n_line, fixed = TRUE))
})

test_that("cluster_evidence applies a deterministic LLM species retain policy", {
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

  n_member_plots <- ev$summaries$plots_membership$n_member_plots
  freq_count <- c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L)
  freq_pct <- 100 * freq_count / n_member_plots

  ev$summaries$cover_summary <- data.frame(
    species = paste0("sp", 1:8),
    mean_cover = c(10, 9, 8, 7, 6, 5, 4, 3),
    median_cover = c(10, 9, 8, 7, 6, 5, 4, 3),
    freq_in_member_plots = freq_count / n_member_plots,
    species_freq_count = freq_count,
    species_freq_pct = freq_pct,
    mean_plot_cover_share_pct = c(22, 20, 18, 16, 14, 12, 10, 8),
    n_member_plots = rep(n_member_plots, 8L),
    cover_scale_type = rep("percentage_cover", 8L),
    cover_scale_label = rep("original percentage-cover scale", 8L),
    cover_scale_min = rep(0, 8L),
    cover_scale_max = rep(100, 8L),
    stringsAsFactors = FALSE
  )
  ev$summaries$species_phi <- data.frame(
    species = c("sp4", "sp2", "sp9", "sp10"),
    phi = c(0.9, 0.8, 0.7, 0.6),
    evidence_id = NA_character_,
    stringsAsFactors = FALSE
  )

  selected <- cocktailr:::.cluster_evidence_llm_selected_species_info(ev)
  phi_items <- cocktailr:::.cluster_evidence_llm_phi_items(
    ev,
    exclude_species = selected$species
  )
  llm_text <- cocktailr:::.format_cluster_evidence_llm_prompt(ev)

  expect_identical(
    selected$species,
    c("sp4", "sp2", "sp1", "sp3", "sp5", "sp6", "sp7", "sp8")
  )
  expect_identical(
    selected$selected_via,
    c(
      "phi", "phi",
      "dominant_cover", "dominant_cover",
      "dominant_cover", "dominant_cover",
      "frequent_cover", "frequent_cover"
    )
  )
  expect_identical(
    phi_items,
    c("sp9 (phi=0.700)", "sp10 (phi=0.600)")
  )

  species_positions <- vapply(
    paste0(selected$species, ": occurs in "),
    function(pattern) regexpr(pattern, llm_text, fixed = TRUE)[[1]],
    integer(1)
  )
  expect_true(all(species_positions > 0L))
  expect_true(all(diff(species_positions) > 0L))
  expect_match(
    llm_text,
    "Species with the strongest cluster association: sp9 (phi=0.700); sp10 (phi=0.600)",
    fixed = TRUE
  )
  expect_false(grepl("Species with the strongest cluster association: sp4", llm_text, fixed = TRUE))
  expect_false(grepl("Species with the strongest cluster association: sp2", llm_text, fixed = TRUE))
})

test_that("cluster_evidence prompt-visible species cap filters species-bearing prompt blocks consistently", {
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

  ev$summaries$cover_summary <- data.frame(
    species = paste0("sp", 1:8),
    mean_cover = c(22, 20, 18, 16, 14, 12, 10, 8),
    species_freq_count = c(9, 8, 7, 6, 5, 4, 3, 2),
    species_freq_pct = c(90, 80, 70, 60, 50, 40, 30, 20),
    n_member_plots = rep(10L, 8L),
    stringsAsFactors = FALSE
  )
  ev$summaries$species_phi <- data.frame(
    species = c("sp4", "sp2", "sp9", "sp10"),
    phi = c(0.9, 0.8, 0.7, 0.6),
    evidence_id = NA_character_,
    stringsAsFactors = FALSE
  )
  ev$summaries$species_axis_values <- data.frame(
    species = paste0("sp", 1:8),
    l = seq(1, 8),
    m = seq(1, 8),
    n = seq(1, 8),
    r = seq(1, 8),
    t = seq(1, 8),
    s = seq(1, 8),
    stringsAsFactors = FALSE
  )
  ev$summaries$life_form_overlay_species <- data.frame(
    species = paste0("sp", 1:8),
    phi = seq(0.8, 0.1, length.out = 8L),
    phi_rank = seq_len(8L),
    assignment_state = rep("fixed", 8L),
    matched_labels = rep("Tree", 8L),
    stringsAsFactors = FALSE
  )
  ev$summaries$life_form_summary <- data.frame(
    raw_flag = "tree",
    label = "Tree",
    phrase = "tree-form species are present among the matched cluster plants",
    priority = 1L,
    matched_species_count = 4L,
    matched_species = "sp1, sp5, sp6, sp9",
    stringsAsFactors = FALSE
  )
  ev$summaries$semantic_unmatched_species <- c("sp1", "sp6", "sp9")

  old_opt <- options(cocktailr.prompt_visible_species_cap = 5L)
  on.exit(options(old_opt), add = TRUE)

  selected <- cocktailr:::.cluster_evidence_llm_selected_species_info(ev)
  rendered <- cocktailr:::.serialize_cluster_evidence_llm_prompt(ev)
  llm_text <- rendered$text

  expect_identical(selected$species, c("sp4", "sp2", "sp1", "sp3", "sp5"))
  expect_identical(rendered$prompt_visible_species_cap, 5L)
  expect_match(llm_text, "sp5: occurs in ", fixed = TRUE)
  expect_false(grepl("sp6: occurs in ", llm_text, fixed = TRUE))
  expect_match(llm_text, "sp5 (phi=0.40): fixed assignment -> Tree.", fixed = TRUE)
  expect_false(grepl("sp6 (phi=", llm_text, fixed = TRUE))
  expect_match(llm_text, "Matched cluster species: sp1, sp5.", fixed = TRUE)
  expect_false(grepl("Matched cluster species: sp1, sp5, sp6", llm_text, fixed = TRUE))
  expect_match(llm_text, "Species without ecological-axis values: sp1", fixed = TRUE)
  expect_false(grepl("sp9", llm_text, fixed = TRUE))
})

test_that("cluster_evidence prompt-visible species cap can expand beyond the default species-core limit", {
  vm <- matrix(
    c(
      40, 30,  0,  0,
      35, 25,  0,  0,
      20, 10, 25,  0,
       0,  0, 45, 30,
       0,  0, 35, 25,
       0,  0, 25, 15
    ),
    nrow = 6,
    byrow = TRUE,
    dimnames = list(
      paste0("plot", seq_len(6)),
      paste0("sp", seq_len(4))
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
    top_n_phi = 6,
    n_prototype_plots = 2,
    n_borderline_plots = 2
  )

  ev$summaries$cover_summary <- data.frame(
    species = paste0("sp", seq_len(16)),
    mean_cover = seq(16, 1),
    species_freq_count = seq(16, 1),
    species_freq_pct = seq(80, 5, length.out = 16L),
    n_member_plots = rep(10L, 16L),
    stringsAsFactors = FALSE
  )
  ev$summaries$species_phi <- data.frame(
    species = paste0("sp", seq_len(8)),
    phi = seq(0.9, 0.2, length.out = 8L),
    evidence_id = NA_character_,
    stringsAsFactors = FALSE
  )

  default_selected <- cocktailr:::.cluster_evidence_llm_selected_species_info(ev)
  expect_identical(nrow(default_selected), 12L)

  old_opt <- options(cocktailr.prompt_visible_species_cap = 17L)
  on.exit(options(old_opt), add = TRUE)

  expanded_selected <- cocktailr:::.cluster_evidence_llm_selected_species_info(ev)
  expect_identical(nrow(expanded_selected), 16L)
  expect_true(setequal(expanded_selected$species, paste0("sp", seq_len(16))))
})

test_that("cluster_evidence LLM serializer is deterministic across repeated runs", {
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

  ev$summaries$species_axis_values <- data.frame(
    species = c("sp1", "sp2"),
    l = c(8.10, 7.20),
    m = c(2.25, 3.10),
    n = c(2.25, 4.40),
    r = c(7.04, 6.42),
    t = c(5.23, 4.22),
    s = c(NA_real_, 0.11),
    stringsAsFactors = FALSE
  )
  ev$summaries$semantic_axes <- data.frame(
    axis_name = c("Light", "Moisture", "Temperature"),
    axis = c("l", "m", "t"),
    score_0_10 = c(8.04, 2.76, 4.95),
    stringsAsFactors = FALSE
  )
  ev$summaries$semantic_unmatched_species <- c("sp9", "sp10")

  first <- cocktailr:::.serialize_cluster_evidence_llm_prompt(
    ev,
    max_chars = 3000L
  )
  second <- cocktailr:::.serialize_cluster_evidence_llm_prompt(
    ev,
    max_chars = 3000L
  )

  expect_identical(first$text, second$text)
  expect_identical(first$full_text, second$full_text)
  expect_identical(first$kept_block_ids, second$kept_block_ids)
  expect_identical(first$dropped_block_ids, second$dropped_block_ids)
  expect_identical(first$truncated_block_ids, second$truncated_block_ids)
  expect_identical(first$blocks$status, second$blocks$status)
  expect_identical(first$blocks$item_count_used, second$blocks$item_count_used)
})

test_that("cluster_evidence uses plain ecological headings in the LLM prompt", {
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

  ev$summaries$semantic_axes <- data.frame(
    axis_name = c("Light", "Moisture"),
    axis = c("l", "m"),
    score_0_10 = c(8.04, 2.76),
    band = c("very_bright", "dry"),
    coverage = c(0.92, 0.88),
    confidence_tier = c("high", "high"),
    stringsAsFactors = FALSE
  )
  ev$summaries$semantic_unmatched_species <- c("sp9")

  review_text <- cocktailr:::.format_cluster_evidence_review_prompt(ev)
  llm_text <- cocktailr:::.format_cluster_evidence_llm_prompt(ev)

  expect_match(review_text, "Semantic axes:", fixed = TRUE)
  expect_match(llm_text, "Ecological axis summary for the cluster:", fixed = TRUE)
  expect_match(
    llm_text,
    "- Light 8.04/10 (prefers very bright places).",
    fixed = TRUE
  )
  expect_match(
    llm_text,
    "- Moisture 2.76/10 (prefers dry conditions).",
    fixed = TRUE
  )
  expect_match(
    llm_text,
    "Species without ecological-axis values: sp9",
    fixed = TRUE
  )
  expect_false(grepl("Semantic axes:", llm_text, fixed = TRUE))
  expect_false(grepl("Semantic indicator profile", llm_text, fixed = TRUE))
  expect_false(grepl("Semantic unmatched species:", llm_text, fixed = TRUE))
  expect_false(grepl("band=", llm_text, fixed = TRUE))
  expect_false(grepl("coverage=", llm_text, fixed = TRUE))
  expect_false(grepl("confidence=", llm_text, fixed = TRUE))
})

test_that("cluster_evidence synopsis prompt uses interpretive references and only local life-form definitions", {
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

  ev$summaries$semantic_axes <- data.frame(
    axis_name = c("Light", "Moisture", "Temperature"),
    axis = c("l", "m", "t"),
    score_0_10 = c(8.04, 2.76, 4.95),
    stringsAsFactors = FALSE
  )
  ev$summaries$species_axis_values <- data.frame(
    species = c("sp1", "sp2", "sp3"),
    l = c(8.10, 7.20, 4.10),
    m = c(2.25, 3.10, 5.20),
    n = c(2.25, 4.40, 5.40),
    r = c(7.04, 6.42, 5.50),
    t = c(5.23, 4.22, 4.40),
    s = c(NA_real_, 0.11, 0.05),
    stringsAsFactors = FALSE
  )
  ev$summaries$life_form_overlay_species <- data.frame(
    species = c("sp1", "sp2", "sp3"),
    phi = c(0.80, 0.60, 0.40),
    phi_rank = 1:3,
    assignment_state = c("fixed", "mixed", "fixed"),
    matched_labels = c("Hemicryptophyte", "Hemicryptophyte; Therophyte", "Therophyte"),
    stringsAsFactors = FALSE
  )
  ev$summaries$life_form_overlay_metrics <- data.frame(
    cluster = rep("c_1", 4L),
    metric = c(
      "fixed_assignment_share",
      "mixed_assignment_share",
      "unmatched_species_share",
      "species_to_life_form_compression"
    ),
    metric_label = c(
      "Fixed-assignment share",
      "Mixed-assignment share",
      "Unmatched-species share",
      "Species-to-life-form compression"
    ),
    value = c(2 / 3, 1 / 3, 0, 2),
    value_text = c("66.7%", "33.3%", "0%", "2.00"),
    bucket_label = c("mostly_fixed", "present_mixing", "near_complete", "strong_compression"),
    bucket_phrase = c(
      "most selected species resolve to a single life form",
      "mixed life-form assignments are present but not dominant",
      "workbook coverage is nearly complete for the selected species core",
      "many species collapse into a smaller number of repeated life-form buckets"
    ),
    stringsAsFactors = FALSE
  )
  ev$summaries$life_form_overlay_diagnosis <- data.frame(
    cluster = "c_1",
    diagnosis_code = "assignment_clarity",
    label = "Assignment clarity",
    phrase = "mixed life-form assignments are present but not dominant",
    driver_metrics = "fixed_assignment_share; mixed_assignment_share",
    stringsAsFactors = FALSE
  )

  rendered <- cocktailr:::.serialize_cluster_evidence_synopsis_prompt(
    ev,
    represented_species = 3L
  )
  text <- rendered$text

  expect_match(text, "Reference:", fixed = TRUE)
  expect_match(text, "structural signal:", fixed = TRUE)
  expect_match(text, "h: cluster cohesion signal on a 0-1 scale", fixed = TRUE)
  expect_match(
    text,
    paste0(
      "k: cluster-core species count; read it as `k=current/",
      ev$context$quantity_context$dataset_species_total,
      " species`"
    ),
    fixed = TRUE
  )
  expect_match(text, "dataset share:", fixed = TRUE)
  expect_match(text, "species rank = hybrid:", fixed = TRUE)
  expect_match(
    text,
    paste0(
      "cluster metrics: h=",
      formatC(ev$context$cluster_metrics$h, digits = 3L, format = "f"),
      "/1.0, k=",
      ev$context$cluster_metrics$k,
      "/",
      ev$context$quantity_context$dataset_species_total,
      " species, m=",
      ev$context$cluster_metrics$m,
      "/",
      ev$context$cluster_metrics$k,
      " core species, n=",
      ev$context$quantity_context$cluster_plot_count,
      "/",
      ev$context$quantity_context$dataset_plot_total,
      " plots"
    ),
    fixed = TRUE
  )
  expect_match(text, "life-form summary: a supporting plain-language structural summary", fixed = TRUE)
  expect_match(text, "Dominant structure: near-ground perennial herbs", fixed = TRUE)
  expect_match(text, "Secondary structure: annual herbs", fixed = TRUE)
  expect_match(
    text,
    "Assignment clarity: 66.7% of the selected species resolve to one structural type, while 33.3% still have multiple possible structural assignments.",
    fixed = TRUE
  )
  expect_match(
    text,
    "Compression caution: many species collapse into a smaller set of repeated structural groups, so keep ecology and species names primary.",
    fixed = TRUE
  )
  expect_false(grepl("Hemicryptophyte", text, fixed = TRUE))
  expect_false(grepl("Therophyte", text, fixed = TRUE))
  expect_false(grepl("life_form=", text, fixed = TRUE))
})

test_that("cluster_evidence synopsis prompt keeps life-form definitions off semantic-only prompts", {
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

  ev$summaries$semantic_axes <- data.frame(
    axis_name = c("Light", "Moisture", "Temperature"),
    axis = c("l", "m", "t"),
    score_0_10 = c(8.04, 2.76, 4.95),
    stringsAsFactors = FALSE
  )
  ev$summaries$species_axis_values <- data.frame(
    species = c("sp1", "sp2", "sp3"),
    l = c(8.10, 7.20, 4.10),
    m = c(2.25, 3.10, 5.20),
    n = c(2.25, 4.40, 5.40),
    r = c(7.04, 6.42, 5.50),
    t = c(5.23, 4.22, 4.40),
    s = c(NA_real_, 0.11, 0.05),
    stringsAsFactors = FALSE
  )

  semantic_only <- cocktailr:::.serialize_cluster_evidence_synopsis_prompt(
    ev,
    represented_species = 3L
  )$text

  ev$summaries$life_form_overlay_species <- data.frame(
    species = c("sp1", "sp2", "sp3"),
    phi = c(0.80, 0.60, 0.40),
    phi_rank = 1:3,
    assignment_state = c("fixed", "mixed", "fixed"),
    matched_labels = c("Hemicryptophyte", "Hemicryptophyte; Therophyte", "Therophyte"),
    stringsAsFactors = FALSE
  )
  ev$summaries$life_form_overlay_metrics <- data.frame(
    cluster = rep("c_1", 4L),
    metric = c(
      "fixed_assignment_share",
      "mixed_assignment_share",
      "unmatched_species_share",
      "species_to_life_form_compression"
    ),
    metric_label = c(
      "Fixed-assignment share",
      "Mixed-assignment share",
      "Unmatched-species share",
      "Species-to-life-form compression"
    ),
    value = c(2 / 3, 1 / 3, 0, 2),
    value_text = c("66.7%", "33.3%", "0%", "2.00"),
    bucket_label = c("mostly_fixed", "present_mixing", "near_complete", "strong_compression"),
    bucket_phrase = c(
      "most selected species resolve to a single life form",
      "mixed life-form assignments are present but not dominant",
      "workbook coverage is nearly complete for the selected species core",
      "many species collapse into a smaller number of repeated life-form buckets"
    ),
    stringsAsFactors = FALSE
  )
  ev$summaries$life_form_overlay_diagnosis <- data.frame(
    cluster = "c_1",
    diagnosis_code = "assignment_clarity",
    label = "Assignment clarity",
    phrase = "mixed life-form assignments are present but not dominant",
    driver_metrics = "fixed_assignment_share; mixed_assignment_share",
    stringsAsFactors = FALSE
  )

  semantic_plus_complex <- cocktailr:::.serialize_cluster_evidence_synopsis_prompt(
    ev,
    represented_species = 3L
  )$text

  expect_false(grepl("Hemicryptophyte", semantic_only, fixed = TRUE))
  expect_false(grepl("Therophyte", semantic_only, fixed = TRUE))
  expect_false(grepl("Life-form summary:", semantic_only, fixed = TRUE))
  expect_match(semantic_plus_complex, "Dominant structure: near-ground perennial herbs", fixed = TRUE)
  expect_match(semantic_plus_complex, "Secondary structure: annual herbs", fixed = TRUE)
  expect_false(grepl("Hemicryptophyte", semantic_plus_complex, fixed = TRUE))
  expect_false(grepl("Therophyte", semantic_plus_complex, fixed = TRUE))
  expect_false(grepl("life_form=", semantic_plus_complex, fixed = TRUE))
})

test_that("cluster_evidence renders literal life-form headings separately from semantic evidence", {
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

  ev$summaries$life_form_summary <- data.frame(
    raw_flag = c("tree", "shrub"),
    label = c("Tree", "Shrub"),
    phrase = c(
      "tree-form species are present among the matched cluster plants",
      "shrub-form species are present among the matched cluster plants"
    ),
    priority = c(10L, 20L),
    matched_species_count = c(1L, 2L),
    matched_species = c("Abies alba", "Acacia cultriformis, Ligustrum vulgare"),
    evidence_id = c("E10", "E11"),
    stringsAsFactors = FALSE
  )
  ev$summaries$life_form_unmatched_species <- c("sp9")

  review_text <- cocktailr:::.format_cluster_evidence_review_prompt(ev)
  llm_text <- cocktailr:::.format_cluster_evidence_llm_prompt(ev)

  expect_match(review_text, "Life-form evidence:", fixed = TRUE)
  expect_match(review_text, "Life-form unmatched species: sp9", fixed = TRUE)
  expect_match(llm_text, "Plant life-form context for the cluster:", fixed = TRUE)
  expect_match(
    llm_text,
    "- Tree: tree-form species are present among the matched cluster plants. Matched cluster species: Abies alba.",
    fixed = TRUE
  )
  expect_match(
    llm_text,
    "- Shrub: shrub-form species are present among the matched cluster plants. Matched cluster species: Acacia cultriformis, Ligustrum vulgare.",
    fixed = TRUE
  )
  expect_false(grepl("Life-form unmatched species:", llm_text, fixed = TRUE))
})

test_that("cluster_evidence uses normalized cover context for non-percentage scales", {
  vm <- matrix(
    c(
      6, 4, 0, 0,
      5, 3, 0, 0,
      2, 1, 3, 0,
      0, 0, 6, 4,
      0, 0, 5, 3,
      0, 0, 4, 2
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

  llm_text <- cocktailr:::.format_cluster_evidence_llm_prompt(ev)

  expect_equal(
    ev$context$quantity_context$cover_scale_type,
    "ordinal_numeric_cover"
  )
  expect_true(all(is.finite(ev$summaries$cover_summary$mean_plot_cover_share_pct)))
  expect_match(
    llm_text,
    "Mean share of total plot cover",
    fixed = TRUE
  )
  expect_false(grepl("/100 on the original percentage-cover scale", llm_text, fixed = TRUE))
  expect_false(grepl("mean_cover=", llm_text, fixed = TRUE))
})

test_that("cluster_evidence renders species axis phrases on enriched species lines and omits missing axes", {
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

  ev$summaries$species_axis_values <- data.frame(
    species = c("sp1"),
    l = 7.82,
    m = 2.25,
    n = 2.25,
    r = 7.04,
    t = 5.23,
    s = NA_real_,
    stringsAsFactors = FALSE
  )

  llm_text <- cocktailr:::.format_cluster_evidence_llm_prompt(ev)

  expect_match(llm_text, "sp1: occurs in ", fixed = TRUE)
  expect_match(
    llm_text,
    "Light 7.82/10 (prefers bright places).",
    fixed = TRUE
  )
  expect_match(
    llm_text,
    "Moisture 2.25/10 (prefers dry conditions).",
    fixed = TRUE
  )
  expect_match(
    llm_text,
    "Nutrients 2.25/10 (prefers nutrient-poor soils).",
    fixed = TRUE
  )
  expect_match(
    llm_text,
    "Reaction 7.04/10 (prefers base-rich soils).",
    fixed = TRUE
  )
  expect_match(
    llm_text,
    "Temperature 5.23/10 (typical of temperate conditions).",
    fixed = TRUE
  )
  expect_false(grepl("Salinity", llm_text, fixed = TRUE))
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
