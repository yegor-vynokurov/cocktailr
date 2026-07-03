test_that("LLM axis dictionary loads from extdata with the expected structure", {
  dict <- cocktailr:::.read_cluster_evidence_llm_axis_dictionary()

  expect_true(is.data.frame(dict))
  expect_equal(
    names(dict),
    c("axis", "lower", "upper", "label", "phrase")
  )
  expect_equal(
    sort(unique(dict$axis)),
    c("l", "m", "n", "r", "s", "t")
  )
  expect_true(all(table(dict$axis) == 10L))
  expect_true(file.exists(attr(dict, "source_path")))
  expect_equal(attr(dict, "version"), "v1")
})

test_that("LLM axis dictionary lookup uses lower inclusive and upper exclusive bins", {
  dict <- cocktailr:::.read_cluster_evidence_llm_axis_dictionary()

  exact_boundary <- cocktailr:::.cluster_evidence_llm_axis_dictionary_lookup(
    axis = "light",
    value = 8.0,
    dictionary = dict
  )
  just_below <- cocktailr:::.cluster_evidence_llm_axis_dictionary_lookup(
    axis = "l",
    value = 7.99,
    dictionary = dict
  )

  expect_equal(exact_boundary$label[[1L]], "very_bright")
  expect_equal(
    exact_boundary$phrase[[1L]],
    "prefers very bright places"
  )
  expect_equal(just_below$label[[1L]], "bright")
  expect_equal(
    just_below$phrase[[1L]],
    "prefers bright places"
  )
})

test_that("LLM axis dictionary includes the final 10.0 endpoint in the last bin", {
  dict <- cocktailr:::.read_cluster_evidence_llm_axis_dictionary()

  match <- cocktailr:::.cluster_evidence_llm_axis_dictionary_lookup(
    axis = "salinity",
    value = 10.0,
    dictionary = dict
  )
  phrase <- cocktailr:::.cluster_evidence_llm_axis_phrase(
    axis = "s",
    value = 10.0,
    dictionary = dict
  )

  expect_equal(match$label[[1L]], "extreme_salinity")
  expect_equal(match$phrase[[1L]], "tolerates extreme salinity")
  expect_equal(phrase, "tolerates extreme salinity")
})

test_that("packaged v1 axis dictionary uses 10 one-unit bins for every 0-10 axis", {
  dict <- cocktailr:::.read_cluster_evidence_llm_axis_dictionary()
  axis_groups <- split(dict, dict$axis, drop = TRUE)

  expect_equal(nrow(dict), 60L)

  for (axis_code in names(axis_groups)) {
    axis_table <- axis_groups[[axis_code]]
    axis_table <- axis_table[order(axis_table$lower, axis_table$upper), , drop = FALSE]

    expect_equal(axis_table$lower, 0:9)
    expect_equal(axis_table$upper, 1:10)
  }
})

test_that("LLM prompt renderer can use an edited axis dictionary via option override", {
  dict <- cocktailr:::.read_cluster_evidence_llm_axis_dictionary()
  tmp <- file.path(tempdir(), "cocktailr_llm_axis_dictionary_override.csv")
  dict$phrase[dict$axis == "l" & dict$lower == 8 & dict$upper == 9] <- "prefers custom bright test wording"
  utils::write.csv(dict, tmp, row.names = FALSE, quote = TRUE)

  old_opt <- options(cocktailr.llm_axis_dictionary_path = tmp)
  on.exit(options(old_opt), add = TRUE)
  on.exit(unlink(tmp, force = TRUE), add = TRUE)

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
    species = "sp1",
    l = 8.10,
    m = NA_real_,
    n = NA_real_,
    r = NA_real_,
    t = NA_real_,
    s = NA_real_,
    stringsAsFactors = FALSE
  )
  ev$summaries$semantic_axes <- data.frame(
    axis_name = "Light",
    axis = "l",
    score_0_10 = 8.04,
    stringsAsFactors = FALSE
  )

  llm_text <- cocktailr:::.format_cluster_evidence_llm_prompt(ev)

  expect_match(
    llm_text,
    "Light 8.10/10 (prefers custom bright test wording).",
    fixed = TRUE
  )
  expect_match(
    llm_text,
    "- Light 8.04/10 (prefers custom bright test wording).",
    fixed = TRUE
  )
})
