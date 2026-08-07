.life_form_workbook_override_path <- function() {
  path <- getOption("cocktailr.life_form_workbook_path", NULL)
  if (is.null(path)) {
    return(NULL)
  }

  .arg_scalar_character(
    path,
    'getOption("cocktailr.life_form_workbook_path")'
  )
}

.life_form_workbook_path <- function(
    root = cocktailr_project_root(),
    path = NULL
) {
  if (is.null(path)) {
    path <- .life_form_workbook_override_path()
  }

  if (!is.null(path)) {
    path <- .resolve_cocktailr_output_path(path)
    if (!file.exists(path)) {
      stop("Life-form workbook file does not exist: ", path, call. = FALSE)
    }

    return(normalizePath(path, winslash = "/", mustWork = TRUE))
  }

  candidate <- file.path(
    root,
    "data-raw",
    "external",
    "life_forms",
    "Life_form.xlsx"
  )

  normalizePath(candidate, winslash = "/", mustWork = FALSE)
}

.life_form_layer_paths <- function(
    root = cocktailr_project_root()
) {
  cache_root <- file.path(root, "cache", "life_form_layer")

  paths <- list(
    root = root,
    workbook_file = .life_form_workbook_path(root = root),
    alias_file = file.path(
      root,
      "data-raw",
      "external",
      "species_aliases.csv"
    ),
    cache_root = cache_root,
    reference_dir = file.path(cache_root, "reference"),
    species_dir = file.path(cache_root, "species"),
    results_dir = file.path(cache_root, "results"),
    reference_cache = file.path(
      cache_root,
      "reference",
      "life_form_reference.rds"
    ),
    species_cache = file.path(
      cache_root,
      "species",
      "species_life_form_lookup.rds"
    )
  )

  dir.create(paths$reference_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$species_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$results_dir, recursive = TRUE, showWarnings = FALSE)

  paths
}

.check_life_form_source_files <- function(paths) {
  missing <- paths$workbook_file[!file.exists(paths$workbook_file)]

  if (length(missing) > 0L) {
    stop(
      "External life-form files were not found:\n",
      paste0("  - ", missing, collapse = "\n"),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.life_form_source_signature <- function(paths) {
  hashes <- tools::md5sum(paths$workbook_file)

  paste(
    names(hashes),
    unname(hashes),
    sep = "=",
    collapse = "|"
  )
}

.life_form_empty_reference_table <- function(flag_names) {
  out <- data.frame(
    reference_name = character(0),
    taxon_key = character(0),
    binomial_key = character(0),
    stringsAsFactors = FALSE
  )

  for (flag_name in flag_names) {
    out[[flag_name]] <- integer(0)
  }

  out
}

.read_life_form_workbook_table <- function(
    path,
    dictionary = NULL,
    sheet = NULL,
    skip = 0L
) {
  .require_semantic_packages()

  dictionary <- dictionary %||% .read_life_form_dictionary()
  sheets <- .semantic_excel_sheets(path)
  chosen_sheet <- sheet %||% sheets[[1L]]
  chosen_skip <- max(0L, as.integer(skip %||% 0L))

  raw <- .semantic_read_excel(
    path,
    sheet = chosen_sheet,
    skip = chosen_skip,
    .name_repair = "minimal"
  )
  raw <- .trim_excel_table(raw, chosen_sheet, chosen_skip)
  names(raw) <- .clean_names_simple(names(raw))

  taxon_col <- .pick_column(
    names(raw),
    patterns = c(
      "^floraveg_taxon$",
      "^flora_veg_taxon$",
      "^taxon$",
      "^species$"
    ),
    label = "life-form taxon"
  )

  flag_names <- unique(dictionary$raw_flag)
  missing_flags <- setdiff(flag_names, names(raw))
  if (length(missing_flags)) {
    stop(
      "Life-form workbook is missing columns required by the packaged dictionary: ",
      paste(missing_flags, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!nrow(raw)) {
    return(.life_form_empty_reference_table(flag_names))
  }

  out <- tibble::tibble(
    reference_name = as.character(raw[[taxon_col]]),
    taxon_key = .taxon_key(raw[[taxon_col]])
  )

  for (flag_name in flag_names) {
    values <- .as_numeric_safe(raw[[flag_name]])
    out[[flag_name]] <- as.integer(is.finite(values) & values > 0)
  }

  out <- out |>
    dplyr::filter(nzchar(.data$taxon_key)) |>
    dplyr::group_by(.data$taxon_key) |>
    dplyr::summarise(
      reference_name = .first_non_missing(.data$reference_name),
      dplyr::across(
        dplyr::all_of(flag_names),
        function(x) {
          as.integer(any(as.integer(x %||% 0L) > 0L, na.rm = TRUE))
        }
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      binomial_key = .binomial_key(.data$reference_name)
    ) |>
    dplyr::arrange(.data$reference_name) |>
    dplyr::select(
      .data$reference_name,
      .data$taxon_key,
      .data$binomial_key,
      dplyr::all_of(flag_names)
    )

  out <- as.data.frame(out, stringsAsFactors = FALSE)
  attr(out, "source_sheet") <- chosen_sheet
  attr(out, "source_skip") <- chosen_skip
  out
}

.life_form_summary_table <- function() {
  data.frame(
    cluster = character(0),
    raw_flag = character(0),
    label = character(0),
    phrase = character(0),
    priority = integer(0),
    matched_species_count = integer(0),
    matched_species = character(0),
    evidence_id = character(0),
    stringsAsFactors = FALSE
  )
}

.life_form_species_evidence_table <- function() {
  data.frame(
    cluster = character(0),
    species = character(0),
    phi = numeric(0),
    raw_flag = character(0),
    label = character(0),
    phrase = character(0),
    priority = integer(0),
    match_method = character(0),
    matched_reference_name = character(0),
    stringsAsFactors = FALSE
  )
}

build_life_form_reference <- function(
    root = cocktailr_project_root(),
    force = FALSE,
    dictionary_path = NULL,
    workbook_path = NULL,
    sheet = NULL,
    skip = 0L
) {
  .require_semantic_packages()

  paths <- .life_form_layer_paths(root)
  dictionary <- .read_life_form_dictionary(path = dictionary_path)
  if (!is.null(workbook_path)) {
    paths$workbook_file <- .life_form_workbook_path(root = root, path = workbook_path)
  }
  .check_life_form_source_files(paths)

  signature <- .life_form_source_signature(paths)

  if (!force && file.exists(paths$reference_cache)) {
    cached <- readRDS(paths$reference_cache)

    if (
      is.list(cached) &&
      identical(cached$source_signature, signature)
    ) {
      return(cached)
    }
  }

  reference <- .read_life_form_workbook_table(
    path = paths$workbook_file,
    dictionary = dictionary,
    sheet = sheet,
    skip = skip
  )

  result <- list(
    source_signature = signature,
    created_at = Sys.time(),
    data = reference,
    source_rows = nrow(reference),
    paths = paths,
    workbook = list(
      path = paths$workbook_file,
      sheet = attr(reference, "source_sheet") %||% NA_character_,
      skip = as.integer(attr(reference, "source_skip") %||% 0L)
    ),
    dictionary = list(
      path = attr(dictionary, "source_path") %||% NULL,
      version = attr(dictionary, "version") %||% NA_character_
    )
  )

  saveRDS(result, paths$reference_cache)
  result
}

get_species_life_form_lookup <- function(
    species,
    root = cocktailr_project_root(),
    force = FALSE,
    force_reference = FALSE,
    dictionary_path = NULL,
    workbook_path = NULL,
    ...
) {
  .require_semantic_packages()

  paths <- .life_form_layer_paths(root)
  if (!is.null(workbook_path)) {
    paths$workbook_file <- .life_form_workbook_path(root = root, path = workbook_path)
  }

  reference_object <- build_life_form_reference(
    root = root,
    force = force_reference,
    dictionary_path = dictionary_path,
    workbook_path = workbook_path,
    ...
  )

  aliases <- .load_species_aliases(paths)
  cache_signature <- paste(
    reference_object$source_signature,
    .alias_signature(aliases),
    sep = "||"
  )

  cached_data <- tibble::tibble()

  if (!force && file.exists(paths$species_cache)) {
    cached <- readRDS(paths$species_cache)

    if (
      is.list(cached) &&
      identical(cached$cache_signature, cache_signature)
    ) {
      cached_data <- cached$data
    }
  }

  requested <- unique(as.character(species))
  missing_species <- if (
    force ||
      nrow(cached_data) == 0L ||
      !"input_species" %in% names(cached_data)
  ) {
    requested
  } else {
    setdiff(requested, cached_data$input_species)
  }

  if (length(missing_species) > 0L) {
    new_matches <- .match_species_to_reference(
      species = missing_species,
      reference = reference_object$data,
      aliases = aliases
    )

    cached_data <- dplyr::bind_rows(
      cached_data,
      new_matches
    ) |>
      dplyr::distinct(
        .data$input_species,
        .keep_all = TRUE
      )

    saveRDS(
      list(
        cache_signature = cache_signature,
        created_at = Sys.time(),
        data = cached_data
      ),
      paths$species_cache
    )
  }

  dplyr::as_tibble(cached_data) |>
    dplyr::filter(.data$input_species %in% requested) |>
    dplyr::arrange(match(.data$input_species, requested))
}

.build_species_life_form_evidence <- function(
    cluster_species,
    lookup,
    dictionary
) {
  flag_names <- dictionary$raw_flag

  joined <- cluster_species |>
    dplyr::left_join(
      lookup,
      by = c("species" = "input_species")
    )

  if (!nrow(joined)) {
    return(.life_form_species_evidence_table())
  }

  rows <- purrr::map_dfr(flag_names, function(flag_name) {
    if (!flag_name %in% names(joined)) {
      return(tibble::tibble())
    }

    matched <- joined |>
      dplyr::filter(
        .data$match_method != "unmatched",
        .data[[flag_name]] > 0
      )

    if (!nrow(matched)) {
      return(tibble::tibble())
    }

    dict_row <- dictionary[dictionary$raw_flag == flag_name, , drop = FALSE]

    tibble::tibble(
      cluster = matched$cluster,
      species = matched$species,
      phi = matched$phi,
      raw_flag = flag_name,
      label = dict_row$label[[1L]],
      phrase = dict_row$phrase[[1L]],
      priority = as.integer(dict_row$priority[[1L]]),
      match_method = matched$match_method,
      matched_reference_name = matched$reference_name
    )
  })

  if (!nrow(rows)) {
    return(.life_form_species_evidence_table())
  }

  rows <- rows[order(
    rows$cluster,
    rows$priority,
    rows$label,
    -rows$phi,
    rows$species
  ), , drop = FALSE]
  rownames(rows) <- NULL
  as.data.frame(rows, stringsAsFactors = FALSE)
}

.summarise_cluster_life_forms <- function(species_evidence) {
  if (!is.data.frame(species_evidence) || !nrow(species_evidence)) {
    return(.life_form_summary_table())
  }

  out <- species_evidence |>
    dplyr::group_by(
      .data$cluster,
      .data$raw_flag,
      .data$label,
      .data$phrase,
      .data$priority
    ) |>
    dplyr::summarise(
      matched_species_count = dplyr::n_distinct(.data$species),
      matched_species = paste(sort(unique(.data$species)), collapse = ", "),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      .data$cluster,
      .data$priority,
      .data$label
    )

  out$evidence_id <- NA_character_
  as.data.frame(out, stringsAsFactors = FALSE)
}

score_cluster_life_forms <- function(
    x,
    clusters,
    min_phi = NULL,
    root = cocktailr_project_root(),
    force_species = FALSE,
    force_reference = FALSE,
    dictionary_path = NULL,
    workbook_path = NULL,
    ...
) {
  .require_semantic_packages()

  if (missing(clusters) || length(clusters) == 0L) {
    stop(
      "Provide one or more cluster labels in `clusters`.",
      call. = FALSE
    )
  }

  cluster_species <- .extract_cluster_species(
    x = x,
    clusters = clusters,
    min_phi = min_phi
  )

  dictionary <- .read_life_form_dictionary(path = dictionary_path)
  lookup <- get_species_life_form_lookup(
    species = unique(cluster_species$species),
    root = root,
    force = force_species,
    force_reference = force_reference,
    dictionary_path = dictionary_path,
    workbook_path = workbook_path,
    ...
  )

  species_evidence <- .build_species_life_form_evidence(
    cluster_species = cluster_species,
    lookup = lookup,
    dictionary = dictionary
  )

  cluster_life_forms <- .summarise_cluster_life_forms(species_evidence)

  unmatched_species <- lookup |>
    dplyr::filter(.data$match_method == "unmatched") |>
    dplyr::select(
      .data$input_species,
      .data$lookup_name,
      .data$match_method
    )

  paths <- .life_form_layer_paths(root)
  if (!is.null(workbook_path)) {
    paths$workbook_file <- .life_form_workbook_path(root = root, path = workbook_path)
  }

  result <- list(
    created_at = Sys.time(),
    clusters = clusters,
    cluster_species = cluster_species,
    cluster_life_forms = cluster_life_forms,
    species_evidence = species_evidence,
    species_lookup = lookup,
    unmatched_species = unmatched_species,
    paths = paths,
    dictionary = dictionary
  )

  saveRDS(
    result,
    file.path(
      paths$results_dir,
      "latest_cluster_life_form_profile.rds"
    )
  )

  utils::write.csv(
    cluster_life_forms,
    file.path(
      paths$results_dir,
      "latest_cluster_life_form_summary.csv"
    ),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  utils::write.csv(
    species_evidence,
    file.path(
      paths$results_dir,
      "latest_species_life_form_evidence.csv"
    ),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  utils::write.csv(
    unmatched_species,
    file.path(
      paths$results_dir,
      "latest_unmatched_species.csv"
    ),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  result
}

.life_form_cluster_unmatched_species <- function(result, cluster) {
  unmatched_species <- result$unmatched_species %||% NULL

  if (is.null(unmatched_species) || !nrow(unmatched_species)) {
    return(character(0))
  }

  cluster_species <- result$cluster_species %||% NULL
  if (is.null(cluster_species) || !nrow(cluster_species)) {
    return(character(0))
  }

  cluster_names <- unique(
    cluster_species$species[cluster_species$cluster == cluster]
  )

  unmatched <- unmatched_species$input_species[
    unmatched_species$input_species %in% cluster_names
  ]

  unique(as.character(unmatched))
}

.augment_cluster_evidence_with_life_form_layer <- function(
    evidence,
    life_form_result
) {
  if (!inherits(evidence, "cluster_evidence")) {
    stop("`evidence` must inherit from class `cluster_evidence`.")
  }

  cluster_id <- evidence$meta$cluster_id
  cluster_summary <- life_form_result$cluster_life_forms %||% NULL

  if (!is.data.frame(cluster_summary) || !nrow(cluster_summary)) {
    cluster_summary <- .life_form_summary_table()
  } else {
    cluster_summary <- cluster_summary[
      cluster_summary$cluster == cluster_id,
      ,
      drop = FALSE
    ]
  }

  next_id <- .cluster_evidence_next_semantic_id(evidence)
  new_items <- list()
  new_ids <- character(0)

  if (nrow(cluster_summary)) {
    for (i in seq_len(nrow(cluster_summary))) {
      evidence_id <- paste0("E", next_id)
      next_id <- next_id + 1L

      cluster_summary$evidence_id[[i]] <- evidence_id
      new_ids <- c(new_ids, evidence_id)
      new_items[[evidence_id]] <- list(
        id = evidence_id,
        type = "life_form",
        label = paste0(
          "Life-form evidence ",
          cluster_summary$label[[i]],
          " for ",
          cluster_id
        ),
        value = list(
          raw_flag = cluster_summary$raw_flag[[i]],
          label = cluster_summary$label[[i]],
          phrase = cluster_summary$phrase[[i]],
          priority = as.integer(cluster_summary$priority[[i]]),
          matched_species_count = as.integer(cluster_summary$matched_species_count[[i]]),
          matched_species = cluster_summary$matched_species[[i]]
        ),
        source = "score_cluster_life_forms",
        support_level = "derived"
      )
    }
  }

  evidence$evidence$items <- c(evidence$evidence$items, new_items)
  evidence$evidence$index$life_form_summary <- new_ids
  evidence$summaries$life_form_summary <- cluster_summary
  evidence$summaries$life_form_unmatched_species <- .life_form_cluster_unmatched_species(
    life_form_result,
    cluster_id
  )
  evidence$meta$source$has_life_form_layer <- TRUE
  evidence$future$life_form_layer <- list(
    source = "FloraVegEU life-form workbook",
    dictionary_path = attr(life_form_result$dictionary, "source_path") %||% NULL,
    reference_cache = life_form_result$paths$reference_cache %||% NULL,
    species_cache = life_form_result$paths$species_cache %||% NULL
  )

  evidence
}
