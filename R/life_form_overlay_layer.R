.life_form_overlay_layer_paths <- function(
    root = cocktailr_project_root()
) {
  cache_root <- file.path(root, "cache", "life_form_overlay_layer")

  paths <- list(
    root = root,
    cache_root = cache_root,
    results_dir = file.path(cache_root, "results")
  )

  dir.create(paths$results_dir, recursive = TRUE, showWarnings = FALSE)
  paths
}

.life_form_overlay_share_metrics <- function() {
  c(
    "dominant_life_form_share",
    "fixed_assignment_share",
    "mixed_assignment_share",
    "unmatched_species_share"
  )
}

.life_form_overlay_metric_label <- function(metric) {
  switch(
    metric,
    dominant_life_form_share = "Dominant life-form share",
    life_form_richness = "Life-form richness",
    fixed_assignment_share = "Fixed-assignment share",
    mixed_assignment_share = "Mixed-assignment share",
    unmatched_species_share = "Unmatched-species share",
    species_to_life_form_compression = "Species-to-life-form compression",
    metric
  )
}

.life_form_overlay_metric_value_text <- function(metric, value) {
  value <- suppressWarnings(as.numeric(value))
  if (!is.finite(value)) {
    return(NA_character_)
  }

  if (metric %in% .life_form_overlay_share_metrics()) {
    return(paste0(formatC(value * 100, digits = 1L, format = "f"), "%"))
  }
  if (identical(metric, "life_form_richness")) {
    return(as.character(as.integer(round(value))))
  }

  formatC(value, digits = 2L, format = "f")
}

.life_form_overlay_species_table <- function() {
  data.frame(
    cluster = character(0),
    species = character(0),
    phi = numeric(0),
    phi_rank = integer(0),
    match_method = character(0),
    matched_reference_name = character(0),
    assignment_state = character(0),
    life_form_count = integer(0),
    primary_raw_flag = character(0),
    primary_label = character(0),
    matched_raw_flags = character(0),
    matched_labels = character(0),
    evidence_id = character(0),
    stringsAsFactors = FALSE
  )
}

.life_form_overlay_metric_table <- function() {
  data.frame(
    cluster = character(0),
    metric = character(0),
    metric_label = character(0),
    value = numeric(0),
    value_text = character(0),
    bucket_label = character(0),
    bucket_phrase = character(0),
    evidence_id = character(0),
    stringsAsFactors = FALSE
  )
}

.life_form_overlay_diagnosis_table <- function() {
  data.frame(
    cluster = character(0),
    diagnosis_code = character(0),
    label = character(0),
    phrase = character(0),
    driver_metrics = character(0),
    evidence_id = character(0),
    stringsAsFactors = FALSE
  )
}

.life_form_overlay_species_flags <- function(row, dictionary) {
  flag_names <- dictionary$raw_flag
  present_flags <- flag_names[vapply(flag_names, function(flag_name) {
    value <- row[[flag_name]]
    length(value) == 1L && isTRUE(as.integer(value) > 0L)
  }, logical(1))]

  if (!length(present_flags)) {
    return(dictionary[0, , drop = FALSE])
  }

  dict_rows <- dictionary[dictionary$raw_flag %in% present_flags, , drop = FALSE]
  dict_rows[order(dict_rows$priority, dict_rows$label, dict_rows$raw_flag), , drop = FALSE]
}

.build_cluster_life_form_overlay_species <- function(
    cluster_species,
    lookup,
    dictionary
) {
  joined <- cluster_species |>
    dplyr::left_join(
      lookup,
      by = c("species" = "input_species")
    )

  if (!nrow(joined)) {
    return(.life_form_overlay_species_table())
  }

  joined <- joined[order(joined$cluster, -joined$phi, joined$species), , drop = FALSE]
  joined$phi_rank <- ave(
    joined$phi,
    joined$cluster,
    FUN = function(x) seq_along(x)
  )

  rows <- lapply(seq_len(nrow(joined)), function(i) {
    row <- joined[i, , drop = FALSE]
    flags <- .life_form_overlay_species_flags(row, dictionary)
    labels <- as.character(flags$label %||% character(0))
    raw_flags <- as.character(flags$raw_flag %||% character(0))
    n_labels <- length(labels)
    match_method <- .as_scalar_character(row$match_method %||% NA_character_)

    assignment_state <- if (!is.na(match_method) && identical(match_method, "unmatched")) {
      "unmatched"
    } else if (n_labels <= 0L) {
      "unmatched"
    } else if (n_labels == 1L) {
      "fixed"
    } else {
      "mixed"
    }

    data.frame(
      cluster = as.character(row$cluster),
      species = as.character(row$species),
      phi = suppressWarnings(as.numeric(row$phi)),
      phi_rank = as.integer(row$phi_rank),
      match_method = match_method,
      matched_reference_name = .as_scalar_character(row$reference_name %||% NA_character_),
      assignment_state = assignment_state,
      life_form_count = as.integer(n_labels),
      primary_raw_flag = if (n_labels) raw_flags[[1L]] else NA_character_,
      primary_label = if (n_labels) labels[[1L]] else NA_character_,
      matched_raw_flags = if (n_labels) paste(raw_flags, collapse = "; ") else NA_character_,
      matched_labels = if (n_labels) paste(labels, collapse = "; ") else NA_character_,
      evidence_id = NA_character_,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.life_form_overlay_label_counts <- function(overlay_species) {
  matched <- overlay_species[
    overlay_species$assignment_state != "unmatched",
    ,
    drop = FALSE
  ]
  if (!nrow(matched)) {
    return(integer(0))
  }

  label_vectors <- strsplit(matched$matched_labels, "; ", fixed = TRUE)
  labels <- unlist(label_vectors, use.names = FALSE)
  labels <- trimws(as.character(labels))
  labels <- labels[nzchar(labels)]

  if (!length(labels)) {
    return(integer(0))
  }

  sort(table(labels), decreasing = TRUE)
}

.summarise_cluster_life_form_overlay_metrics <- function(overlay_species) {
  if (!is.data.frame(overlay_species) || !nrow(overlay_species)) {
    return(.life_form_overlay_metric_table())
  }

  split_clusters <- split(overlay_species, overlay_species$cluster, drop = TRUE)
  rows <- lapply(names(split_clusters), function(cluster_id) {
    cluster_tbl <- split_clusters[[cluster_id]]
    total_species <- nrow(cluster_tbl)
    matched_tbl <- cluster_tbl[cluster_tbl$assignment_state != "unmatched", , drop = FALSE]
    matched_count <- nrow(matched_tbl)
    matched_assignment_count <- sum(
      suppressWarnings(as.integer(matched_tbl$life_form_count)),
      na.rm = TRUE
    )
    fixed_count <- sum(cluster_tbl$assignment_state == "fixed")
    mixed_count <- sum(cluster_tbl$assignment_state == "mixed")
    unmatched_count <- sum(cluster_tbl$assignment_state == "unmatched")

    label_counts <- .life_form_overlay_label_counts(cluster_tbl)
    dominant_share <- if (matched_count > 0L && length(label_counts)) {
      as.numeric(max(label_counts) / matched_count)
    } else {
      NA_real_
    }
    richness <- if (length(label_counts)) {
      as.numeric(length(label_counts))
    } else {
      0
    }
    compression <- if (matched_count > 0L && richness > 0) {
      as.numeric(matched_assignment_count / richness)
    } else {
      NA_real_
    }

    data.frame(
      cluster = rep(cluster_id, 6L),
      metric = .life_form_overlay_dictionary_supported_metrics(),
      value = c(
        dominant_share,
        richness,
        fixed_count / total_species,
        mixed_count / total_species,
        unmatched_count / total_species,
        compression
      ),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  out$metric_label <- vapply(
    out$metric,
    .life_form_overlay_metric_label,
    character(1)
  )
  out$value_text <- vapply(seq_len(nrow(out)), function(i) {
    .life_form_overlay_metric_value_text(out$metric[[i]], out$value[[i]])
  }, character(1))
  out$bucket_label <- NA_character_
  out$bucket_phrase <- NA_character_
  out$evidence_id <- NA_character_
  rownames(out) <- NULL
  out
}

.interpret_cluster_life_form_overlay_metrics <- function(
    overlay_metrics,
    dictionary = NULL
) {
  if (!is.data.frame(overlay_metrics) || !nrow(overlay_metrics)) {
    return(.life_form_overlay_metric_table())
  }

  dictionary <- dictionary %||% .read_life_form_overlay_dictionary()
  out <- as.data.frame(overlay_metrics, stringsAsFactors = FALSE)

  for (i in seq_len(nrow(out))) {
    match <- .life_form_overlay_dictionary_lookup(
      metric = out$metric[[i]],
      value = out$value[[i]],
      dictionary = dictionary
    )

    if (!is.null(match) && nrow(match)) {
      out$bucket_label[[i]] <- match$label[[1L]]
      out$bucket_phrase[[i]] <- match$phrase[[1L]]
    }
  }

  out
}

.build_cluster_life_form_overlay_diagnosis <- function(overlay_metrics) {
  if (!is.data.frame(overlay_metrics) || !nrow(overlay_metrics)) {
    return(.life_form_overlay_diagnosis_table())
  }

  split_clusters <- split(overlay_metrics, overlay_metrics$cluster, drop = TRUE)
  rows <- lapply(names(split_clusters), function(cluster_id) {
    cluster_metrics <- split_clusters[[cluster_id]]
    phrase_for <- function(metric_name) {
      row <- cluster_metrics[cluster_metrics$metric == metric_name, , drop = FALSE]
      if (!nrow(row)) {
        return(NULL)
      }
      text <- .as_scalar_character(row$bucket_phrase[[1L]])
      if (is.na(text) || !nzchar(text)) {
        return(NULL)
      }
      text
    }
    join_parts <- function(...) {
      parts <- unlist(list(...), use.names = FALSE)
      parts <- parts[!is.na(parts) & nzchar(parts)]
      paste(parts, collapse = " ")
    }

    lines <- data.frame(
      cluster = cluster_id,
      diagnosis_code = c(
        "structure_balance",
        "assignment_clarity",
        "compression_note"
      ),
      label = c(
        "Structure balance",
        "Assignment clarity",
        "Compression note"
      ),
      phrase = c(
        join_parts(
          "Dominance cue:",
          phrase_for("dominant_life_form_share"),
          "Richness cue:",
          phrase_for("life_form_richness")
        ),
        join_parts(
          "Fixed-assignment cue:",
          phrase_for("fixed_assignment_share"),
          "Mixed-assignment cue:",
          phrase_for("mixed_assignment_share"),
          "Coverage cue:",
          phrase_for("unmatched_species_share")
        ),
        join_parts(
          "Compression cue:",
          phrase_for("species_to_life_form_compression"),
          "Keep species names primary when compression is high or coverage is partial."
        )
      ),
      driver_metrics = c(
        "dominant_life_form_share; life_form_richness",
        "fixed_assignment_share; mixed_assignment_share; unmatched_species_share",
        "species_to_life_form_compression"
      ),
      evidence_id = NA_character_,
      stringsAsFactors = FALSE
    )
    lines
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

score_cluster_life_form_overlay <- function(
    x,
    clusters,
    min_phi = NULL,
    root = cocktailr_project_root(),
    force_species = FALSE,
    force_reference = FALSE,
    dictionary_path = NULL,
    workbook_path = NULL,
    overlay_dictionary_path = NULL,
    ...
) {
  simple_result <- score_cluster_life_forms(
    x = x,
    clusters = clusters,
    min_phi = min_phi,
    root = root,
    force_species = force_species,
    force_reference = force_reference,
    dictionary_path = dictionary_path,
    workbook_path = workbook_path,
    ...
  )

  overlay_dictionary <- .read_life_form_overlay_dictionary(
    path = overlay_dictionary_path
  )
  overlay_species <- .build_cluster_life_form_overlay_species(
    cluster_species = simple_result$cluster_species,
    lookup = simple_result$species_lookup,
    dictionary = simple_result$dictionary
  )
  overlay_metrics <- .summarise_cluster_life_form_overlay_metrics(
    overlay_species
  )
  overlay_metrics <- .interpret_cluster_life_form_overlay_metrics(
    overlay_metrics = overlay_metrics,
    dictionary = overlay_dictionary
  )
  overlay_diagnosis <- .build_cluster_life_form_overlay_diagnosis(
    overlay_metrics
  )

  paths <- .life_form_overlay_layer_paths(root)
  result <- list(
    created_at = Sys.time(),
    clusters = clusters,
    simple_result = simple_result,
    overlay_species = overlay_species,
    overlay_metrics = overlay_metrics,
    overlay_diagnosis = overlay_diagnosis,
    overlay_dictionary = overlay_dictionary,
    paths = paths
  )

  saveRDS(
    result,
    file.path(
      paths$results_dir,
      "latest_cluster_life_form_overlay_profile.rds"
    )
  )

  utils::write.csv(
    overlay_species,
    file.path(
      paths$results_dir,
      "latest_cluster_life_form_overlay_species.csv"
    ),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  utils::write.csv(
    overlay_metrics,
    file.path(
      paths$results_dir,
      "latest_cluster_life_form_overlay_metrics.csv"
    ),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  utils::write.csv(
    overlay_diagnosis,
    file.path(
      paths$results_dir,
      "latest_cluster_life_form_overlay_diagnosis.csv"
    ),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  result
}

.augment_cluster_evidence_with_life_form_overlay_layer <- function(
    evidence,
    life_form_overlay_result
) {
  if (!inherits(evidence, "cluster_evidence")) {
    stop("`evidence` must inherit from class `cluster_evidence`.")
  }

  evidence <- .augment_cluster_evidence_with_life_form_layer(
    evidence = evidence,
    life_form_result = life_form_overlay_result$simple_result
  )

  cluster_id <- evidence$meta$cluster_id
  overlay_species <- life_form_overlay_result$overlay_species %||% NULL
  overlay_metrics <- life_form_overlay_result$overlay_metrics %||% NULL
  overlay_diagnosis <- life_form_overlay_result$overlay_diagnosis %||% NULL

  if (!is.data.frame(overlay_species) || !nrow(overlay_species)) {
    overlay_species <- .life_form_overlay_species_table()
  } else {
    overlay_species <- overlay_species[
      overlay_species$cluster == cluster_id,
      ,
      drop = FALSE
    ]
  }
  if (!is.data.frame(overlay_metrics) || !nrow(overlay_metrics)) {
    overlay_metrics <- .life_form_overlay_metric_table()
  } else {
    overlay_metrics <- overlay_metrics[
      overlay_metrics$cluster == cluster_id,
      ,
      drop = FALSE
    ]
  }
  if (!is.data.frame(overlay_diagnosis) || !nrow(overlay_diagnosis)) {
    overlay_diagnosis <- .life_form_overlay_diagnosis_table()
  } else {
    overlay_diagnosis <- overlay_diagnosis[
      overlay_diagnosis$cluster == cluster_id,
      ,
      drop = FALSE
    ]
  }

  next_id <- .cluster_evidence_next_semantic_id(evidence)
  new_items <- list()

  if (nrow(overlay_species)) {
    species_ids <- character(nrow(overlay_species))
    for (i in seq_len(nrow(overlay_species))) {
      evidence_id <- paste0("E", next_id)
      next_id <- next_id + 1L
      species_ids[[i]] <- evidence_id

      overlay_species$evidence_id[[i]] <- evidence_id
      new_items[[evidence_id]] <- list(
        id = evidence_id,
        type = "life_form_overlay_species",
        label = paste0(
          "Life-form overlay species ",
          overlay_species$species[[i]],
          " for ",
          cluster_id
        ),
        value = as.list(overlay_species[i, , drop = FALSE]),
        source = "score_cluster_life_form_overlay",
        support_level = "derived"
      )
    }
    evidence$evidence$index$life_form_overlay_species <- species_ids
  }

  if (nrow(overlay_metrics)) {
    metric_ids <- character(nrow(overlay_metrics))
    for (i in seq_len(nrow(overlay_metrics))) {
      evidence_id <- paste0("E", next_id)
      next_id <- next_id + 1L
      metric_ids[[i]] <- evidence_id

      overlay_metrics$evidence_id[[i]] <- evidence_id
      new_items[[evidence_id]] <- list(
        id = evidence_id,
        type = "life_form_overlay_metric",
        label = paste0(
          "Life-form overlay metric ",
          overlay_metrics$metric[[i]],
          " for ",
          cluster_id
        ),
        value = list(
          metric = overlay_metrics$metric[[i]],
          metric_label = overlay_metrics$metric_label[[i]],
          value = overlay_metrics$value[[i]],
          value_text = overlay_metrics$value_text[[i]],
          bucket_label = overlay_metrics$bucket_label[[i]],
          bucket_phrase = overlay_metrics$bucket_phrase[[i]]
        ),
        source = "score_cluster_life_form_overlay",
        support_level = "derived"
      )
    }
    evidence$evidence$index$life_form_overlay_metrics <- metric_ids
  }

  if (nrow(overlay_diagnosis)) {
    diagnosis_ids <- character(nrow(overlay_diagnosis))
    for (i in seq_len(nrow(overlay_diagnosis))) {
      evidence_id <- paste0("E", next_id)
      next_id <- next_id + 1L
      diagnosis_ids[[i]] <- evidence_id

      overlay_diagnosis$evidence_id[[i]] <- evidence_id
      new_items[[evidence_id]] <- list(
        id = evidence_id,
        type = "life_form_overlay_diagnosis",
        label = paste0(
          "Life-form overlay diagnosis ",
          overlay_diagnosis$diagnosis_code[[i]],
          " for ",
          cluster_id
        ),
        value = list(
          diagnosis_code = overlay_diagnosis$diagnosis_code[[i]],
          label = overlay_diagnosis$label[[i]],
          phrase = overlay_diagnosis$phrase[[i]],
          driver_metrics = overlay_diagnosis$driver_metrics[[i]]
        ),
        source = "score_cluster_life_form_overlay",
        support_level = "derived"
      )
    }
    evidence$evidence$index$life_form_overlay_diagnosis <- diagnosis_ids
  }

  evidence$evidence$items <- c(evidence$evidence$items, new_items)
  evidence$summaries$life_form_overlay_species <- overlay_species
  evidence$summaries$life_form_overlay_metrics <- overlay_metrics
  evidence$summaries$life_form_overlay_diagnosis <- overlay_diagnosis
  evidence$meta$source$has_life_form_overlay_layer <- TRUE
  evidence$future$life_form_layer$overlay_dictionary_path <-
    attr(life_form_overlay_result$overlay_dictionary, "source_path") %||% NULL
  evidence$future$life_form_layer$overlay_cache_root <-
    life_form_overlay_result$paths$cache_root %||% NULL
  evidence$future$life_form_layer$mode <- "complex"

  evidence
}
