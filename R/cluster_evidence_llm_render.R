.cluster_evidence_llm_species_selection_policy <- function() {
  # Keep the model-facing species bundle bounded and deterministic:
  # start with phi-ranked species that also have cover context, then add
  # dominant species, then frequent species, then fill remaining slots by
  # dominant-cover order up to a fixed cap.
  policy <- list(
    top_phi_n = 6L,
    dominant_cover_n = 6L,
    frequent_cover_n = 6L,
    max_total_n = 12L
  )

  cap <- .cluster_evidence_llm_prompt_visible_species_cap()
  if (!is.null(cap)) {
    policy$max_total_n <- as.integer(cap)
  }

  policy
}

.cluster_evidence_llm_prompt_visible_species_cap <- function() {
  cap <- getOption("cocktailr.prompt_visible_species_cap", NULL)
  if (is.null(cap)) {
    return(NULL)
  }

  as.integer(.arg_nullable_positive_integer(
    cap,
    'getOption("cocktailr.prompt_visible_species_cap")'
  ))
}

.cluster_evidence_llm_filter_species <- function(species, selected_species = NULL) {
  species <- as.character(species %||% character(0))
  species <- species[!is.na(species) & nzchar(species)]

  if (!length(species) || is.null(selected_species)) {
    return(species)
  }

  selected_species <- as.character(selected_species %||% character(0))
  selected_species <- selected_species[!is.na(selected_species) & nzchar(selected_species)]
  if (!length(selected_species)) {
    return(character(0))
  }

  species[species %in% selected_species]
}

.cluster_evidence_llm_filter_csv_species <- function(text, selected_species = NULL) {
  text <- .as_scalar_character(text)
  if (is.na(text) || !nzchar(trimws(text)) || is.null(selected_species)) {
    return(text)
  }

  parts <- strsplit(text, ",", fixed = TRUE)[[1L]]
  parts <- trimws(as.character(parts))
  parts <- parts[nzchar(parts)]
  if (!length(parts)) {
    return("")
  }

  filtered <- .cluster_evidence_llm_filter_species(parts, selected_species = selected_species)
  if (!length(filtered)) {
    return("")
  }

  paste(filtered, collapse = ", ")
}

.cluster_evidence_llm_strip_evidence_ids <- function(text) {
  text <- .null_default(.as_scalar_character(text), "")
  if (!nzchar(text)) {
    return("")
  }

  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  lines <- gsub("[[:space:]]*\\[E[0-9]+\\]", "", lines, perl = TRUE)
  lines <- gsub("[[:space:]]{2,}", " ", lines, perl = TRUE)
  lines <- trimws(lines, which = "right")
  paste(lines, collapse = "\n")
}

.cluster_evidence_llm_cover_summary_table <- function(x) {
  out <- x$summaries$cover_summary %||% NULL

  if (!is.data.frame(out) || !nrow(out)) {
    return(data.frame(species = character(0), stringsAsFactors = FALSE))
  }

  if (!"species" %in% names(out)) {
    stop("`x$summaries$cover_summary` must contain a `species` column.")
  }

  out <- as.data.frame(out, stringsAsFactors = FALSE)
  out$species <- as.character(out$species)
  out <- out[!is.na(out$species) & nzchar(out$species), , drop = FALSE]
  out <- out[!duplicated(out$species), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.cluster_evidence_llm_phi_table <- function(x) {
  out <- x$summaries$species_phi %||% NULL

  if (!is.data.frame(out) || !nrow(out)) {
    return(data.frame(
      species = character(0),
      phi = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  if (!"species" %in% names(out)) {
    stop("`x$summaries$species_phi` must contain a `species` column.")
  }

  out <- as.data.frame(out, stringsAsFactors = FALSE)
  out$species <- as.character(out$species)
  out$phi <- suppressWarnings(as.numeric(out$phi %||% NA_real_))
  out <- out[!is.na(out$species) & nzchar(out$species), , drop = FALSE]
  out <- out[!duplicated(out$species), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.cluster_evidence_llm_cover_row <- function(species, cover_summary) {
  species <- .as_scalar_character(species)
  if (!is.data.frame(cover_summary) || !nrow(cover_summary)) {
    return(NULL)
  }

  idx <- match(species, cover_summary$species)
  if (is.na(idx) || idx < 1L) {
    return(NULL)
  }

  as.list(cover_summary[idx, , drop = FALSE])
}

.cluster_evidence_llm_selected_species_info <- function(x, policy = NULL) {
  policy <- policy %||% .cluster_evidence_llm_species_selection_policy()
  cover_summary <- .cluster_evidence_llm_cover_summary_table(x)
  phi_summary <- .cluster_evidence_llm_phi_table(x)
  topo_summary <- x$summaries$species_topological %||% NULL

  selected_species <- character(0)
  selected_via <- character(0)

  add_species <- function(species, via) {
    species <- as.character(species %||% character(0))
    species <- species[!is.na(species) & nzchar(species)]
    species <- species[!species %in% selected_species]
    if (!length(species)) {
      return(invisible(NULL))
    }

    selected_species <<- c(selected_species, species)
    selected_via <<- c(selected_via, rep(via, length(species)))
    invisible(NULL)
  }

  if (nrow(cover_summary)) {
    mean_cover <- suppressWarnings(as.numeric(cover_summary$mean_cover %||% NA_real_))
    freq_rank <- suppressWarnings(as.numeric(
      cover_summary$species_freq_count %||% NA_real_
    ))
    if (!length(freq_rank) || all(!is.finite(freq_rank))) {
      freq_rank <- suppressWarnings(as.numeric(
        cover_summary$freq_in_member_plots %||% NA_real_
      ))
    } else {
      fallback_freq <- suppressWarnings(as.numeric(
        cover_summary$freq_in_member_plots %||% NA_real_
      ))
      idx_na <- !is.finite(freq_rank)
      freq_rank[idx_na] <- fallback_freq[idx_na]
    }

    dominant_order <- order(
      -mean_cover,
      -freq_rank,
      cover_summary$species,
      na.last = TRUE
    )
    frequent_order <- order(
      -freq_rank,
      -mean_cover,
      cover_summary$species,
      na.last = TRUE
    )

    phi_species <- character(0)
    if (nrow(phi_summary)) {
      phi_cap <- min(nrow(phi_summary), as.integer(policy$top_phi_n))
      phi_species <- phi_summary$species[seq_len(phi_cap)]
      phi_species <- phi_species[phi_species %in% cover_summary$species]
    }

    dominant_species <- cover_summary$species[dominant_order]
    frequent_species <- cover_summary$species[frequent_order]

    add_species(phi_species, "phi")
    add_species(
      utils::head(dominant_species, as.integer(policy$dominant_cover_n)),
      "dominant_cover"
    )
    add_species(
      utils::head(frequent_species, as.integer(policy$frequent_cover_n)),
      "frequent_cover"
    )
    add_species(dominant_species, "cover_fill")
  } else {
    if (nrow(phi_summary)) {
      add_species(
        utils::head(phi_summary$species, as.integer(policy$top_phi_n)),
        "phi"
      )
    }

    if (is.data.frame(topo_summary) && nrow(topo_summary) && "species" %in% names(topo_summary)) {
      add_species(as.character(topo_summary$species), "topological_fallback")
    }
  }

  if (length(selected_species) > as.integer(policy$max_total_n)) {
    keep <- seq_len(as.integer(policy$max_total_n))
    selected_species <- selected_species[keep]
    selected_via <- selected_via[keep]
  }

  data.frame(
    species = selected_species,
    selected_via = selected_via,
    stringsAsFactors = FALSE
  )
}

.cluster_evidence_llm_species_axis_values_table <- function(x) {
  axis_cols <- c("l", "m", "n", "r", "t", "s")
  out <- x$summaries$species_axis_values %||% NULL

  if (!is.data.frame(out) || !nrow(out)) {
    empty <- data.frame(species = character(0), stringsAsFactors = FALSE)
    for (axis in axis_cols) {
      empty[[axis]] <- numeric(0)
    }
    return(empty)
  }

  if (!"species" %in% names(out)) {
    stop("`x$summaries$species_axis_values` must contain a `species` column.")
  }

  out <- as.data.frame(out, stringsAsFactors = FALSE)
  out$species <- as.character(out$species)
  for (axis in axis_cols) {
    if (!axis %in% names(out)) {
      out[[axis]] <- NA_real_
    } else {
      out[[axis]] <- suppressWarnings(as.numeric(out[[axis]]))
    }
  }

  out[, c("species", axis_cols), drop = FALSE]
}

.cluster_evidence_llm_species_axis_row <- function(species, species_axis_values) {
  species <- .as_scalar_character(species)
  if (!is.data.frame(species_axis_values) || !nrow(species_axis_values)) {
    return(NULL)
  }

  idx <- match(species, species_axis_values$species)
  if (is.na(idx) || idx < 1L) {
    return(NULL)
  }

  as.list(species_axis_values[idx, , drop = FALSE])
}

.cluster_evidence_llm_cluster_axis_items <- function(x, dictionary = NULL) {
  semantic_axes <- x$summaries$semantic_axes %||% NULL
  axis_order <- c("l", "m", "n", "r", "t", "s")

  if (!is.data.frame(semantic_axes) || !nrow(semantic_axes)) {
    return(character(0))
  }
  if (!"axis" %in% names(semantic_axes)) {
    stop("`x$summaries$semantic_axes` must contain an `axis` column.")
  }
  if (!"score_0_10" %in% names(semantic_axes)) {
    stop("`x$summaries$semantic_axes` must contain a `score_0_10` column.")
  }

  semantic_axes <- as.data.frame(semantic_axes, stringsAsFactors = FALSE)
  semantic_axes$axis <- vapply(
    semantic_axes$axis,
    .cluster_evidence_llm_normalize_axis,
    character(1)
  )
  semantic_axes$score_0_10 <- suppressWarnings(
    as.numeric(semantic_axes$score_0_10)
  )
  semantic_axes$row_id <- seq_len(nrow(semantic_axes))
  semantic_axes$order_axis <- match(semantic_axes$axis, axis_order)
  semantic_axes <- semantic_axes[
    !is.na(semantic_axes$axis) & !is.na(semantic_axes$order_axis),
    ,
    drop = FALSE
  ]

  if (!nrow(semantic_axes)) {
    return(character(0))
  }

  semantic_axes <- semantic_axes[
    order(semantic_axes$order_axis, semantic_axes$row_id),
    ,
    drop = FALSE
  ]
  semantic_axes <- semantic_axes[
    !duplicated(semantic_axes$axis),
    ,
    drop = FALSE
  ]

  items <- vapply(seq_len(nrow(semantic_axes)), function(i) {
    .cluster_evidence_llm_axis_sentence(
      axis = semantic_axes$axis[[i]],
      value = semantic_axes$score_0_10[[i]],
      dictionary = dictionary
    )
  }, character(1))

  items[!is.na(items) & nzchar(items)]
}

.cluster_evidence_llm_life_form_items <- function(x, selected_species = NULL) {
  life_form_summary <- x$summaries$life_form_summary %||% NULL

  if (!is.data.frame(life_form_summary) || !nrow(life_form_summary)) {
    return(character(0))
  }

  required <- c("label", "phrase", "matched_species")
  missing <- setdiff(required, names(life_form_summary))
  if (length(missing)) {
    stop(
      "`x$summaries$life_form_summary` must contain columns: ",
      paste(required, collapse = ", "),
      ". Missing: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  life_form_summary <- as.data.frame(
    life_form_summary,
    stringsAsFactors = FALSE
  )
  if (!"priority" %in% names(life_form_summary)) {
    life_form_summary$priority <- seq_len(nrow(life_form_summary))
  }
  life_form_summary$priority <- suppressWarnings(
    as.integer(life_form_summary$priority)
  )
  life_form_summary$order_row <- seq_len(nrow(life_form_summary))
  life_form_summary <- life_form_summary[order(
    life_form_summary$priority,
    life_form_summary$order_row
  ), , drop = FALSE]

  vapply(seq_len(nrow(life_form_summary)), function(i) {
    label <- trimws(as.character(life_form_summary$label[[i]]))
    phrase <- trimws(as.character(life_form_summary$phrase[[i]]))
    matched_species <- .cluster_evidence_llm_filter_csv_species(
      trimws(as.character(life_form_summary$matched_species[[i]])),
      selected_species = selected_species
    )

    parts <- c(
      paste0(label, ": ", phrase, ".")
    )

    if (nzchar(matched_species)) {
      parts <- c(
        parts,
        paste0("Matched cluster species: ", matched_species, ".")
      )
    }

    paste(parts, collapse = " ")
  }, character(1))
}

.cluster_evidence_llm_life_form_overlay_species_items <- function(
    x,
    selected_species = NULL
) {
  overlay_species <- x$summaries$life_form_overlay_species %||% NULL

  if (!is.data.frame(overlay_species) || !nrow(overlay_species)) {
    return(character(0))
  }

  overlay_species <- as.data.frame(overlay_species, stringsAsFactors = FALSE)
  if (!"phi_rank" %in% names(overlay_species)) {
    overlay_species$phi_rank <- seq_len(nrow(overlay_species))
  }
  if (!"phi" %in% names(overlay_species)) {
    overlay_species$phi <- NA_real_
  }
  overlay_species <- overlay_species[order(
    overlay_species$phi_rank,
    -overlay_species$phi,
    overlay_species$species
  ), , drop = FALSE]
  if (!is.null(selected_species)) {
    overlay_species <- overlay_species[
      overlay_species$species %in% selected_species,
      ,
      drop = FALSE
    ]
  }

  if (!nrow(overlay_species)) {
    return(character(0))
  }

  vapply(seq_len(nrow(overlay_species)), function(i) {
    row <- overlay_species[i, , drop = FALSE]
    species <- .as_scalar_character(row$species[[1L]])
    phi <- suppressWarnings(as.numeric(row$phi[[1L]]))
    assignment <- .as_scalar_character(row$assignment_state[[1L]])
    labels <- .as_scalar_character(row$matched_labels[[1L]])

    if (identical(assignment, "unmatched") || is.na(labels) || !nzchar(labels)) {
      return(
        paste0(
          species,
          " (phi=",
          formatC(phi, digits = 2L, format = "f"),
          "): no workbook life-form match."
        )
      )
    }

    paste0(
      species,
      " (phi=",
      formatC(phi, digits = 2L, format = "f"),
      "): ",
      assignment,
      " assignment -> ",
      labels,
      "."
    )
  }, character(1))
}

.cluster_evidence_llm_life_form_overlay_metric_items <- function(x) {
  overlay_metrics <- x$summaries$life_form_overlay_metrics %||% NULL

  if (!is.data.frame(overlay_metrics) || !nrow(overlay_metrics)) {
    return(character(0))
  }

  overlay_metrics <- as.data.frame(overlay_metrics, stringsAsFactors = FALSE)

  vapply(seq_len(nrow(overlay_metrics)), function(i) {
    row <- overlay_metrics[i, , drop = FALSE]
    metric_label <- .as_scalar_character(row$metric_label[[1L]])
    value_text <- .as_scalar_character(row$value_text[[1L]])
    bucket_label <- .as_scalar_character(row$bucket_label[[1L]])
    bucket_phrase <- .as_scalar_character(row$bucket_phrase[[1L]])

    parts <- c(
      paste0(metric_label, ": ", value_text)
    )
    if (!is.na(bucket_label) && nzchar(bucket_label)) {
      parts[[1L]] <- paste0(parts[[1L]], " (", bucket_label, ")")
    }
    if (!is.na(bucket_phrase) && nzchar(bucket_phrase)) {
      parts[[1L]] <- paste0(parts[[1L]], "; ", bucket_phrase)
    }

    paste(parts, collapse = "")
  }, character(1))
}

.cluster_evidence_llm_life_form_overlay_diagnosis_items <- function(x) {
  overlay_diagnosis <- x$summaries$life_form_overlay_diagnosis %||% NULL

  if (!is.data.frame(overlay_diagnosis) || !nrow(overlay_diagnosis)) {
    return(character(0))
  }

  overlay_diagnosis <- as.data.frame(overlay_diagnosis, stringsAsFactors = FALSE)

  vapply(seq_len(nrow(overlay_diagnosis)), function(i) {
    row <- overlay_diagnosis[i, , drop = FALSE]
    paste0(
      .as_scalar_character(row$label[[1L]]),
      ": ",
      .as_scalar_character(row$phrase[[1L]])
    )
  }, character(1))
}

.cluster_evidence_llm_axis_sentence <- function(axis, value, dictionary) {
  value <- suppressWarnings(as.numeric(value))
  if (!is.finite(value)) {
    return(NULL)
  }

  phrase <- .cluster_evidence_llm_axis_phrase(
    axis = axis,
    value = value,
    dictionary = dictionary
  )
  axis_name <- .semantic_axis_display_name(axis)
  base <- paste0(
    axis_name,
    " ",
    .cluster_evidence_format_decimal(value, digits = 2L),
    "/10"
  )

  if (!is.na(phrase) && nzchar(phrase)) {
    paste0(base, " (", phrase, ").")
  } else {
    paste0(base, ".")
  }
}

.cluster_evidence_llm_species_line <- function(
    species,
    cover_row = NULL,
    species_axis_row = NULL,
    dictionary = NULL
) {
  species <- .as_scalar_character(species)
  if (is.na(species) || !nzchar(species)) {
    return(NULL)
  }

  if (is.null(cover_row)) {
    return(species)
  }

  intro <- paste0(
    species,
    ": occurs in ",
    as.integer(cover_row$species_freq_count %||% NA_integer_),
    " of ",
    as.integer(cover_row$n_member_plots %||% NA_integer_),
    " member plots (",
    .cluster_evidence_format_percent(cover_row$species_freq_pct %||% NA_real_),
    "%)."
  )

  parts <- c(
    intro,
    .cluster_evidence_llm_cover_sentence(cover_row)
  )

  axis_values <- species_axis_row %||% list()
  for (axis in c("l", "m", "n", "r", "t", "s")) {
    sentence <- .cluster_evidence_llm_axis_sentence(
      axis = axis,
      value = axis_values[[axis]] %||% NA_real_,
      dictionary = dictionary
    )
    if (!is.null(sentence)) {
      parts <- c(parts, sentence)
    }
  }

  paste(parts, collapse = " ")
}

.cluster_evidence_llm_phi_items <- function(
    x,
    exclude_species = character(0),
    policy = NULL,
    selected_species = NULL
) {
  policy <- policy %||% .cluster_evidence_llm_species_selection_policy()
  phi_summary <- .cluster_evidence_llm_phi_table(x)

  if (!nrow(phi_summary)) {
    return(character(0))
  }

  if (!is.null(selected_species)) {
    phi_summary <- phi_summary[
      phi_summary$species %in% selected_species,
      ,
      drop = FALSE
    ]
    if (!nrow(phi_summary)) {
      return(character(0))
    }
  }

  phi_cap <- min(nrow(phi_summary), as.integer(policy$top_phi_n))
  phi_summary <- phi_summary[seq_len(phi_cap), , drop = FALSE]

  exclude_species <- as.character(exclude_species %||% character(0))
  exclude_species <- exclude_species[!is.na(exclude_species) & nzchar(exclude_species)]
  if (length(exclude_species)) {
    unique_phi <- phi_summary[!phi_summary$species %in% exclude_species, , drop = FALSE]
    if (nrow(unique_phi)) {
      phi_summary <- unique_phi
    }
  }

  paste0(
    phi_summary$species,
    " (phi=",
    formatC(phi_summary$phi, digits = 3L, format = "f"),
    ")"
  )
}

.cluster_evidence_llm_topological_species_items <- function(
    x,
    dictionary = NULL,
    selected_species = NULL
) {
  cover_summary <- .cluster_evidence_llm_cover_summary_table(x)
  species_axis_values <- .cluster_evidence_llm_species_axis_values_table(x)
  selected_species <- as.character(selected_species %||% character(0))
  selected_species <- selected_species[!is.na(selected_species) & nzchar(selected_species)]

  if (nrow(cover_summary)) {
    if (!length(selected_species)) {
      selected_species <- .cluster_evidence_llm_selected_species_info(x)$species
    }

    return(vapply(selected_species, function(species) {
      cover_row <- .cluster_evidence_llm_cover_row(species, cover_summary)
      .cluster_evidence_llm_species_line(
        species = species,
        cover_row = cover_row,
        species_axis_row = .cluster_evidence_llm_species_axis_row(
          species,
          species_axis_values
        ),
        dictionary = dictionary
      )
    }, character(1)))
  }

  topo_summary <- x$summaries$species_topological %||% NULL
  if (is.data.frame(topo_summary) && nrow(topo_summary)) {
    if (!length(selected_species)) {
      selected_species <- .cluster_evidence_llm_selected_species_info(x)$species
    }

    if (!length(selected_species)) {
      selected_species <- as.character(topo_summary$species)
    }

    return(selected_species)
  }

  character(0)
}

.cluster_evidence_llm_prompt_blocks <- function(x) {
  if (!inherits(x, "cluster_evidence")) {
    stop("`x` must inherit from class `cluster_evidence`.")
  }

  axis_dictionary <- .read_cluster_evidence_llm_axis_dictionary()
  selected_species_info <- .cluster_evidence_llm_selected_species_info(x)
  selected_species_filter <- if (is.null(.cluster_evidence_llm_prompt_visible_species_cap())) {
    NULL
  } else {
    selected_species_info$species
  }
  topo_items <- .cluster_evidence_llm_topological_species_items(
    x,
    dictionary = axis_dictionary,
    selected_species = selected_species_info$species
  )
  phi_items <- .cluster_evidence_llm_phi_items(
    x,
    exclude_species = selected_species_info$species,
    selected_species = selected_species_filter
  )

  prototype_items <- if (nrow(x$summaries$plots_prototype)) {
    paste0(
      x$summaries$plots_prototype$plot,
      " (score=",
      formatC(x$summaries$plots_prototype$support_score, digits = 3L, format = "f"),
      ")"
    )
  } else {
    character(0)
  }

  borderline_items <- if (nrow(x$summaries$plots_borderline)) {
    paste0(
      x$summaries$plots_borderline$plot,
      " (score=",
      formatC(x$summaries$plots_borderline$support_score, digits = 3L, format = "f"),
      ")"
    )
  } else {
    character(0)
  }

  semantic_axis_items <- .cluster_evidence_llm_cluster_axis_items(
    x,
    dictionary = axis_dictionary
  )
  life_form_overlay_species_items <- .cluster_evidence_llm_life_form_overlay_species_items(
    x,
    selected_species = selected_species_filter
  )
  life_form_overlay_metric_items <- .cluster_evidence_llm_life_form_overlay_metric_items(x)
  life_form_overlay_diagnosis_items <- .cluster_evidence_llm_life_form_overlay_diagnosis_items(x)
  life_form_items <- .cluster_evidence_llm_life_form_items(
    x,
    selected_species = selected_species_filter
  )

  semantic_unmatched_items <- .cluster_evidence_llm_filter_species(
    as.character(x$summaries$semantic_unmatched_species %||% character(0)),
    selected_species = selected_species_filter
  )

  limitation_items <- as.character(c(
    x$limitations$warnings %||% character(0),
    x$limitations$unsupported_inferences %||% character(0)
  ))
  dataset_context_line <- .cluster_evidence_llm_dataset_context_line(x)
  cluster_metric_items <- .cluster_evidence_llm_cluster_metric_items(x)
  user_added_lines <- .cluster_evidence_user_added_prompt_lines(
    x$user_added_data %||% NULL
  )

  list(
    .new_cluster_evidence_prompt_fixed_block(
      id = "cluster_header",
      label = "Cluster header",
      display_order = 10L,
      retain_rank = 10L,
      lines = paste0("Cluster: ", x$meta$cluster_id)
    ),
    .new_cluster_evidence_prompt_fixed_block(
      id = "dataset_context",
      label = "Dataset context",
      display_order = 20L,
      retain_rank = 20L,
      lines = dataset_context_line
    ),
    .new_cluster_evidence_prompt_bulleted_block(
      id = "cluster_metrics",
      label = "How to read these cluster metrics",
      display_order = 25L,
      retain_rank = 25L,
      header = "How to read these cluster metrics:",
      items = cluster_metric_items
    ),
    .new_cluster_evidence_prompt_bulleted_block(
      id = "species_topological",
      label = "Plants that regularly occur in this cluster",
      display_order = 30L,
      retain_rank = 50L,
      header = "Plants that regularly occur in this cluster:",
      items = topo_items
    ),
    .new_cluster_evidence_prompt_inline_block(
      id = "species_phi",
      label = "Species with the strongest cluster association",
      display_order = 40L,
      retain_rank = 40L,
      header = "Species with the strongest cluster association: ",
      items = phi_items
    ),
    .new_cluster_evidence_prompt_inline_block(
      id = "plots_prototype",
      label = "Prototype plots",
      display_order = 60L,
      retain_rank = 60L,
      header = "Prototype plots: ",
      items = prototype_items
    ),
    .new_cluster_evidence_prompt_inline_block(
      id = "plots_borderline",
      label = "Borderline plots",
      display_order = 70L,
      retain_rank = 90L,
      header = "Borderline plots: ",
      items = borderline_items
    ),
    .new_cluster_evidence_prompt_fixed_block(
      id = "user_added_data",
      label = "User-added data",
      display_order = 85L,
      retain_rank = 65L,
      lines = user_added_lines
    ),
    .new_cluster_evidence_prompt_bulleted_block(
      id = "life_form_overlay_species",
      label = "Species-first plant life-form overlay",
      display_order = 86L,
      retain_rank = 66L,
      header = "Species-first plant life-form overlay:",
      items = life_form_overlay_species_items
    ),
    .new_cluster_evidence_prompt_bulleted_block(
      id = "life_form_overlay_metrics",
      label = "Life-form structure metrics",
      display_order = 87L,
      retain_rank = 67L,
      header = "Life-form structure metrics:",
      items = life_form_overlay_metric_items
    ),
    .new_cluster_evidence_prompt_bulleted_block(
      id = "life_form_overlay_diagnosis",
      label = "Life-form overlay diagnosis",
      display_order = 88L,
      retain_rank = 67L,
      header = "Life-form overlay diagnosis:",
      items = life_form_overlay_diagnosis_items
    ),
    .new_cluster_evidence_prompt_bulleted_block(
      id = "life_form_summary",
      label = "Plant life-form context for the cluster",
      display_order = 89L,
      retain_rank = 68L,
      header = "Plant life-form context for the cluster:",
      items = life_form_items
    ),
    .new_cluster_evidence_prompt_bulleted_block(
      id = "semantic_axes",
      label = "Ecological axis summary for the cluster",
      display_order = 90L,
      retain_rank = 70L,
      header = "Ecological axis summary for the cluster:",
      items = semantic_axis_items
    ),
    .new_cluster_evidence_prompt_inline_block(
      id = "semantic_unmatched_species",
      label = "Species without ecological-axis values",
      display_order = 100L,
      retain_rank = 110L,
      header = "Species without ecological-axis values: ",
      items = semantic_unmatched_items
    ),
    .new_cluster_evidence_prompt_bulleted_block(
      id = "limitations",
      label = "Limitations",
      display_order = 110L,
      retain_rank = 80L,
      header = "Limitations:",
      items = limitation_items
    )
  )
}

.serialize_cluster_evidence_llm_prompt <- function(x, max_chars = NULL) {
  if (!inherits(x, "cluster_evidence")) {
    stop("`x` must inherit from class `cluster_evidence`.")
  }

  rendered <- .serialize_cluster_evidence_blocks(
    .cluster_evidence_llm_prompt_blocks(x),
    max_chars = max_chars
  )

  rendered$text <- .cluster_evidence_llm_strip_evidence_ids(rendered$text)
  rendered$full_text <- .cluster_evidence_llm_strip_evidence_ids(
    rendered$full_text
  )
  rendered$chars_full <- .cluster_evidence_prompt_char_count(rendered$full_text)
  rendered$chars_used <- .cluster_evidence_prompt_char_count(rendered$text)
  rendered$trimmed <- isTRUE(rendered$trimmed) ||
    rendered$chars_used < rendered$chars_full
  rendered$prompt_visible_species_cap <- .cluster_evidence_llm_prompt_visible_species_cap()

  rendered
}

.format_cluster_evidence_llm_prompt <- function(x, max_chars = NULL) {
  .serialize_cluster_evidence_llm_prompt(x, max_chars = max_chars)$text
}

.cluster_evidence_synopsis_axis_bucket <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  if (!is.finite(value)) {
    return(NA_character_)
  }

  if (value < (10 / 3)) {
    return("low")
  }
  if (value < (20 / 3)) {
    return("mid")
  }
  "high"
}

.cluster_evidence_synopsis_axis_item <- function(axis, value, dictionary = NULL) {
  value <- suppressWarnings(as.numeric(value))
  if (!is.finite(value)) {
    return(NULL)
  }

  phrase <- .cluster_evidence_llm_axis_phrase(
    axis = axis,
    value = value,
    dictionary = dictionary
  )
  paste0(
    .semantic_axis_display_name(axis),
    "=",
    .cluster_evidence_synopsis_axis_bucket(value),
    " (",
    .cluster_evidence_format_decimal(value, digits = 2L),
    "/10",
    if (!is.na(phrase) && nzchar(phrase)) paste0("; ", phrase) else "",
    ")"
  )
}

.cluster_evidence_synopsis_semantic_summary_items <- function(x, dictionary = NULL) {
  semantic_axes <- x$summaries$semantic_axes %||% NULL
  axis_order <- c("l", "m", "n", "r", "t", "s")

  if (!is.data.frame(semantic_axes) || !nrow(semantic_axes)) {
    return(character(0))
  }

  semantic_axes <- as.data.frame(semantic_axes, stringsAsFactors = FALSE)
  semantic_axes$axis <- vapply(
    semantic_axes$axis,
    .cluster_evidence_llm_normalize_axis,
    character(1)
  )
  semantic_axes$score_0_10 <- suppressWarnings(as.numeric(semantic_axes$score_0_10))
  semantic_axes$order_axis <- match(semantic_axes$axis, axis_order)
  semantic_axes <- semantic_axes[
    !is.na(semantic_axes$order_axis),
    ,
    drop = FALSE
  ]
  semantic_axes <- semantic_axes[
    order(semantic_axes$order_axis),
    ,
    drop = FALSE
  ]

  items <- vapply(seq_len(nrow(semantic_axes)), function(i) {
    .cluster_evidence_synopsis_axis_item(
      axis = semantic_axes$axis[[i]],
      value = semantic_axes$score_0_10[[i]],
      dictionary = dictionary
    ) %||% ""
  }, character(1))

  items[nzchar(items)]
}

.cluster_evidence_synopsis_parse_species_csv <- function(text) {
  text <- .as_scalar_character(text)
  if (is.na(text) || !nzchar(trimws(text))) {
    return(character(0))
  }

  parts <- trimws(strsplit(text, ",", fixed = TRUE)[[1L]])
  parts[nzchar(parts)]
}

.cluster_evidence_synopsis_life_form_label_counts <- function(x) {
  overlay_species <- x$summaries$life_form_overlay_species %||% NULL
  if (is.data.frame(overlay_species) && nrow(overlay_species)) {
    matched <- overlay_species[
      overlay_species$assignment_state != "unmatched",
      ,
      drop = FALSE
    ]
    if (!nrow(matched) || !"matched_labels" %in% names(matched)) {
      return(integer(0))
    }

    labels <- unlist(lapply(
      as.character(matched$matched_labels),
      function(text) {
        parts <- trimws(strsplit(.as_scalar_character(text), ";", fixed = TRUE)[[1L]])
        parts[nzchar(parts)]
      }
    ), use.names = FALSE)

    labels <- labels[!is.na(labels) & nzchar(labels)]
    if (!length(labels)) {
      return(integer(0))
    }

    return(sort(table(labels), decreasing = TRUE))
  }

  life_form_summary <- x$summaries$life_form_summary %||% NULL
  if (!is.data.frame(life_form_summary) || !nrow(life_form_summary)) {
    return(integer(0))
  }
  if (!all(c("label", "matched_species_count") %in% names(life_form_summary))) {
    return(integer(0))
  }

  counts <- suppressWarnings(as.integer(life_form_summary$matched_species_count))
  names(counts) <- as.character(life_form_summary$label)
  counts <- counts[!is.na(names(counts)) & nzchar(names(counts)) & is.finite(counts)]
  if (!length(counts)) {
    return(integer(0))
  }

  counts <- tapply(counts, names(counts), sum)
  sort(counts, decreasing = TRUE)
}

.cluster_evidence_synopsis_used_life_form_labels <- function(x) {
  counts <- .cluster_evidence_synopsis_life_form_label_counts(x)
  if (!length(counts)) {
    return(character(0))
  }
  unique(names(counts))
}

.cluster_evidence_synopsis_life_form_structural_groups <- function() {
  data.frame(
    raw_flag = c(
      "tree",
      "shrub",
      "woody_liana",
      "phanerophyte",
      "dwarf_shrub",
      "semi_shrub",
      "chamaephyte",
      "hemicryptophyte",
      "geophyte",
      "hydrophyte",
      "therophyte",
      "epiphyte",
      "herbaceous_liana"
    ),
    group_id = c(
      "woody_upper_layer",
      "woody_upper_layer",
      "woody_upper_layer",
      "woody_upper_layer",
      "low_woody_layer",
      "low_woody_layer",
      "low_woody_layer",
      "near_ground_perennial_herbs",
      "belowground_perennials",
      "aquatic_wetland_plants",
      "annual_herbs",
      "epiphytes",
      "climbing_herbs"
    ),
    group_label = c(
      "woody trees and shrubs",
      "woody trees and shrubs",
      "woody trees and shrubs",
      "woody trees and shrubs",
      "low woody and subshrub plants",
      "low woody and subshrub plants",
      "low woody and subshrub plants",
      "near-ground perennial herbs",
      "plants with belowground storage or buds",
      "aquatic or persistently wet-site plants",
      "annual herbs",
      "epiphytes",
      "climbing herbs"
    ),
    descriptor = c(
      "woody trees, shrubs, or lianas that help form a persistent aboveground layer",
      "woody trees, shrubs, or lianas that help form a persistent aboveground layer",
      "woody trees, shrubs, or lianas that help form a persistent aboveground layer",
      "woody trees, shrubs, or lianas that help form a persistent aboveground layer",
      "low woody or subshrub plants concentrated close to the soil surface",
      "low woody or subshrub plants concentrated close to the soil surface",
      "low woody or subshrub plants concentrated close to the soil surface",
      "perennial herbs with renewal buds close to the soil surface",
      "plants that survive the unfavorable season with buds or storage organs belowground",
      "plants tied to standing water or persistently wet microsites",
      "short-lived annual herbs that persist through the unfavorable season mainly as seeds",
      "plants that depend on host surfaces rather than rooted ground cover",
      "climbing herbs that rely on surrounding vegetation for support"
    ),
    interpretation = c(
      "This usually points to a clear woody layer and comparatively persistent aboveground structure.",
      "This usually points to a clear woody layer and comparatively persistent aboveground structure.",
      "This usually points to a clear woody layer and comparatively persistent aboveground structure.",
      "This usually points to a clear woody layer and comparatively persistent aboveground structure.",
      "This suggests a low-stature persistent layer close to the soil surface.",
      "This suggests a low-stature persistent layer close to the soil surface.",
      "This suggests a low-stature persistent layer close to the soil surface.",
      "This often fits a persistent seasonal herb layer in temperate vegetation.",
      "This often signals seasonal avoidance through bulbs, rhizomes, or other belowground organs.",
      "This usually points to standing water or persistently wet soil conditions.",
      "This often fits seasonal or disturbance-tolerant vegetation and can also align with dry or open conditions.",
      "This is a supporting structural cue about substrate use, not usually the main ecological driver.",
      "This is a supporting structural cue about vertical support and local layering."
    ),
    interpretation_source = c(
      "literature-backed",
      "literature-backed",
      "literature-backed",
      "literature-backed",
      "heuristic",
      "heuristic",
      "literature-backed",
      "literature-backed",
      "literature-backed",
      "literature-backed",
      "literature-backed",
      "heuristic",
      "heuristic"
    ),
    order = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 4L, 5L, 6L, 7L, 8L),
    stringsAsFactors = FALSE
  )
}

.cluster_evidence_synopsis_life_form_assignments <- function(x) {
  dictionary <- .read_life_form_dictionary()
  label_to_flag <- setNames(dictionary$raw_flag, dictionary$label)
  flag_priority <- setNames(dictionary$priority, dictionary$raw_flag)

  overlay_species <- x$summaries$life_form_overlay_species %||% NULL
  if (is.data.frame(overlay_species) && nrow(overlay_species)) {
    overlay_species <- as.data.frame(overlay_species, stringsAsFactors = FALSE)
    matched_tbl <- overlay_species[
      overlay_species$assignment_state != "unmatched",
      ,
      drop = FALSE
    ]

    if (nrow(matched_tbl)) {
      primary_flags <- vapply(seq_len(nrow(matched_tbl)), function(i) {
        row <- matched_tbl[i, , drop = FALSE]
        primary_flag <- .as_scalar_character(row$primary_raw_flag %||% NA_character_)
        if (!is.na(primary_flag) && nzchar(primary_flag)) {
          return(primary_flag)
        }

        primary_label <- .as_scalar_character(row$primary_label %||% NA_character_)
        if (!is.na(primary_label) && nzchar(primary_label) && primary_label %in% names(label_to_flag)) {
          return(unname(label_to_flag[[primary_label]]))
        }

        raw_flags <- .cluster_evidence_synopsis_parse_species_csv(
          row$matched_raw_flags %||% NA_character_
        )
        if (length(raw_flags)) {
          return(raw_flags[[1L]])
        }

        labels <- .cluster_evidence_synopsis_parse_species_csv(
          row$matched_labels %||% NA_character_
        )
        labels <- labels[labels %in% names(label_to_flag)]
        if (length(labels)) {
          return(unname(label_to_flag[[labels[[1L]]]]))
        }

        NA_character_
      }, character(1))

      return(data.frame(
        species = as.character(matched_tbl$species),
        assignment_state = as.character(matched_tbl$assignment_state),
        life_form_count = suppressWarnings(as.integer(matched_tbl$life_form_count %||% NA_integer_)),
        primary_raw_flag = primary_flags,
        stringsAsFactors = FALSE
      ))
    }
  }

  life_form_summary <- x$summaries$life_form_summary %||% NULL
  if (!is.data.frame(life_form_summary) || !nrow(life_form_summary) ||
      !all(c("label", "matched_species") %in% names(life_form_summary))) {
    return(data.frame(
      species = character(0),
      assignment_state = character(0),
      life_form_count = integer(0),
      primary_raw_flag = character(0),
      stringsAsFactors = FALSE
    ))
  }

  species_map <- list()
  for (i in seq_len(nrow(life_form_summary))) {
    label <- trimws(as.character(life_form_summary$label[[i]]))
    if (!nzchar(label) || !label %in% names(label_to_flag)) {
      next
    }

    species_list <- .cluster_evidence_synopsis_parse_species_csv(
      life_form_summary$matched_species[[i]]
    )
    if (!length(species_list)) {
      next
    }

    for (species in species_list) {
      if (!nzchar(species)) {
        next
      }
      species_map[[species]] <- unique(c(species_map[[species]], label_to_flag[[label]]))
    }
  }

  if (!length(species_map)) {
    return(data.frame(
      species = character(0),
      assignment_state = character(0),
      life_form_count = integer(0),
      primary_raw_flag = character(0),
      stringsAsFactors = FALSE
    ))
  }

  species_names <- sort(names(species_map))
  rows <- lapply(species_names, function(species_name) {
    flags <- species_map[[species_name]]
    flags <- flags[!is.na(flags) & nzchar(flags)]
    if (!length(flags)) {
      return(NULL)
    }

    flag_order <- suppressWarnings(as.integer(flag_priority[flags]))
    ord <- order(flag_order, flags, na.last = TRUE)
    flags <- flags[ord]

    data.frame(
      species = species_name,
      assignment_state = if (length(flags) > 1L) "mixed" else "fixed",
      life_form_count = length(flags),
      primary_raw_flag = flags[[1L]],
      stringsAsFactors = FALSE
    )
  })

  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) {
    return(data.frame(
      species = character(0),
      assignment_state = character(0),
      life_form_count = integer(0),
      primary_raw_flag = character(0),
      stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.cluster_evidence_synopsis_life_form_group_summary <- function(x) {
  assignments <- .cluster_evidence_synopsis_life_form_assignments(x)
  if (!is.data.frame(assignments) || !nrow(assignments)) {
    return(data.frame(
      group_id = character(0),
      group_label = character(0),
      descriptor = character(0),
      interpretation = character(0),
      interpretation_source = character(0),
      count = integer(0),
      share = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  assignments <- assignments[
    !is.na(assignments$primary_raw_flag) & nzchar(assignments$primary_raw_flag),
    ,
    drop = FALSE
  ]
  if (!nrow(assignments)) {
    return(data.frame(
      group_id = character(0),
      group_label = character(0),
      descriptor = character(0),
      interpretation = character(0),
      interpretation_source = character(0),
      count = integer(0),
      share = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  groups <- .cluster_evidence_synopsis_life_form_structural_groups()
  assignments <- merge(
    assignments,
    groups,
    by.x = "primary_raw_flag",
    by.y = "raw_flag",
    all.x = FALSE,
    all.y = FALSE,
    sort = FALSE
  )
  if (!nrow(assignments)) {
    return(data.frame(
      group_id = character(0),
      group_label = character(0),
      descriptor = character(0),
      interpretation = character(0),
      interpretation_source = character(0),
      count = integer(0),
      share = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  matched_total <- nrow(assignments)
  split_groups <- split(assignments, assignments$group_id, drop = TRUE)
  out <- do.call(rbind, lapply(split_groups, function(tbl) {
    data.frame(
      group_id = tbl$group_id[[1L]],
      group_label = tbl$group_label[[1L]],
      descriptor = tbl$descriptor[[1L]],
      interpretation = tbl$interpretation[[1L]],
      interpretation_source = tbl$interpretation_source[[1L]],
      order = suppressWarnings(as.integer(tbl$order[[1L]])),
      count = nrow(tbl),
      share = nrow(tbl) / matched_total,
      stringsAsFactors = FALSE
    )
  }))

  out <- out[order(-out$count, out$order, out$group_label), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.cluster_evidence_synopsis_life_form_glossary_items <- function(x) {
  labels <- .cluster_evidence_synopsis_used_life_form_labels(x)
  if (!length(labels)) {
    return(character(0))
  }

  dictionary <- .read_life_form_glossary_dictionary()
  dictionary <- dictionary[dictionary$label %in% labels, , drop = FALSE]
  if (!nrow(dictionary)) {
    return(character(0))
  }

  dictionary$order <- match(dictionary$label, labels)
  dictionary <- dictionary[order(dictionary$order, dictionary$label), , drop = FALSE]

  vapply(seq_len(nrow(dictionary)), function(i) {
    paste0(
      dictionary$label[[i]],
      ": ",
      dictionary$short_definition[[i]],
      " ",
      dictionary$interpretation_hint[[i]]
    )
  }, character(1))
}

.cluster_evidence_synopsis_life_form_coverage <- function(x) {
  topo <- x$summaries$species_topological %||% NULL
  denominator <- if (is.data.frame(topo) && nrow(topo)) {
    nrow(topo)
  } else {
    as.integer(x$context$cluster_metrics$k %||% 0L)
  }

  overlay_species <- x$summaries$life_form_overlay_species %||% NULL
  if (is.data.frame(overlay_species) && nrow(overlay_species)) {
    matched <- overlay_species$assignment_state %||% character(0)
    return(list(
      matched = sum(matched != "unmatched", na.rm = TRUE),
      total = denominator
    ))
  }

  life_form_summary <- x$summaries$life_form_summary %||% NULL
  matched_species <- character(0)
  if (is.data.frame(life_form_summary) && nrow(life_form_summary) &&
      "matched_species" %in% names(life_form_summary)) {
    matched_species <- unique(unlist(lapply(
      as.character(life_form_summary$matched_species),
      .cluster_evidence_synopsis_parse_species_csv
    ), use.names = FALSE))
  }

  list(
    matched = length(matched_species),
    total = denominator
  )
}

.cluster_evidence_synopsis_coverage_items <- function(x) {
  items <- character(0)
  topo <- x$summaries$species_topological %||% NULL
  denominator <- if (is.data.frame(topo) && nrow(topo)) {
    nrow(topo)
  } else {
    as.integer(x$context$cluster_metrics$k %||% 0L)
  }

  species_axis_values <- x$summaries$species_axis_values %||% NULL
  if (is.data.frame(species_axis_values) && nrow(species_axis_values) && denominator > 0L) {
    items <- c(
      items,
      paste0(
        "semantic coverage: ",
        nrow(species_axis_values),
        "/",
        denominator,
        " species scored"
      )
    )
  }

  life_form_cov <- .cluster_evidence_synopsis_life_form_coverage(x)
  if (life_form_cov$total > 0L && life_form_cov$matched > 0L) {
    items <- c(
      items,
      paste0(
        "life-form coverage: ",
        life_form_cov$matched,
        "/",
        life_form_cov$total,
        " species matched"
      )
    )
  }

  items
}

.cluster_evidence_synopsis_conflict_items <- function(x) {
  items <- character(0)
  quantity_context <- x$context$quantity_context %||% list()
  n_member_plots <- as.integer(quantity_context$cluster_plot_count %||% NA_integer_)
  if (is.finite(n_member_plots) && n_member_plots <= 5L) {
    items <- c(items, "small cluster: few member plots can make the signal unstable")
  }

  semantic_axes <- x$summaries$semantic_axes %||% NULL
  if (is.data.frame(semantic_axes) && nrow(semantic_axes) >= 2L) {
    scores <- suppressWarnings(as.numeric(semantic_axes$score_0_10))
    if (sum(is.finite(scores)) >= 2L && (max(scores, na.rm = TRUE) - min(scores, na.rm = TRUE)) >= 4) {
      items <- c(items, "mixed ecological signal: axis scores span contrasting conditions")
    }
  }

  overlay_diagnosis <- x$summaries$life_form_overlay_diagnosis %||% NULL
  if (is.data.frame(overlay_diagnosis) && nrow(overlay_diagnosis)) {
    phrase <- tolower(paste(overlay_diagnosis$phrase, collapse = " "))
    if (grepl("mixed|heterogeneous|compression", phrase)) {
      group_summary <- .cluster_evidence_synopsis_life_form_group_summary(x)
      dominant_label <- if (is.data.frame(group_summary) && nrow(group_summary)) {
        group_summary$group_label[[1L]]
      } else {
        NULL
      }
      items <- c(
        items,
        paste0(
          "mixed structural signal: some species have non-uniform structural assignments",
          if (!is.null(dominant_label)) {
            paste0(", although ", dominant_label, " remains the strongest structural signal")
          } else {
            ""
          }
        )
      )
    }
  }

  unique(items)[seq_len(min(length(unique(items)), 4L))]
}

.cluster_evidence_synopsis_life_form_metric_row <- function(x, metric_name) {
  overlay_metrics <- x$summaries$life_form_overlay_metrics %||% NULL
  if (!is.data.frame(overlay_metrics) || !nrow(overlay_metrics)) {
    return(NULL)
  }
  row <- overlay_metrics[overlay_metrics$metric == metric_name, , drop = FALSE]
  if (!nrow(row)) {
    return(NULL)
  }
  row[1L, , drop = FALSE]
}

.cluster_evidence_synopsis_life_form_summary_items <- function(x) {
  group_summary <- .cluster_evidence_synopsis_life_form_group_summary(x)
  if (!is.data.frame(group_summary) || !nrow(group_summary)) {
    return(character(0))
  }

  dominant_row <- group_summary[1L, , drop = FALSE]
  dominant_share <- suppressWarnings(as.numeric(dominant_row$share[[1L]]))

  items <- c(
    paste0(
      "Dominant structure: ",
      dominant_row$group_label[[1L]],
      if (is.finite(dominant_share)) {
        paste0(
          " (",
          .cluster_evidence_format_percent(100 * dominant_share, digits = 1L),
          "% of matched species)"
        )
      } else {
        ""
      },
      ". ",
      tools::toTitleCase(dominant_row$group_label[[1L]]),
      " here are mainly ",
      dominant_row$descriptor[[1L]],
      ". ",
      dominant_row$interpretation[[1L]]
    )
  )

  if (nrow(group_summary) >= 2L) {
    secondary_row <- group_summary[2L, , drop = FALSE]
    items <- c(
      items,
      paste0(
        "Secondary structure: ",
        secondary_row$group_label[[1L]],
        " (",
        .cluster_evidence_format_percent(100 * secondary_row$share[[1L]], digits = 1L),
        "% of matched species). The structural pattern is not fully uniform."
      )
    )
  }

  fixed_row <- .cluster_evidence_synopsis_life_form_metric_row(
    x,
    "fixed_assignment_share"
  )
  mixed_row <- .cluster_evidence_synopsis_life_form_metric_row(
    x,
    "mixed_assignment_share"
  )
  if (!is.null(fixed_row) || !is.null(mixed_row)) {
    fixed_value <- if (!is.null(fixed_row)) {
      suppressWarnings(as.numeric(fixed_row$value[[1L]]))
    } else {
      NA_real_
    }
    mixed_value <- if (!is.null(mixed_row)) {
      suppressWarnings(as.numeric(mixed_row$value[[1L]]))
    } else {
      NA_real_
    }

    clarity_text <- NULL
    if (is.finite(fixed_value) && is.finite(mixed_value)) {
      clarity_text <- paste0(
        .cluster_evidence_format_percent(100 * fixed_value, digits = 1L),
        "% of the selected species resolve to one structural type, while ",
        .cluster_evidence_format_percent(100 * mixed_value, digits = 1L),
        "% still have multiple possible structural assignments."
      )
    } else if (is.finite(fixed_value)) {
      clarity_text <- paste0(
        .cluster_evidence_format_percent(100 * fixed_value, digits = 1L),
        "% of the selected species resolve to one structural type."
      )
    } else if (is.finite(mixed_value)) {
      clarity_text <- paste0(
        .cluster_evidence_format_percent(100 * mixed_value, digits = 1L),
        "% of the selected species still have multiple possible structural assignments."
      )
    }

    if (is.finite(mixed_value) && mixed_value >= 0.50) {
      clarity_text <- paste0(
        clarity_text,
        " Treat the life-form block as supporting context and keep ecology primary."
      )
    } else if (is.finite(fixed_value) && fixed_value >= 0.75) {
      clarity_text <- paste0(
        clarity_text,
        " The structural signal is comparatively steady."
      )
    } else if (is.finite(mixed_value) && mixed_value > 0) {
      clarity_text <- paste0(
        clarity_text,
        " The structural signal is useful, but not fully stable."
      )
    }

    if (!is.null(clarity_text) && nzchar(clarity_text)) {
      items <- c(items, paste0("Assignment clarity: ", clarity_text))
    }
  }

  unmatched_row <- .cluster_evidence_synopsis_life_form_metric_row(
    x,
    "unmatched_species_share"
  )
  if (!is.null(unmatched_row)) {
    unmatched_value <- suppressWarnings(as.numeric(unmatched_row$value[[1L]]))
    items <- c(
      items,
      paste0(
        "Coverage caution: ",
        .cluster_evidence_format_percent(100 * unmatched_value, digits = 1L),
        "% of the selected species still have no life-form match."
      )
    )
  }

  compression_row <- .cluster_evidence_synopsis_life_form_metric_row(
    x,
    "species_to_life_form_compression"
  )
  if (!is.null(compression_row)) {
    compression_value <- suppressWarnings(as.numeric(compression_row$value[[1L]]))
    items <- c(
      items,
      paste0(
        "Compression caution: ",
        if (is.finite(compression_value) && compression_value >= 2) {
          "many species collapse into a smaller set of repeated structural groups"
        } else {
          "some species collapse into repeated structural groups"
        },
        ", so keep ecology and species names primary."
      )
    )
  }

  items
}

.cluster_evidence_synopsis_species_items <- function(
    x,
    represented_species = 7L,
    dictionary = NULL
) {
  represented_species <- .normalize_cluster_label_represented_species(
    represented_species,
    "represented_species"
  )
  if (identical(represented_species, 0L)) {
    return(character(0))
  }

  selected <- .cluster_evidence_llm_selected_species_info(x)
  if (!nrow(selected)) {
    return(character(0))
  }
  selected <- utils::head(selected, represented_species)

  cover_summary <- .cluster_evidence_llm_cover_summary_table(x)
  species_axis_values <- .cluster_evidence_llm_species_axis_values_table(x)
  overlay_species <- x$summaries$life_form_overlay_species %||% NULL
  if (is.data.frame(overlay_species) && nrow(overlay_species)) {
    overlay_species <- as.data.frame(overlay_species, stringsAsFactors = FALSE)
  } else {
    overlay_species <- NULL
  }

  vapply(seq_len(nrow(selected)), function(i) {
    species <- selected$species[[i]]
    via <- selected$selected_via[[i]]
    cover_row <- .cluster_evidence_llm_cover_row(species, cover_summary)
    axis_row <- .cluster_evidence_llm_species_axis_row(species, species_axis_values)
    phi_value <- NA_real_
    freq_value <- NA_real_
    cover_value <- NA_real_

    if (!is.null(cover_row)) {
      phi_tbl <- .cluster_evidence_llm_phi_table(x)
      idx_phi <- match(species, phi_tbl$species)
      if (is.finite(idx_phi)) {
        phi_value <- phi_tbl$phi[[idx_phi]]
      }
      freq_value <- suppressWarnings(as.numeric(cover_row$species_freq_pct %||% NA_real_))
      cover_value <- suppressWarnings(as.numeric(
        cover_row$mean_plot_cover_share_pct %||% cover_row$mean_cover %||% NA_real_
      ))
    }

    axis_items <- character(0)
    for (axis in c("l", "m", "n", "r", "t", "s")) {
      axis_item <- .cluster_evidence_synopsis_axis_item(
        axis = axis,
        value = axis_row[[axis]] %||% NA_real_,
        dictionary = dictionary
      )
      if (!is.null(axis_item)) {
        axis_items <- c(axis_items, axis_item)
      }
    }
    axis_items <- utils::head(axis_items, 3L)

    parts <- c(
      species,
      paste0("rank=", via),
      if (is.finite(phi_value)) paste0("phi=", .cluster_evidence_format_decimal(phi_value, digits = 2L)),
      if (is.finite(freq_value)) paste0("freq=", .cluster_evidence_format_percent(freq_value), "%"),
      if (is.finite(cover_value)) paste0("cover=", .cluster_evidence_format_percent(cover_value), "%"),
      if (length(axis_items)) paste0("axes=", paste(axis_items, collapse = "; "))
    )

    paste(parts[!is.na(parts) & nzchar(parts)], collapse = " | ")
  }, character(1))
}

.cluster_evidence_synopsis_reference_items <- function(
    x,
    semantic_items,
    life_form_items,
    species_items,
    coverage_items,
    conflict_items
) {
  quantity_context <- x$context$quantity_context %||% list()
  items <- c(
    "structural signal: how clearly the cluster shows a readable internal pattern rather than a loose mixture; weaker structure means labels should stay broader and more cautious",
    "structural dominance: one species group, ecological tendency, or life form clearly outweighs others; use it only when the summary and represented species agree",
    "h: cluster cohesion signal on a 0-1 scale; lower values mean looser shared-species structure and higher values mean tighter structure; use it comparatively inside this dataset",
    paste0(
      "k: cluster-core species count; read it as `k=current/",
      as.integer(quantity_context$dataset_species_total %||% NA_integer_),
      " species` where the denominator is the full recorded species pool in this dataset"
    ),
    "m: minimum core species needed for plot membership; read it as `m=current/k core species`, where higher values mean stricter membership",
    paste0(
      "n: member-plot count; read it as `n=current/",
      as.integer(quantity_context$dataset_plot_total %||% NA_integer_),
      " plots` where the denominator is the full plot count in this dataset"
    ),
    "dataset share: species share is the percent of all recorded species contained in this cluster core, and plot share is the percent of all plots assigned to this cluster; higher values mean broader spread within this dataset"
  )

  if (length(semantic_items)) {
    items <- c(
      items,
      "ecological axes: values are shown on a 0-10 scale and should be read through the attached verbal interpretation, for example `Light=high (8.04/10)`"
    )
  }

  if (length(species_items)) {
    items <- c(
      items,
      "species rank = hybrid: represented species are ordered first by stronger cluster distinctiveness (`phi`), then by stronger structural importance (`cover`), then by frequent recurrence",
      "phi: species-cluster association strength; higher phi means the species is more distinctive for this cluster relative to others",
      "freq: percent of member plots containing the species; higher frequency means broader recurrence inside the cluster",
      "cover: mean cover contribution of the species within member plots; higher cover means stronger structural prominence"
    )
  }

  if (length(coverage_items)) {
    items <- c(
      items,
      "coverage lines: `X/Y` means matched or scored species out of the cluster core; lower coverage means more caution is needed"
    )
  }

  if (length(conflict_items)) {
    items <- c(
      items,
      "conflict flags: bounded warnings about mixed or weak structure; treat them as caution, not as the main label"
    )
  }

  if (length(life_form_items)) {
    items <- c(
      items,
      "life-form summary: a supporting plain-language structural summary built from matched species growth-form evidence; use it only when it agrees with ecology and represented species",
      "assignment clarity: higher single-type share means the structural summary is steadier, while higher multi-type or unmatched share means it should stay secondary"
    )
  }

  items
}

.cluster_evidence_synopsis_prompt_blocks <- function(x, represented_species = 7L) {
  dictionary <- .read_cluster_evidence_llm_axis_dictionary()
  quantity_context <- x$context$quantity_context %||% list()
  cluster_metrics <- x$context$cluster_metrics %||% list()
  dataset_species_total <- as.integer(
    quantity_context$dataset_species_total %||% NA_integer_
  )
  dataset_plot_total <- as.integer(
    quantity_context$dataset_plot_total %||% NA_integer_
  )
  cluster_core_count <- as.integer(cluster_metrics$k %||% NA_integer_)
  cluster_plot_count <- as.integer(
    quantity_context$cluster_plot_count %||% NA_integer_
  )

  base_items <- c(
    paste0(
      "cluster metrics: h=",
      .cluster_evidence_format_decimal(cluster_metrics$h %||% NA_real_, digits = 3L),
      "/1.0",
      ", k=",
      cluster_core_count,
      "/",
      dataset_species_total,
      " species",
      ", m=",
      as.integer(cluster_metrics$m %||% NA_integer_),
      "/",
      cluster_core_count,
      " core species",
      ", n=",
      cluster_plot_count,
      "/",
      dataset_plot_total,
      " plots"
    ),
    paste0(
      "dataset share: species=",
      .cluster_evidence_format_percent(quantity_context$cluster_species_share_pct %||% NA_real_),
      "%, plots=",
      .cluster_evidence_format_percent(quantity_context$cluster_plot_share_pct %||% NA_real_),
      "%"
    )
  )

  semantic_items <- .cluster_evidence_synopsis_semantic_summary_items(
    x,
    dictionary = dictionary
  )
  life_form_items <- .cluster_evidence_synopsis_life_form_summary_items(x)
  species_items <- .cluster_evidence_synopsis_species_items(
    x,
    represented_species = represented_species,
    dictionary = dictionary
  )
  coverage_items <- .cluster_evidence_synopsis_coverage_items(x)
  conflict_items <- .cluster_evidence_synopsis_conflict_items(x)
  reference_items <- .cluster_evidence_synopsis_reference_items(
    x = x,
    semantic_items = semantic_items,
    life_form_items = life_form_items,
    species_items = species_items,
    coverage_items = coverage_items,
    conflict_items = conflict_items
  )
  user_added_lines <- .cluster_evidence_user_added_prompt_lines(
    x$user_added_data %||% NULL
  )

  list(
    .new_cluster_evidence_prompt_bulleted_block(
      id = "reference",
      label = "Reference",
      display_order = 10L,
      retain_rank = 10L,
      header = "Reference:",
      items = reference_items
    ),
    .new_cluster_evidence_prompt_bulleted_block(
      id = "base_summary",
      label = "Cluster summary",
      display_order = 20L,
      retain_rank = 20L,
      header = "Cluster summary:",
      items = base_items
    ),
    .new_cluster_evidence_prompt_bulleted_block(
      id = "semantic_summary",
      label = "Ecological summary",
      display_order = 30L,
      retain_rank = 30L,
      header = "Ecological summary:",
      items = semantic_items
    ),
    .new_cluster_evidence_prompt_bulleted_block(
      id = "life_form_summary",
      label = "Life-form summary",
      display_order = 40L,
      retain_rank = 40L,
      header = "Life-form summary:",
      items = life_form_items
    ),
    .new_cluster_evidence_prompt_bulleted_block(
      id = "species_detail",
      label = "Represented species",
      display_order = 50L,
      retain_rank = 50L,
      header = "Represented species:",
      items = species_items
    ),
    .new_cluster_evidence_prompt_bulleted_block(
      id = "coverage",
      label = "Coverage",
      display_order = 60L,
      retain_rank = 60L,
      header = "Coverage:",
      items = coverage_items
    ),
    .new_cluster_evidence_prompt_bulleted_block(
      id = "conflicts",
      label = "Conflicts",
      display_order = 70L,
      retain_rank = 70L,
      header = "Conflicts:",
      items = conflict_items
    ),
    .new_cluster_evidence_prompt_fixed_block(
      id = "user_added_data",
      label = "User-added data",
      display_order = 80L,
      retain_rank = 80L,
      lines = user_added_lines
    )
  )
}

.serialize_cluster_evidence_synopsis_prompt <- function(
    x,
    represented_species = 7L
) {
  if (!inherits(x, "cluster_evidence")) {
    stop("`x` must inherit from class `cluster_evidence`.")
  }

  represented_species <- .normalize_cluster_label_represented_species(
    represented_species,
    "represented_species"
  )
  rendered <- .serialize_cluster_evidence_blocks(
    .cluster_evidence_synopsis_prompt_blocks(
      x,
      represented_species = represented_species
    ),
    max_chars = NULL
  )

  rendered$text <- .cluster_evidence_llm_strip_evidence_ids(rendered$text)
  rendered$full_text <- .cluster_evidence_llm_strip_evidence_ids(
    rendered$full_text
  )
  rendered$chars_full <- .cluster_evidence_prompt_char_count(rendered$full_text)
  rendered$chars_used <- .cluster_evidence_prompt_char_count(rendered$text)
  rendered$trimmed <- FALSE
  rendered$prompt_visible_species_cap <- NULL
  rendered$represented_species <- as.integer(represented_species)

  rendered
}
