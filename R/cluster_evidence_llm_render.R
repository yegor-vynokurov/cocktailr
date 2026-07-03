.cluster_evidence_llm_species_selection_policy <- function() {
  # Keep the model-facing species bundle bounded and deterministic:
  # start with phi-ranked species that also have cover context, then add
  # dominant species, then frequent species, then fill remaining slots by
  # dominant-cover order up to a fixed cap.
  list(
    top_phi_n = 6L,
    dominant_cover_n = 6L,
    frequent_cover_n = 6L,
    max_total_n = 12L
  )
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
    policy = NULL
) {
  policy <- policy %||% .cluster_evidence_llm_species_selection_policy()
  phi_summary <- .cluster_evidence_llm_phi_table(x)

  if (!nrow(phi_summary)) {
    return(character(0))
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
  topo_items <- .cluster_evidence_llm_topological_species_items(
    x,
    dictionary = axis_dictionary,
    selected_species = selected_species_info$species
  )
  phi_items <- .cluster_evidence_llm_phi_items(
    x,
    exclude_species = selected_species_info$species
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

  semantic_unmatched_items <- as.character(
    x$summaries$semantic_unmatched_species %||% character(0)
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

  rendered
}

.format_cluster_evidence_llm_prompt <- function(x, max_chars = NULL) {
  .serialize_cluster_evidence_llm_prompt(x, max_chars = max_chars)$text
}
