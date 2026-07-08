# Internal helpers for quantity context in model-facing cluster evidence.
#
# The goal is to make count-based and scale-based quantities explicit before
# they reach prompt rendering. The helper layer computes denominators,
# percentages, and cover-scale metadata without adding habitat interpretation.

.cluster_evidence_percent <- function(numerator, denominator) {
  numerator <- suppressWarnings(as.numeric(numerator))
  denominator <- suppressWarnings(as.numeric(denominator))

  if (!is.finite(numerator) || !is.finite(denominator) || denominator <= 0) {
    return(NA_real_)
  }

  100 * numerator / denominator
}

.cluster_evidence_format_percent <- function(x, digits = 1L) {
  x <- suppressWarnings(as.numeric(x))
  digits <- max(0L, as.integer(digits %||% 0L))

  if (!is.finite(x)) {
    return("NA")
  }

  text <- formatC(x, digits = digits, format = "f")
  sub("\\.?0+$", "", text)
}

.cluster_evidence_format_decimal <- function(x, digits = 2L) {
  x <- suppressWarnings(as.numeric(x))
  digits <- max(0L, as.integer(digits %||% 0L))

  if (!is.finite(x)) {
    return("NA")
  }

  formatC(x, digits = digits, format = "f")
}

.cluster_evidence_detect_cover_scale <- function(vegmatrix = NULL) {
  unknown <- list(
    cover_scale_type = "numeric_unknown_scale",
    cover_scale_label = "original numeric cover scale",
    cover_scale_bounds = c(lower = NA_real_, upper = NA_real_),
    cover_scale_min = NA_real_,
    cover_scale_max = NA_real_
  )

  if (is.null(vegmatrix)) {
    return(unknown)
  }

  values <- suppressWarnings(as.numeric(vegmatrix))
  values <- values[is.finite(values)]
  if (!length(values)) {
    return(unknown)
  }

  tol <- sqrt(.Machine$double.eps)
  min_value <- min(values)
  max_value <- max(values)
  unique_values <- sort(unique(values))
  all_integer_like <- all(abs(unique_values - round(unique_values)) <= tol)
  unique_count <- length(unique_values)

  if (all_integer_like && unique_count <= 11L && max_value <= 10) {
    return(list(
      cover_scale_type = "ordinal_numeric_cover",
      cover_scale_label = paste0(
        "original ordinal numeric cover scale (",
        .cluster_evidence_format_decimal(min_value, digits = 0L),
        "-",
        .cluster_evidence_format_decimal(max_value, digits = 0L),
        ")"
      ),
      cover_scale_bounds = c(lower = min_value, upper = max_value),
      cover_scale_min = min_value,
      cover_scale_max = max_value
    ))
  }

  if (min_value >= 0 && max_value <= 100) {
    return(list(
      cover_scale_type = "percentage_cover",
      cover_scale_label = "original percentage-cover scale",
      cover_scale_bounds = c(lower = 0, upper = 100),
      cover_scale_min = 0,
      cover_scale_max = 100
    ))
  }

  if (all_integer_like && unique_count <= 11L) {
    return(list(
      cover_scale_type = "ordinal_numeric_cover",
      cover_scale_label = paste0(
        "original ordinal numeric cover scale (",
        .cluster_evidence_format_decimal(min_value, digits = 0L),
        "-",
        .cluster_evidence_format_decimal(max_value, digits = 0L),
        ")"
      ),
      cover_scale_bounds = c(lower = min_value, upper = max_value),
      cover_scale_min = min_value,
      cover_scale_max = max_value
    ))
  }

  list(
    cover_scale_type = "numeric_unknown_scale",
    cover_scale_label = paste0(
      "original numeric cover scale (observed ",
      .cluster_evidence_format_decimal(min_value, digits = 2L),
      "-",
      .cluster_evidence_format_decimal(max_value, digits = 2L),
      ")"
    ),
    cover_scale_bounds = c(lower = min_value, upper = max_value),
    cover_scale_min = min_value,
    cover_scale_max = max_value
  )
}

.cluster_evidence_build_quantity_context <- function(
    source,
    cluster_metrics,
    plots_membership,
    vegmatrix = NULL
) {
  if (!.cluster_evidence_is_cocktail_object(source)) {
    stop("`source` must be a Cocktail object.", call. = FALSE)
  }

  dataset_species_total <- as.integer(ncol(source$Cluster.species))
  dataset_plot_total <- if (inherits(source$Plot.cluster, "Matrix")) {
    as.integer(nrow(source$Plot.cluster))
  } else {
    as.integer(nrow(as.matrix(source$Plot.cluster)))
  }
  dataset_cluster_total <- if (is.matrix(source$Cluster.info)) {
    as.integer(nrow(source$Cluster.info))
  } else {
    as.integer(nrow(source$Cluster.species))
  }

  cluster_species_count <- as.integer(cluster_metrics$k %||% NA_integer_)
  cluster_membership_threshold <- as.integer(cluster_metrics$m %||% NA_integer_)
  cluster_plot_count <- as.integer(
    plots_membership$n_member_plots %||% length(plots_membership$plot_ids %||% character(0))
  )

  cover_scale <- .cluster_evidence_detect_cover_scale(vegmatrix)

  c(
    list(
      dataset_species_total = dataset_species_total,
      dataset_plot_total = dataset_plot_total,
      dataset_cluster_total = dataset_cluster_total,
      cluster_species_count = cluster_species_count,
      cluster_membership_threshold = cluster_membership_threshold,
      cluster_plot_count = cluster_plot_count,
      cluster_species_share_pct = .cluster_evidence_percent(
        cluster_species_count,
        dataset_species_total
      ),
      cluster_plot_share_pct = .cluster_evidence_percent(
        cluster_plot_count,
        dataset_plot_total
      )
    ),
    cover_scale
  )
}

.cluster_evidence_add_cover_quantity_context <- function(
    cover_summary,
    member_plot_ids,
    vegmatrix,
    quantity_context
) {
  if (is.null(cover_summary) || !nrow(cover_summary)) {
    return(cover_summary)
  }

  out <- cover_summary
  n_member_plots <- as.integer(quantity_context$cluster_plot_count %||% 0L)
  species_names <- as.character(out$species)
  counts <- rep(NA_integer_, length(species_names))
  names(counts) <- species_names
  mean_plot_cover_share_pct <- rep(NA_real_, length(species_names))
  names(mean_plot_cover_share_pct) <- species_names

  if (!is.null(vegmatrix) &&
      length(member_plot_ids) > 0L &&
      all(member_plot_ids %in% rownames(vegmatrix))) {
    vm_member <- vegmatrix[member_plot_ids, , drop = FALSE]
    total_plot_cover <- rowSums(vm_member, na.rm = TRUE)
    matched_species <- intersect(species_names, colnames(vegmatrix))
    if (length(matched_species)) {
      vm_sub <- vm_member[, matched_species, drop = FALSE]
      raw_counts <- colSums(vm_sub > 0)
      counts[names(raw_counts)] <- as.integer(raw_counts)

      for (species_name in matched_species) {
        species_cover <- vm_sub[, species_name]
        cover_share <- ifelse(total_plot_cover > 0, species_cover / total_plot_cover, 0)
        mean_plot_cover_share_pct[[species_name]] <- 100 * mean(
          cover_share,
          na.rm = TRUE
        )
      }
    }
  }

  fallback_idx <- which(is.na(counts))
  if (length(fallback_idx)) {
    fallback_counts <- round(out$freq_in_member_plots[fallback_idx] * n_member_plots)
    counts[fallback_idx] <- as.integer(fallback_counts)
  }

  out$species_freq_count <- unname(counts[species_names])
  out$species_freq_pct <- if (n_member_plots > 0L) {
    100 * out$species_freq_count / n_member_plots
  } else {
    NA_real_
  }
  out$mean_plot_cover_share_pct <- unname(mean_plot_cover_share_pct[species_names])
  out$n_member_plots <- n_member_plots
  out$cover_scale_type <- rep(
    .as_scalar_character(quantity_context$cover_scale_type),
    nrow(out)
  )
  out$cover_scale_label <- rep(
    .as_scalar_character(quantity_context$cover_scale_label),
    nrow(out)
  )
  out$cover_scale_min <- rep(
    suppressWarnings(as.numeric(quantity_context$cover_scale_min)),
    nrow(out)
  )
  out$cover_scale_max <- rep(
    suppressWarnings(as.numeric(quantity_context$cover_scale_max)),
    nrow(out)
  )

  out
}

.cluster_evidence_llm_cover_sentence <- function(cover_row) {
  cover_scale_type <- .as_scalar_character(cover_row$cover_scale_type %||% NA_character_)
  n_member_plots <- suppressWarnings(as.integer(cover_row$n_member_plots %||% NA_integer_))
  mean_cover <- suppressWarnings(as.numeric(cover_row$mean_cover %||% NA_real_))
  mean_plot_cover_share_pct <- suppressWarnings(as.numeric(
    cover_row$mean_plot_cover_share_pct %||% NA_real_
  ))

  if (identical(cover_scale_type, "percentage_cover") && is.finite(mean_cover)) {
    return(paste0(
      "Mean cover ",
      .cluster_evidence_format_decimal(mean_cover, digits = 2L),
      "/100 on the original percentage-cover scale, averaged across all ",
      n_member_plots,
      " member plots including zeros where the species is absent."
    ))
  }

  if (is.finite(mean_plot_cover_share_pct)) {
    return(paste0(
      "Mean share of total plot cover ",
      .cluster_evidence_format_percent(mean_plot_cover_share_pct),
      "% across all ",
      n_member_plots,
      " member plots."
    ))
  }

  if (is.finite(mean_cover)) {
    return(paste0(
      "Raw mean cover on the ",
      .as_scalar_character(cover_row$cover_scale_label %||% "original input scale"),
      ": ",
      .cluster_evidence_format_decimal(mean_cover, digits = 2L),
      "."
    ))
  }

  "Cover context is unavailable."
}

.cluster_evidence_llm_cover_item <- function(cover_row) {
  species_name <- .as_scalar_character(cover_row$species %||% NA_character_)
  species_freq_count <- suppressWarnings(as.integer(cover_row$species_freq_count %||% NA_integer_))
  n_member_plots <- suppressWarnings(as.integer(cover_row$n_member_plots %||% NA_integer_))
  species_freq_pct <- suppressWarnings(as.numeric(cover_row$species_freq_pct %||% NA_real_))

  paste0(
    species_name,
    ": occurs in ",
    species_freq_count,
    " of ",
    n_member_plots,
    " member plots (",
    .cluster_evidence_format_percent(species_freq_pct),
    "%). ",
    .cluster_evidence_llm_cover_sentence(cover_row)
  )
}

.augment_cluster_evidence_with_quantity_context <- function(
    evidence,
    source,
    vegmatrix = NULL
) {
  if (!inherits(evidence, "cluster_evidence")) {
    stop("`evidence` must inherit from class `cluster_evidence`.", call. = FALSE)
  }

  quantity_context <- .cluster_evidence_build_quantity_context(
    source = source,
    cluster_metrics = evidence$context$cluster_metrics,
    plots_membership = evidence$summaries$plots_membership,
    vegmatrix = vegmatrix
  )

  evidence$context$quantity_context <- quantity_context
  evidence$summaries$cover_summary <- .cluster_evidence_add_cover_quantity_context(
    cover_summary = evidence$summaries$cover_summary,
    member_plot_ids = evidence$summaries$plots_membership$plot_ids %||% character(0),
    vegmatrix = vegmatrix,
    quantity_context = quantity_context
  )

  evidence
}

.cluster_evidence_llm_dataset_context_line <- function(x) {
  quantity_context <- x$context$quantity_context %||% NULL
  if (!is.list(quantity_context) || !length(quantity_context)) {
    stop("`x` does not contain `context$quantity_context`.", call. = FALSE)
  }

  paste0(
    "Dataset context: ",
    quantity_context$dataset_species_total,
    " recorded species, ",
    quantity_context$dataset_plot_total,
    " plots, ",
    quantity_context$dataset_cluster_total,
    " evaluated clusters."
  )
}

.cluster_evidence_llm_cluster_metric_items <- function(x) {
  quantity_context <- x$context$quantity_context %||% NULL
  if (!is.list(quantity_context) || !length(quantity_context)) {
    stop("`x` does not contain `context$quantity_context`.", call. = FALSE)
  }

  h <- suppressWarnings(as.numeric(x$context$cluster_metrics$h %||% NA_real_))
  k <- as.integer(x$context$cluster_metrics$k %||% NA_integer_)
  m <- as.integer(x$context$cluster_metrics$m %||% NA_integer_)
  n_member_plots <- as.integer(
    x$summaries$plots_membership$n_member_plots %||% NA_integer_
  )

  c(
    paste0(
      "h=",
      .cluster_evidence_format_decimal(h, digits = 3L),
      ": merge-phi value for this cluster. Lower values mean a looser shared species signal; higher values mean a tighter shared species signal."
    ),
    paste0(
      "k=", k,
      ": the cluster core contains ",
      k,
      " species out of ",
      quantity_context$dataset_species_total,
      " recorded species in the dataset (",
      .cluster_evidence_format_percent(quantity_context$cluster_species_share_pct),
      "% of all recorded species)."
    ),
    paste0(
      "m=", m,
      ": a plot must contain at least ",
      m,
      " of these ",
      k,
      " species to count as a member of the cluster."
    ),
    paste0(
      "n=", n_member_plots,
      ": this cluster contains ",
      n_member_plots,
      " member plots out of ",
      quantity_context$dataset_plot_total,
      " plots in the dataset (",
      .cluster_evidence_format_percent(quantity_context$cluster_plot_share_pct),
      "% of all plots)."
    )
  )
}
