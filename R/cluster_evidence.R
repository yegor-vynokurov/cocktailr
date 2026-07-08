#' Build an evidence object for one Cocktail cluster
#'
#' @description
#' Collects deterministic, machine-readable evidence for a single Cocktail
#' cluster. The returned object is intended as the fact layer between a
#' \code{"cocktail"} object and downstream LLM-based labeling or interpretation.
#'
#' The function does not call an LLM. It extracts cluster metrics, species,
#' plot-membership summaries, basic topology, optional cover-based summaries,
#' and explicit limitations from a Cocktail object.
#'
#' @param x A \code{"cocktail"} object produced by \code{\link{cocktail_cluster}}.
#'   The function requires at least \code{Cluster.species}, \code{Cluster.info},
#'   \code{Cluster.height}, \code{Cluster.merged}, and \code{Plot.cluster}.
#'
#' @param cluster A single cluster identifier, either as a character label like
#'   \code{"c_37"} or as an integer node ID such as \code{37}.
#'
#' @param top_n_phi Optional integer >= 1. Maximum number of phi-ranked species
#'   to keep in \code{summaries$species_phi}. Default \code{10}.
#'
#' @param n_prototype_plots Optional integer >= 0. Maximum number of prototype
#'   plots to keep. Default \code{5}.
#'
#' @param n_borderline_plots Optional integer >= 0. Maximum number of borderline
#'   plots to keep. Default \code{5}.
#'
#' @param include_cover Logical; if \code{TRUE} (default) and \code{x$vegmatrix}
#'   is available, compute cover summaries for the cluster's topological species.
#' @param user_added_data Optional additional user-supplied material. Accepts
#'   \code{NULL} (default), an in-memory object, a path to one supported file,
#'   or a path to one directory scanned non-recursively for supported files.
#'   Supported file extensions are \code{.txt}, \code{.json}, \code{.yaml},
#'   and \code{.yml}. These inputs are included in the evidence object as raw
#'   \code{user_added_data} without domain-specific preprocessing.
#'
#' @return
#' A list of class \code{"cluster_evidence"} with top-level components:
#' \itemize{
#'   \item \code{meta}
#'   \item \code{context}
#'   \item \code{summaries}
#'   \item \code{evidence}
#'   \item \code{limitations}
#'   \item \code{future}
#' }
#'
#' The \code{evidence$items} component contains atomic fact records with
#' deterministic IDs such as \code{"E1"}, \code{"E2"}, ...
#'
#' @examples
#' syn <- generate_synthetic_vegetation_data(
#'   n_plots_per_community = 4,
#'   n_transition_plots = 2,
#'   seed = 42
#' )
#' res <- cocktail_cluster(
#'   vegmatrix = syn$wide_matrix,
#'   progress = FALSE,
#'   plot_values = "rel_cover",
#'   species_cluster_phi = TRUE,
#'   save_vegmatrix = TRUE
#' )
#' ev <- cluster_evidence(res, cluster = "c_1", top_n_phi = 3)
#' print(ev)
#'
#' @export
cluster_evidence <- function(
    x,
    cluster,
    top_n_phi = 10L,
    n_prototype_plots = 5L,
    n_borderline_plots = 5L,
    include_cover = TRUE,
    user_added_data = NULL
) {
  if (!.cluster_evidence_is_cocktail_object(x)) {
    stop("`x` must be a Cocktail object with Cluster.species, Cluster.info, Cluster.height, Cluster.merged, and Plot.cluster.")
  }

  top_n_phi <- .arg_positive_integer(top_n_phi, "top_n_phi")
  n_prototype_plots <- .arg_non_negative_integer(
    n_prototype_plots,
    "n_prototype_plots"
  )
  n_borderline_plots <- .arg_non_negative_integer(
    n_borderline_plots,
    "n_borderline_plots"
  )
  include_cover <- .arg_single_flag(include_cover, "include_cover")
  loaded_user_added_data <- .cluster_evidence_resolve_user_added_data(
    user_added_data
  )

  CS <- x$Cluster.species
  KI <- x$Cluster.info
  H <- x$Cluster.height
  CM <- x$Cluster.merged
  PC <- x$Plot.cluster

  if (!is.matrix(CS)) stop("`x$Cluster.species` must be a matrix.")
  if (!is.matrix(KI) || !all(c("k", "m") %in% colnames(KI))) {
    stop("`x$Cluster.info` must be a matrix with columns `k` and `m`.")
  }
  if (!is.matrix(CM) || ncol(CM) != 2L) {
    stop("`x$Cluster.merged` must be a two-column matrix.")
  }
  if (length(H) < nrow(CS)) {
    stop("`x$Cluster.height` must have length >= nrow(x$Cluster.species).")
  }

  n_nodes <- nrow(CS)
  cluster_ref <- .cluster_evidence_parse_cluster_id(cluster, n_nodes)
  cluster_id <- cluster_ref$id
  cluster_label <- cluster_ref$label

  if (!inherits(PC, "Matrix")) {
    PC <- Matrix::Matrix(as.matrix(PC), sparse = TRUE)
  }
  if (is.null(rownames(PC))) {
    if ("plots" %in% names(x) && !is.null(x$plots)) {
      rownames(PC) <- x$plots
    } else {
      rownames(PC) <- paste0("plot_", seq_len(nrow(PC)))
    }
  }

  plot_names <- rownames(PC)
  species_names <- colnames(CS)
  if (is.null(species_names) && "species" %in% names(x)) {
    species_names <- x$species
    colnames(CS) <- species_names
  }
  if (is.null(colnames(CS))) {
    stop("`x$Cluster.species` must have species column names, or `x$species` must be available.")
  }

  vm <- .cluster_evidence_align_vegmatrix(x, plot_names, colnames(CS))

  topo_species <- colnames(CS)[CS[cluster_id, ] > 0]
  topo_summary <- data.frame(
    species = topo_species,
    evidence_id = rep(NA_character_, length(topo_species)),
    stringsAsFactors = FALSE
  )

  phi_available <- "Species.cluster.phi" %in% names(x) && !is.null(x$Species.cluster.phi)
  phi_summary <- if (phi_available) {
    .cluster_evidence_phi_summary(x$Species.cluster.phi, cluster_id, top_n_phi)
  } else {
    NULL
  }

  member_values <- releves_in_clusters(
    x,
    clusters = cluster_label,
    values = TRUE,
    return = "vector"
  )
  member_values <- member_values[order(names(member_values))]

  plot_scores <- .cluster_evidence_score_member_plots(member_values, topo_species, vm)
  prototype_plots <- .cluster_evidence_pick_prototypes(plot_scores, n_prototype_plots)
  borderline_plots <- .cluster_evidence_pick_borderlines(
    plot_scores,
    n_borderline_plots,
    exclude_plots = prototype_plots$plot
  )

  cover_summary <- if (isTRUE(include_cover)) {
    .cluster_evidence_cover_summary(names(member_values), topo_species, vm)
  } else {
    NULL
  }

  parent_id <- .cluster_evidence_parent_id(CM, cluster_id)
  depth <- .cluster_evidence_depth(CM, cluster_id)
  children_df <- .cluster_evidence_children_info(
    CM,
    cluster_id,
    if ("species" %in% names(x)) x$species else colnames(CS)
  )
  if (!nrow(children_df)) {
    children_df <- data.frame(
      child_kind = character(0),
      child_id = integer(0),
      child_label = character(0),
      evidence_id = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    children_df$evidence_id <- NA_character_
  }

  plot_values_kind <- .cluster_evidence_plot_values_kind(PC)

  missing_components <- character(0)
  warnings <- character(0)
  unsupported_inferences <- c(
    "Cluster evidence alone does not prove habitat, region, or syntaxonomic interpretation.",
    "The MVP evidence object does not yet include sibling or nearest-cluster contrast."
  )

  if (!phi_available) {
    missing_components <- c(missing_components, "Species.cluster.phi")
    warnings <- c(
      warnings,
      "Phi-ranked species summary omitted because `x$Species.cluster.phi` is missing."
    )
  }

  if (is.null(vm)) {
    missing_components <- c(missing_components, "vegmatrix")
    warnings <- c(
      warnings,
      "Cover summaries are omitted because `x$vegmatrix` is missing.",
      "Prototype and borderline plot support scores use Plot.cluster membership values only because `x$vegmatrix` is missing."
    )
  }

  if (!isTRUE(include_cover)) {
    warnings <- c(
      warnings,
      "Cover summaries were skipped because `include_cover = FALSE`."
    )
  }

  if (!length(member_values)) {
    warnings <- c(
      warnings,
      "The selected cluster has no member plots with Plot.cluster > 0."
    )
  }

  evidence_items <- list()
  evidence_index <- list(
    cluster_metrics = character(0),
    topology = character(0),
    species_topological = character(0),
    species_phi = character(0),
    plots_membership = character(0),
    plots_prototype = character(0),
    plots_borderline = character(0),
    cover_summary = character(0),
    limitations = character(0)
  )
  next_eid <- 1L

  add_evidence <- function(category, type, label, value, source, support_level = "direct") {
    id <- paste0("E", next_eid)
    next_eid <<- next_eid + 1L

    evidence_items[[length(evidence_items) + 1L]] <<- list(
      id = id,
      type = type,
      label = label,
      value = value,
      source = source,
      support_level = support_level
    )
    evidence_index[[category]] <<- c(evidence_index[[category]], id)
    id
  }

  metric_ids <- c(
    h = add_evidence(
      "cluster_metrics",
      "cluster_metric",
      paste0("Merge phi for ", cluster_label),
      list(metric = "h", value = as.numeric(H[cluster_id])),
      "Cluster.height"
    ),
    k = add_evidence(
      "cluster_metrics",
      "cluster_metric",
      paste0("Cluster size k for ", cluster_label),
      list(metric = "k", value = as.integer(KI[cluster_id, "k"])),
      "Cluster.info"
    ),
    m = add_evidence(
      "cluster_metrics",
      "cluster_metric",
      paste0("Membership threshold m for ", cluster_label),
      list(metric = "m", value = as.integer(KI[cluster_id, "m"])),
      "Cluster.info"
    )
  )

  parent_info <- NULL
  if (!is.null(parent_id)) {
    parent_eid <- add_evidence(
      "topology",
      "topology",
      paste0("Parent cluster of ", cluster_label),
      list(cluster_id = paste0("c_", parent_id), cluster_num = parent_id),
      "Cluster.merged"
    )
    parent_info <- list(
      cluster_id = paste0("c_", parent_id),
      cluster_num = parent_id,
      h = as.numeric(H[parent_id]),
      evidence_id = parent_eid
    )
  }

  if (nrow(children_df)) {
    for (i in seq_len(nrow(children_df))) {
      children_df$evidence_id[i] <- add_evidence(
        "topology",
        "topology",
        paste0("Child ", children_df$child_label[i], " of ", cluster_label),
        as.list(children_df[i, c("child_kind", "child_id", "child_label"), drop = FALSE]),
        "Cluster.merged"
      )
    }
  }

  if (nrow(topo_summary)) {
    for (i in seq_len(nrow(topo_summary))) {
      topo_summary$evidence_id[i] <- add_evidence(
        "species_topological",
        "species_topological",
        paste0("Topological member species ", topo_summary$species[i], " in ", cluster_label),
        list(species = topo_summary$species[i]),
        "Cluster.species"
      )
    }
  }

  if (!is.null(phi_summary) && nrow(phi_summary)) {
    for (i in seq_len(nrow(phi_summary))) {
      phi_summary$evidence_id[i] <- add_evidence(
        "species_phi",
        "species_phi",
        paste0("Phi-ranked species ", phi_summary$species[i], " in ", cluster_label),
        list(species = phi_summary$species[i], phi = phi_summary$phi[i]),
        "Species.cluster.phi"
      )
    }
  }

  membership_eid <- add_evidence(
    "plots_membership",
    "plot_membership",
    paste0("Plot membership summary for ", cluster_label),
    list(
      n_member_plots = length(member_values),
      plot_ids = names(member_values),
      membership_values = as.numeric(member_values)
    ),
    "Plot.cluster"
  )

  if (nrow(prototype_plots)) {
    for (i in seq_len(nrow(prototype_plots))) {
      prototype_plots$evidence_id[i] <- add_evidence(
        "plots_prototype",
        "plot_prototype",
        paste0("Prototype plot ", prototype_plots$plot[i], " for ", cluster_label),
        as.list(prototype_plots[i, c("plot", "membership_value", "present_fraction",
                                     "cover_share", "support_score", "rationale"), drop = FALSE]),
        if (is.null(vm)) "Plot.cluster" else "Plot.cluster + vegmatrix"
      )
    }
  }

  if (nrow(borderline_plots)) {
    for (i in seq_len(nrow(borderline_plots))) {
      borderline_plots$evidence_id[i] <- add_evidence(
        "plots_borderline",
        "plot_borderline",
        paste0("Borderline plot ", borderline_plots$plot[i], " for ", cluster_label),
        as.list(borderline_plots[i, c("plot", "membership_value", "present_fraction",
                                      "cover_share", "support_score", "rationale"), drop = FALSE]),
        if (is.null(vm)) "Plot.cluster" else "Plot.cluster + vegmatrix"
      )
    }
  }

  if (!is.null(cover_summary) && nrow(cover_summary)) {
    for (i in seq_len(nrow(cover_summary))) {
      cover_value_fields <- intersect(
        c(
          "species", "mean_cover", "median_cover", "freq_in_member_plots",
          "species_freq_count", "species_freq_pct", "mean_plot_cover_share_pct",
          "n_member_plots", "cover_scale_type", "cover_scale_label",
          "cover_scale_min", "cover_scale_max"
        ),
        names(cover_summary)
      )
      cover_summary$evidence_id[i] <- add_evidence(
        "cover_summary",
        "cover_summary",
        paste0("Cover summary for species ", cover_summary$species[i], " in ", cluster_label),
        as.list(cover_summary[i, cover_value_fields, drop = FALSE]),
        "vegmatrix"
      )
    }
  }

  limitation_ids <- list(
    missing_components = character(0),
    warnings = character(0),
    unsupported_inferences = character(0)
  )

  if (length(missing_components)) {
    for (txt in missing_components) {
      limitation_ids$missing_components <- c(
        limitation_ids$missing_components,
        add_evidence(
          "limitations",
          "limitation",
          paste0("Missing component: ", txt),
          list(kind = "missing_component", text = txt),
          "derived",
          support_level = "limitation"
        )
      )
    }
  }

  if (length(warnings)) {
    for (txt in warnings) {
      limitation_ids$warnings <- c(
        limitation_ids$warnings,
        add_evidence(
          "limitations",
          "limitation",
          paste0("Evidence warning for ", cluster_label),
          list(kind = "warning", text = txt),
          "derived",
          support_level = "limitation"
        )
      )
    }
  }

  if (length(unsupported_inferences)) {
    for (txt in unsupported_inferences) {
      limitation_ids$unsupported_inferences <- c(
        limitation_ids$unsupported_inferences,
        add_evidence(
          "limitations",
          "limitation",
          paste0("Unsupported inference note for ", cluster_label),
          list(kind = "unsupported_inference", text = txt),
          "derived",
          support_level = "limitation"
        )
      )
    }
  }

  out <- list(
    meta = list(
      cluster_id = cluster_label,
      cluster_num = cluster_id,
      generated_at = NULL,
      user_added_data_present = !is.null(loaded_user_added_data),
      user_added_data_source_type = loaded_user_added_data$source_type %||% NULL,
      user_added_data_truncated = isTRUE(
        loaded_user_added_data$truncated %||% FALSE
      ),
      user_added_data_entry_count = length(
        loaded_user_added_data$entries %||% list()
      ),
      dataset = .cluster_evidence_dataset_info(x),
      source = list(
        object_class = "cocktail",
        has_species_cluster_phi = isTRUE(phi_available),
        has_vegmatrix = !is.null(vm),
        plot_values = plot_values_kind,
        input_format = x$input_format %||% NULL
      )
    ),
    context = list(
      cluster_metrics = list(
        h = as.numeric(H[cluster_id]),
        k = as.integer(KI[cluster_id, "k"]),
        m = as.integer(KI[cluster_id, "m"]),
        evidence_ids = metric_ids
      ),
      topology = list(
        parent = parent_info,
        children = children_df,
        depth = depth
      )
    ),
    summaries = list(
      species_topological = topo_summary,
      species_phi = phi_summary,
      plots_membership = list(
        n_member_plots = length(member_values),
        plot_ids = names(member_values),
        membership_values = as.numeric(member_values),
        evidence_id = membership_eid
      ),
      plots_prototype = prototype_plots,
      plots_borderline = borderline_plots,
      cover_summary = cover_summary
    ),
    evidence = list(
      items = stats::setNames(evidence_items, vapply(evidence_items, `[[`, character(1L), "id")),
      index = evidence_index
    ),
    limitations = list(
      missing_components = missing_components,
      warnings = warnings,
      unsupported_inferences = unsupported_inferences,
      evidence_ids = limitation_ids
    ),
    future = list(
      sibling_summary = NULL,
      nearest_clusters = NULL,
      ontology_slots = NULL,
      retrieval_provenance = NULL
    )
  )

  if (!is.null(loaded_user_added_data)) {
    out$user_added_data <- loaded_user_added_data
  }

  class(out) <- c("cluster_evidence", class(out))
  out <- .augment_cluster_evidence_with_quantity_context(
    evidence = out,
    source = x,
    vegmatrix = vm
  )
  out
}

.cluster_evidence_prompt_char_count <- function(text) {
  text <- .null_default(.as_scalar_character(text), "")
  if (!nzchar(text)) {
    return(0L)
  }

  as.integer(nchar(enc2utf8(text), type = "chars", allowNA = FALSE, keepNA = FALSE))
}

.new_cluster_evidence_prompt_fixed_block <- function(
    id,
    label,
    display_order,
    retain_rank,
    lines
) {
  lines <- as.character(lines %||% character(0))

  list(
    id = id,
    label = label,
    display_order = as.integer(display_order),
    retain_rank = as.integer(retain_rank),
    item_count_full = 0L,
    render = function(n_items = 0L) lines
  )
}

.new_cluster_evidence_prompt_inline_block <- function(
    id,
    label,
    display_order,
    retain_rank,
    header,
    items,
    joiner = "; ",
    intro_lines = NULL,
    empty_text = NULL
) {
  items <- as.character(items %||% character(0))
  intro_lines <- as.character(intro_lines %||% character(0))
  empty_text <- if (is.null(empty_text)) NULL else as.character(empty_text)

  list(
    id = id,
    label = label,
    display_order = as.integer(display_order),
    retain_rank = as.integer(retain_rank),
    item_count_full = as.integer(length(items)),
    render = function(n_items = length(items)) {
      lines <- intro_lines

      if (!length(items)) {
        if (is.null(empty_text)) {
          return(lines)
        }

        return(c(lines, paste0(header, empty_text)))
      }

      n_items <- max(0L, min(as.integer(n_items), length(items)))
      if (n_items < 1L) {
        return(character(0))
      }

      c(
        lines,
        paste0(header, paste(items[seq_len(n_items)], collapse = joiner))
      )
    }
  )
}

.new_cluster_evidence_prompt_bulleted_block <- function(
    id,
    label,
    display_order,
    retain_rank,
    header,
    items
) {
  items <- as.character(items %||% character(0))

  list(
    id = id,
    label = label,
    display_order = as.integer(display_order),
    retain_rank = as.integer(retain_rank),
    item_count_full = as.integer(length(items)),
    render = function(n_items = length(items)) {
      if (!length(items)) {
        return(character(0))
      }

      n_items <- max(0L, min(as.integer(n_items), length(items)))
      if (n_items < 1L) {
        return(character(0))
      }

      c(header, paste0("- ", items[seq_len(n_items)]))
    }
  )
}

.render_cluster_evidence_prompt_block_text <- function(block, n_items = NULL) {
  n_items <- n_items %||% block$item_count_full
  lines <- block$render(n_items)
  lines <- as.character(lines %||% character(0))
  lines <- lines[!is.na(lines) & nzchar(lines)]
  paste(lines, collapse = "\n")
}

.cluster_evidence_prompt_blocks <- function(x) {
  if (!inherits(x, "cluster_evidence")) {
    stop("`x` must inherit from class `cluster_evidence`.")
  }

  topo_items <- if (nrow(x$summaries$species_topological)) {
    paste0(
      x$summaries$species_topological$species,
      " [", x$summaries$species_topological$evidence_id, "]"
    )
  } else {
    character(0)
  }

  phi_items <- if (!is.null(x$summaries$species_phi) && nrow(x$summaries$species_phi)) {
    paste0(
      x$summaries$species_phi$species,
      " (phi=",
      formatC(x$summaries$species_phi$phi, digits = 3L, format = "f"),
      ") [",
      x$summaries$species_phi$evidence_id,
      "]"
    )
  } else {
    character(0)
  }

  prototype_items <- if (nrow(x$summaries$plots_prototype)) {
    paste0(
      x$summaries$plots_prototype$plot,
      " (score=",
      formatC(x$summaries$plots_prototype$support_score, digits = 3L, format = "f"),
      ") [",
      x$summaries$plots_prototype$evidence_id,
      "]"
    )
  } else {
    character(0)
  }

  borderline_items <- if (nrow(x$summaries$plots_borderline)) {
    paste0(
      x$summaries$plots_borderline$plot,
      " (score=",
      formatC(x$summaries$plots_borderline$support_score, digits = 3L, format = "f"),
      ") [",
      x$summaries$plots_borderline$evidence_id,
      "]"
    )
  } else {
    character(0)
  }

  cover_items <- if (!is.null(x$summaries$cover_summary) && nrow(x$summaries$cover_summary)) {
    paste0(
      x$summaries$cover_summary$species,
      " (mean_cover=",
      formatC(x$summaries$cover_summary$mean_cover, digits = 2L, format = "f"),
      ", freq=",
      formatC(x$summaries$cover_summary$freq_in_member_plots, digits = 2L, format = "f"),
      ") [",
      x$summaries$cover_summary$evidence_id,
      "]"
    )
  } else {
    character(0)
  }

  semantic_axis_items <- if (!is.null(x$summaries$semantic_axes) && nrow(x$summaries$semantic_axes)) {
    paste0(
      x$summaries$semantic_axes$axis_name,
      " [", x$summaries$semantic_axes$axis, "]",
      " (score=",
      formatC(x$summaries$semantic_axes$score_0_10, digits = 2L, format = "f"),
      "/10, band=", x$summaries$semantic_axes$band,
      ", coverage=",
      formatC(x$summaries$semantic_axes$coverage, digits = 2L, format = "f"),
      ", confidence=", x$summaries$semantic_axes$confidence_tier,
      ") [", x$summaries$semantic_axes$evidence_id, "]"
    )
  } else {
    character(0)
  }

  semantic_unmatched_items <- as.character(
    x$summaries$semantic_unmatched_species %||% character(0)
  )

  limitation_items <- as.character(c(
    x$limitations$warnings %||% character(0),
    x$limitations$unsupported_inferences %||% character(0)
  ))
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
      id = "cluster_metrics",
      label = "Cluster metrics",
      display_order = 20L,
      retain_rank = 20L,
      lines = paste0(
        "Metrics: h=",
        formatC(x$context$cluster_metrics$h, digits = 3L, format = "f"),
        ", k=",
        x$context$cluster_metrics$k,
        ", m=",
        x$context$cluster_metrics$m
      )
    ),
    .new_cluster_evidence_prompt_inline_block(
      id = "species_topological",
      label = "Topological species",
      display_order = 30L,
      retain_rank = 50L,
      header = "Topological species: ",
      items = topo_items,
      empty_text = "none"
    ),
    .new_cluster_evidence_prompt_inline_block(
      id = "species_phi",
      label = "Phi-ranked species",
      display_order = 40L,
      retain_rank = 40L,
      header = "Phi-ranked species: ",
      items = phi_items
    ),
    .new_cluster_evidence_prompt_fixed_block(
      id = "plots_membership",
      label = "Plot membership",
      display_order = 50L,
      retain_rank = 30L,
      lines = paste0(
        "Plot membership: n=",
        x$summaries$plots_membership$n_member_plots,
        " [",
        x$summaries$plots_membership$evidence_id,
        "]"
      )
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
    .new_cluster_evidence_prompt_inline_block(
      id = "cover_summary",
      label = "Cover summary",
      display_order = 80L,
      retain_rank = 100L,
      header = "Cover summary: ",
      items = cover_items
    ),
    .new_cluster_evidence_prompt_fixed_block(
      id = "user_added_data",
      label = "User-added data",
      display_order = 85L,
      retain_rank = 65L,
      lines = user_added_lines
    ),
    .new_cluster_evidence_prompt_inline_block(
      id = "semantic_axes",
      label = "Semantic axes",
      display_order = 90L,
      retain_rank = 70L,
      header = "Semantic axes: ",
      items = semantic_axis_items,
      intro_lines = "Semantic indicator profile: use as an ecological hint, not as formal habitat proof."
    ),
    .new_cluster_evidence_prompt_inline_block(
      id = "semantic_unmatched_species",
      label = "Semantic unmatched species",
      display_order = 100L,
      retain_rank = 110L,
      header = "Semantic unmatched species: ",
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

.fit_cluster_evidence_prompt_block <- function(block, available_chars) {
  available_chars <- if (is.null(available_chars) || !is.finite(available_chars)) {
    Inf
  } else {
    as.integer(available_chars)
  }
  full_text <- .render_cluster_evidence_prompt_block_text(block)
  full_chars <- .cluster_evidence_prompt_char_count(full_text)

  if (!nzchar(full_text)) {
    return(list(
      status = "absent",
      text = "",
      chars_used = 0L,
      chars_full = full_chars,
      item_count_used = 0L,
      item_count_full = as.integer(block$item_count_full)
    ))
  }

  if (!is.finite(available_chars) || full_chars <= available_chars) {
    return(list(
      status = "kept",
      text = full_text,
      chars_used = full_chars,
      chars_full = full_chars,
      item_count_used = as.integer(block$item_count_full),
      item_count_full = as.integer(block$item_count_full)
    ))
  }

  if (block$item_count_full > 1L) {
    for (n_items in seq.int(block$item_count_full - 1L, 1L)) {
      text_try <- .render_cluster_evidence_prompt_block_text(block, n_items = n_items)
      chars_try <- .cluster_evidence_prompt_char_count(text_try)
      if (chars_try <= available_chars) {
        return(list(
          status = "truncated",
          text = text_try,
          chars_used = chars_try,
          chars_full = full_chars,
          item_count_used = as.integer(n_items),
          item_count_full = as.integer(block$item_count_full)
        ))
      }
    }
  }

  list(
    status = "dropped",
    text = "",
    chars_used = 0L,
    chars_full = full_chars,
    item_count_used = 0L,
    item_count_full = as.integer(block$item_count_full)
  )
}

.serialize_cluster_evidence_blocks <- function(blocks, max_chars = NULL) {
  max_chars <- .arg_nullable_non_negative_integer(max_chars, "max_chars")
  remaining_chars <- if (is.null(max_chars)) Inf else as.integer(max_chars)
  select_order <- order(
    vapply(blocks, `[[`, integer(1L), "retain_rank"),
    vapply(blocks, `[[`, integer(1L), "display_order")
  )

  block_state <- vector("list", length(blocks))
  names(block_state) <- vapply(blocks, `[[`, character(1L), "id")
  kept_block_count <- 0L

  for (idx in select_order) {
    block <- blocks[[idx]]
    separator_chars <- if (kept_block_count > 0L) 1L else 0L
    available_for_block <- if (is.finite(remaining_chars)) {
      remaining_chars - separator_chars
    } else {
      Inf
    }

    if (is.finite(available_for_block) && available_for_block < 1L) {
      fit <- .fit_cluster_evidence_prompt_block(block, 0L)
      fit$status <- if (identical(fit$status, "absent")) "absent" else "dropped"
    } else {
      fit <- .fit_cluster_evidence_prompt_block(block, available_for_block)
    }

    if (fit$chars_used > 0L) {
      kept_block_count <- kept_block_count + 1L
      if (is.finite(remaining_chars)) {
        remaining_chars <- remaining_chars - separator_chars - fit$chars_used
      }
    }

    block_state[[idx]] <- c(
      list(
        id = block$id,
        label = block$label,
        display_order = as.integer(block$display_order),
        retain_rank = as.integer(block$retain_rank)
      ),
      fit
    )
  }

  block_table <- do.call(
    rbind,
    lapply(block_state, function(row) {
      data.frame(
        id = row$id,
        label = row$label,
        display_order = row$display_order,
        retain_rank = row$retain_rank,
        status = row$status,
        item_count_full = row$item_count_full,
        item_count_used = row$item_count_used,
        chars_full = row$chars_full,
        chars_used = row$chars_used,
        stringsAsFactors = FALSE
      )
    })
  )

  full_order <- order(block_table$display_order)
  block_table <- block_table[full_order, , drop = FALSE]
  block_state <- block_state[full_order]

  text_full <- paste(
    Filter(
      nzchar,
      vapply(
        blocks[full_order],
        .render_cluster_evidence_prompt_block_text,
        character(1L)
      )
    ),
    collapse = "\n"
  )
  text_used <- paste(
    Filter(
      nzchar,
      vapply(block_state, `[[`, character(1L), "text")
    ),
    collapse = "\n"
  )

  list(
    text = text_used,
    full_text = text_full,
    blocks = block_table,
    max_chars = max_chars,
    chars_full = .cluster_evidence_prompt_char_count(text_full),
    chars_used = .cluster_evidence_prompt_char_count(text_used),
    trimmed = isTRUE(.cluster_evidence_prompt_char_count(text_used) < .cluster_evidence_prompt_char_count(text_full)),
    kept_block_ids = block_table$id[block_table$status %in% c("kept", "truncated")],
    dropped_block_ids = block_table$id[block_table$status == "dropped"],
    truncated_block_ids = block_table$id[block_table$status == "truncated"]
  )
}

.serialize_cluster_evidence_prompt <- function(x, max_chars = NULL) {
  if (!inherits(x, "cluster_evidence")) {
    stop("`x` must inherit from class `cluster_evidence`.")
  }

  .serialize_cluster_evidence_blocks(
    .cluster_evidence_prompt_blocks(x),
    max_chars = max_chars
  )
}

.format_cluster_evidence_review_prompt <- function(x, max_chars = NULL) {
  .serialize_cluster_evidence_prompt(x, max_chars = max_chars)$text
}

.format_cluster_evidence_prompt <- function(x, max_chars = NULL) {
  .format_cluster_evidence_review_prompt(x, max_chars = max_chars)
}

.format_cluster_evidence_debug <- function(x) {
  if (!inherits(x, "cluster_evidence")) {
    stop("`x` must inherit from class `cluster_evidence`.")
  }

  lines <- c(
    paste0("Cluster evidence for ", x$meta$cluster_id),
    paste0("  h = ", formatC(x$context$cluster_metrics$h, digits = 3L, format = "f")),
    paste0("  k = ", x$context$cluster_metrics$k),
    paste0("  m = ", x$context$cluster_metrics$m),
    paste0("  plot_values = ", x$meta$source$plot_values)
  )

  if (!is.null(x$context$topology$parent)) {
    lines <- c(lines, paste0("  parent = ", x$context$topology$parent$cluster_id))
  } else {
    lines <- c(lines, "  parent = <none>")
  }

  if (nrow(x$summaries$species_topological)) {
    lines <- c(
      lines,
      paste0("  topological species (", nrow(x$summaries$species_topological), "): ",
             paste(x$summaries$species_topological$species, collapse = ", "))
    )
  } else {
    lines <- c(lines, "  topological species: <none>")
  }

  if (!is.null(x$summaries$species_phi) && nrow(x$summaries$species_phi)) {
    lines <- c(
      lines,
      paste0("  phi species (", nrow(x$summaries$species_phi), "): ",
             paste(
               paste0(
                 x$summaries$species_phi$species, "=",
                 formatC(x$summaries$species_phi$phi, digits = 3L, format = "f")
               ),
               collapse = ", "
             ))
    )
  } else {
    lines <- c(lines, "  phi species: <none>")
  }

  if (!is.null(x$summaries$semantic_axes) && nrow(x$summaries$semantic_axes)) {
    lines <- c(
      lines,
      paste0(
        "  semantic axes (",
        nrow(x$summaries$semantic_axes),
        "): ",
        paste(
          paste0(
            x$summaries$semantic_axes$axis,
            "=",
            formatC(x$summaries$semantic_axes$score_0_10, digits = 2L, format = "f"),
            " [", x$summaries$semantic_axes$band, "]"
          ),
          collapse = ", "
        )
      )
    )

    unmatched <- x$summaries$semantic_unmatched_species %||% character(0)
    if (length(unmatched)) {
      lines <- c(
        lines,
        paste0("  semantic unmatched species: ", paste(unmatched, collapse = ", "))
      )
    }
  }

  lines <- c(
    lines,
    paste0("  member plots = ", x$summaries$plots_membership$n_member_plots),
    paste0("  evidence records = ", length(x$evidence$items))
  )

  if (length(x$limitations$missing_components)) {
    lines <- c(
      lines,
      paste0("  missing components: ", paste(x$limitations$missing_components, collapse = ", "))
    )
  }
  if (length(x$limitations$warnings)) {
    lines <- c(lines, paste0("  warnings: ", paste(x$limitations$warnings, collapse = " | ")))
  }

  paste(lines, collapse = "\n")
}

#' @export
print.cluster_evidence <- function(x, ...) {
  cat(.format_cluster_evidence_debug(x), "\n")
  invisible(x)
}
