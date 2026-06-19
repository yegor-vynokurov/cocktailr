# Internal helpers for deterministic cluster-evidence extraction.
#
# Keeping these helpers at file scope makes `cluster_evidence()` itself read as
# a small orchestration function and avoids re-creating many nested functions on
# every call.

.cluster_evidence_is_cocktail_object <- function(obj) {
  is.list(obj) &&
    all(c(
      "Cluster.species",
      "Cluster.info",
      "Cluster.height",
      "Cluster.merged",
      "Plot.cluster"
    ) %in% names(obj))
}

.cluster_evidence_parse_cluster_id <- function(cluster, n_nodes) {
  if (missing(cluster) || is.null(cluster) || length(cluster) != 1L) {
    stop("`cluster` must be a single cluster label like 'c_37' or one integer node ID.")
  }

  id <- if (is.character(cluster)) {
    as.integer(sub("^c_", "", cluster))
  } else if (is.numeric(cluster) || is.integer(cluster)) {
    as.integer(cluster)
  } else {
    stop("`cluster` must be a single cluster label like 'c_37' or one integer node ID.")
  }

  if (!is.finite(id) || is.na(id) || id < 1L || id > n_nodes) {
    stop("`cluster` does not refer to a valid node in 1..", n_nodes, ".")
  }

  list(id = id, label = paste0("c_", id))
}

.cluster_evidence_plot_values_kind <- function(plot_cluster) {
  vals <- if (inherits(plot_cluster, "Matrix")) {
    as.numeric(plot_cluster@x)
  } else {
    as.numeric(plot_cluster[plot_cluster != 0])
  }

  if (!length(vals)) {
    return("unknown")
  }

  if (all(abs(vals - 1) < 1e-12)) "binary" else "rel_cover"
}

.cluster_evidence_node_col_index <- function(colnames_x, id) {
  cand1 <- paste0("c_", id)
  if (!is.null(colnames_x) && cand1 %in% colnames_x) {
    return(match(cand1, colnames_x))
  }

  cand2 <- as.character(id)
  if (!is.null(colnames_x) && cand2 %in% colnames_x) {
    return(match(cand2, colnames_x))
  }

  id
}

.cluster_evidence_parent_id <- function(cluster_merged, id) {
  idx <- which(cluster_merged[, 1L] == id | cluster_merged[, 2L] == id)
  if (!length(idx)) {
    return(NULL)
  }
  idx[1L]
}

.cluster_evidence_depth <- function(cluster_merged, id) {
  depth <- 0L
  seen <- integer(0)
  current <- id

  repeat {
    pid <- .cluster_evidence_parent_id(cluster_merged, current)
    if (is.null(pid) || pid %in% seen) {
      break
    }
    seen <- c(seen, pid)
    depth <- depth + 1L
    current <- pid
  }

  depth
}

.cluster_evidence_children_info <- function(cluster_merged, id, species_names) {
  kids <- as.integer(cluster_merged[id, ])
  if (!length(kids)) {
    return(data.frame(
      child_kind = character(0),
      child_id = integer(0),
      child_label = character(0),
      stringsAsFactors = FALSE
    ))
  }

  out <- lapply(kids, function(kid) {
    if (kid > 0L) {
      data.frame(
        child_kind = "cluster",
        child_id = kid,
        child_label = paste0("c_", kid),
        stringsAsFactors = FALSE
      )
    } else {
      sid <- abs(kid)
      sp_label <- if (!is.null(species_names) &&
        sid >= 1L &&
        sid <= length(species_names)) {
        species_names[sid]
      } else {
        paste0("sp_", sid)
      }
      data.frame(
        child_kind = "species",
        child_id = sid,
        child_label = sp_label,
        stringsAsFactors = FALSE
      )
    }
  })

  do.call(rbind, out)
}

.cluster_evidence_empty_plot_summary_df <- function() {
  data.frame(
    plot = character(0),
    membership_value = numeric(0),
    present_fraction = numeric(0),
    cover_share = numeric(0),
    support_score = numeric(0),
    rationale = character(0),
    evidence_id = character(0),
    stringsAsFactors = FALSE
  )
}

.cluster_evidence_score_member_plots <- function(member_values, topo_species, vegmatrix) {
  if (!length(member_values)) {
    return(.cluster_evidence_empty_plot_summary_df())
  }

  plot_ids <- names(member_values)
  membership_value <- as.numeric(member_values)

  if (is.null(vegmatrix) || !length(topo_species)) {
    support_score <- membership_value
    out <- data.frame(
      plot = plot_ids,
      membership_value = membership_value,
      present_fraction = rep(NA_real_, length(plot_ids)),
      cover_share = rep(NA_real_, length(plot_ids)),
      support_score = support_score,
      rationale = sprintf(
        paste(
          "membership_value=%.3f; support uses Plot.cluster only because",
          "vegmatrix or cluster species detail is unavailable."
        ),
        membership_value
      ),
      evidence_id = rep(NA_character_, length(plot_ids)),
      stringsAsFactors = FALSE
    )
    return(out)
  }

  vm_sub <- vegmatrix[plot_ids, , drop = FALSE]
  vm_topo <- vm_sub[, topo_species, drop = FALSE]

  total_cover <- rowSums(vm_sub)
  topo_cover <- rowSums(vm_topo)
  present_count <- rowSums(vm_topo > 0)
  present_fraction <- present_count / max(1L, length(topo_species))
  cover_share <- ifelse(total_cover > 0, topo_cover / total_cover, 0)

  membership_scaled <- if (length(unique(membership_value)) > 1L &&
    max(membership_value, na.rm = TRUE) > 0) {
    membership_value / max(membership_value, na.rm = TRUE)
  } else {
    ifelse(membership_value > 0, 1, 0)
  }

  support_score <- (membership_scaled + present_fraction + cover_share) / 3

  data.frame(
    plot = plot_ids,
    membership_value = membership_value,
    present_fraction = present_fraction,
    cover_share = cover_share,
    support_score = support_score,
    rationale = sprintf(
      "membership=%.3f; present_fraction=%.3f; cover_share=%.3f",
      membership_value,
      present_fraction,
      cover_share
    ),
    evidence_id = rep(NA_character_, length(plot_ids)),
    stringsAsFactors = FALSE
  )
}

.cluster_evidence_pick_prototypes <- function(score_df, n_keep) {
  if (!nrow(score_df) || n_keep < 1L) {
    return(.cluster_evidence_empty_plot_summary_df())
  }

  ord <- order(
    -score_df$support_score,
    -score_df$membership_value,
    score_df$plot,
    na.last = TRUE
  )
  out <- score_df[ord, , drop = FALSE]
  rownames(out) <- NULL
  out[seq_len(min(n_keep, nrow(out))), , drop = FALSE]
}

.cluster_evidence_pick_borderlines <- function(
    score_df,
    n_keep,
    exclude_plots = character(0)
) {
  if (!nrow(score_df) || n_keep < 1L) {
    return(.cluster_evidence_empty_plot_summary_df())
  }
  if (length(exclude_plots)) {
    score_df <- score_df[!(score_df$plot %in% exclude_plots), , drop = FALSE]
  }
  if (!nrow(score_df)) {
    return(.cluster_evidence_empty_plot_summary_df())
  }

  ord <- order(
    score_df$support_score,
    score_df$membership_value,
    score_df$plot,
    na.last = TRUE
  )
  out <- score_df[ord, , drop = FALSE]
  rownames(out) <- NULL
  out[seq_len(min(n_keep, nrow(out))), , drop = FALSE]
}

.cluster_evidence_cover_summary <- function(member_plots, topo_species, vegmatrix) {
  if (is.null(vegmatrix) || !length(member_plots) || !length(topo_species)) {
    return(NULL)
  }

  vm_topo <- vegmatrix[member_plots, topo_species, drop = FALSE]
  out <- data.frame(
    species = topo_species,
    mean_cover = as.numeric(colMeans(vm_topo)),
    median_cover = as.numeric(apply(vm_topo, 2L, stats::median)),
    freq_in_member_plots = as.numeric(colMeans(vm_topo > 0)),
    evidence_id = rep(NA_character_, length(topo_species)),
    stringsAsFactors = FALSE
  )

  ord <- order(
    -out$mean_cover,
    -out$freq_in_member_plots,
    out$species,
    na.last = TRUE
  )
  out <- out[ord, , drop = FALSE]
  rownames(out) <- NULL
  out
}

.cluster_evidence_phi_summary <- function(species_cluster_phi, cluster_id, top_n_phi) {
  if (is.null(species_cluster_phi)) {
    return(NULL)
  }
  if (!is.matrix(species_cluster_phi) ||
      is.null(rownames(species_cluster_phi)) ||
      is.null(colnames(species_cluster_phi))) {
    stop("`x$Species.cluster.phi` must be a matrix with row and column names.")
  }

  col_idx <- .cluster_evidence_node_col_index(colnames(species_cluster_phi), cluster_id)
  if (is.na(col_idx) || col_idx < 1L || col_idx > ncol(species_cluster_phi)) {
    stop("`x$Species.cluster.phi` does not contain a column for cluster c_", cluster_id, ".")
  }

  phi_vals <- as.numeric(species_cluster_phi[, col_idx])
  names(phi_vals) <- rownames(species_cluster_phi)
  phi_vals[!is.finite(phi_vals) | phi_vals < 0] <- 0
  phi_vals <- sort(phi_vals[phi_vals > 0], decreasing = TRUE)

  if (!length(phi_vals)) {
    return(data.frame(
      species = character(0),
      phi = numeric(0),
      evidence_id = character(0),
      stringsAsFactors = FALSE
    ))
  }

  if (is.finite(top_n_phi) && top_n_phi > 0L && length(phi_vals) > top_n_phi) {
    phi_vals <- phi_vals[seq_len(top_n_phi)]
  }

  data.frame(
    species = names(phi_vals),
    phi = as.numeric(phi_vals),
    evidence_id = rep(NA_character_, length(phi_vals)),
    stringsAsFactors = FALSE
  )
}

.cluster_evidence_align_vegmatrix <- function(x, plot_names, species_names) {
  if (!("vegmatrix" %in% names(x)) || is.null(x$vegmatrix)) {
    return(NULL)
  }

  vegmatrix <- as.matrix(x$vegmatrix)
  vegmatrix[is.na(vegmatrix)] <- 0
  storage.mode(vegmatrix) <- "double"

  if (is.null(rownames(vegmatrix))) {
    stop("`x$vegmatrix` is present but does not have plot row names.")
  }
  if (is.null(colnames(vegmatrix))) {
    stop("`x$vegmatrix` is present but does not have species column names.")
  }

  if (!all(plot_names %in% rownames(vegmatrix))) {
    missing_plots <- setdiff(plot_names, rownames(vegmatrix))
    stop(
      "`x$vegmatrix` is missing plots referenced by `x$Plot.cluster`: ",
      paste(head(missing_plots, 10L), collapse = ", "),
      if (length(missing_plots) > 10L) " ..." else ""
    )
  }

  common_species <- intersect(species_names, colnames(vegmatrix))
  if (!length(common_species)) {
    stop("`x$vegmatrix` does not overlap with species stored in `x$Cluster.species`.")
  }

  vegmatrix[plot_names, common_species, drop = FALSE]
}

.cluster_evidence_dataset_info <- function(x) {
  info <- x$dataset %||% list()
  if (!is.list(info)) {
    info <- list()
  }

  list(
    type = info$type %||% NULL,
    label = info$label %||% NULL,
    path = info$path %||% NULL,
    input_format = info$input_format %||% (x$input_format %||% NULL),
    source = info$source %||% NULL,
    representation = info$representation %||% NULL
  )
}
