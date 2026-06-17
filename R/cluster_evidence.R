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
#' @export
cluster_evidence <- function(
    x,
    cluster,
    top_n_phi = 10L,
    n_prototype_plots = 5L,
    n_borderline_plots = 5L,
    include_cover = TRUE
) {
  .is_cocktail <- function(obj) {
    is.list(obj) &&
      all(c("Cluster.species", "Cluster.info", "Cluster.height",
            "Cluster.merged", "Plot.cluster") %in% names(obj))
  }

  .parse_cluster_id <- function(cluster, n_nodes) {
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

  .as_plot_values_kind <- function(PC) {
    vals <- if (inherits(PC, "Matrix")) {
      as.numeric(PC@x)
    } else {
      as.numeric(PC[PC != 0])
    }

    if (!length(vals)) return("unknown")
    if (all(abs(vals - 1) < 1e-12)) "binary" else "rel_cover"
  }

  .node_col_index <- function(colnames_x, id) {
    cand1 <- paste0("c_", id)
    if (!is.null(colnames_x) && cand1 %in% colnames_x) return(match(cand1, colnames_x))
    cand2 <- as.character(id)
    if (!is.null(colnames_x) && cand2 %in% colnames_x) return(match(cand2, colnames_x))
    id
  }

  .parent_id <- function(CM, id) {
    idx <- which(CM[, 1L] == id | CM[, 2L] == id)
    if (!length(idx)) return(NULL)
    idx[1L]
  }

  .depth_of <- function(CM, id) {
    depth <- 0L
    seen <- integer(0)
    current <- id

    repeat {
      pid <- .parent_id(CM, current)
      if (is.null(pid)) break
      if (pid %in% seen) break
      seen <- c(seen, pid)
      depth <- depth + 1L
      current <- pid
    }

    depth
  }

  .children_info <- function(CM, id, species_names) {
    kids <- as.integer(CM[id, ])
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
        sp_label <- if (!is.null(species_names) && sid >= 1L && sid <= length(species_names)) {
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

  .empty_plot_summary_df <- function() {
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

  .score_member_plots <- function(member_values, topo_species, vm) {
    if (!length(member_values)) return(.empty_plot_summary_df())

    plot_ids <- names(member_values)
    membership_value <- as.numeric(member_values)

    if (is.null(vm) || !length(topo_species)) {
      support_score <- membership_value
      out <- data.frame(
        plot = plot_ids,
        membership_value = membership_value,
        present_fraction = rep(NA_real_, length(plot_ids)),
        cover_share = rep(NA_real_, length(plot_ids)),
        support_score = support_score,
        rationale = sprintf(
          "membership_value=%.3f; support uses Plot.cluster only because vegmatrix or cluster species detail is unavailable.",
          membership_value
        ),
        evidence_id = rep(NA_character_, length(plot_ids)),
        stringsAsFactors = FALSE
      )
      return(out)
    }

    vm_sub <- vm[plot_ids, , drop = FALSE]
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
        membership_value, present_fraction, cover_share
      ),
      evidence_id = rep(NA_character_, length(plot_ids)),
      stringsAsFactors = FALSE
    )
  }

  .pick_prototypes <- function(score_df, n_keep) {
    if (!nrow(score_df) || n_keep < 1L) return(.empty_plot_summary_df())
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

  .pick_borderlines <- function(score_df, n_keep, exclude_plots = character(0)) {
    if (!nrow(score_df) || n_keep < 1L) return(.empty_plot_summary_df())
    if (length(exclude_plots)) {
      score_df <- score_df[!(score_df$plot %in% exclude_plots), , drop = FALSE]
    }
    if (!nrow(score_df)) return(.empty_plot_summary_df())

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

  .cover_summary <- function(member_plots, topo_species, vm) {
    if (is.null(vm) || !length(member_plots) || !length(topo_species)) {
      return(NULL)
    }

    vm_topo <- vm[member_plots, topo_species, drop = FALSE]
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

  .phi_summary <- function(Phi, cluster_id, top_n_phi) {
    if (is.null(Phi)) return(NULL)
    if (!is.matrix(Phi) || is.null(rownames(Phi)) || is.null(colnames(Phi))) {
      stop("`x$Species.cluster.phi` must be a matrix with row and column names.")
    }

    col_idx <- .node_col_index(colnames(Phi), cluster_id)
    if (is.na(col_idx) || col_idx < 1L || col_idx > ncol(Phi)) {
      stop("`x$Species.cluster.phi` does not contain a column for cluster c_", cluster_id, ".")
    }

    phi_vals <- as.numeric(Phi[, col_idx])
    names(phi_vals) <- rownames(Phi)
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

  .align_vegmatrix <- function(x, plot_names, species_names) {
    if (!("vegmatrix" %in% names(x)) || is.null(x$vegmatrix)) return(NULL)

    vm <- as.matrix(x$vegmatrix)
    vm[is.na(vm)] <- 0
    storage.mode(vm) <- "double"

    if (is.null(rownames(vm))) {
      stop("`x$vegmatrix` is present but does not have plot row names.")
    }
    if (is.null(colnames(vm))) {
      stop("`x$vegmatrix` is present but does not have species column names.")
    }

    if (!all(plot_names %in% rownames(vm))) {
      missing_plots <- setdiff(plot_names, rownames(vm))
      stop("`x$vegmatrix` is missing plots referenced by `x$Plot.cluster`: ",
           paste(head(missing_plots, 10L), collapse = ", "),
           if (length(missing_plots) > 10L) " ..." else "")
    }

    common_species <- intersect(species_names, colnames(vm))
    if (!length(common_species)) {
      stop("`x$vegmatrix` does not overlap with species stored in `x$Cluster.species`.")
    }

    vm[plot_names, common_species, drop = FALSE]
  }

  .fmt_num <- function(x, digits = 3L) {
    if (length(x) != 1L || !is.finite(x)) return("NA")
    formatC(x, digits = digits, format = "f")
  }

  if (!.is_cocktail(x)) {
    stop("`x` must be a Cocktail object with Cluster.species, Cluster.info, Cluster.height, Cluster.merged, and Plot.cluster.")
  }

  top_n_phi <- as.integer(top_n_phi)
  if (!is.finite(top_n_phi) || top_n_phi < 1L) top_n_phi <- 10L

  n_prototype_plots <- as.integer(n_prototype_plots)
  if (!is.finite(n_prototype_plots) || n_prototype_plots < 0L) n_prototype_plots <- 5L

  n_borderline_plots <- as.integer(n_borderline_plots)
  if (!is.finite(n_borderline_plots) || n_borderline_plots < 0L) n_borderline_plots <- 5L

  if (!is.logical(include_cover) || length(include_cover) != 1L || is.na(include_cover)) {
    stop("`include_cover` must be TRUE or FALSE.")
  }

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
  cluster_ref <- .parse_cluster_id(cluster, n_nodes)
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

  vm <- .align_vegmatrix(x, plot_names, colnames(CS))

  topo_species <- colnames(CS)[CS[cluster_id, ] > 0]
  topo_summary <- data.frame(
    species = topo_species,
    evidence_id = rep(NA_character_, length(topo_species)),
    stringsAsFactors = FALSE
  )

  phi_available <- "Species.cluster.phi" %in% names(x) && !is.null(x$Species.cluster.phi)
  phi_summary <- if (phi_available) .phi_summary(x$Species.cluster.phi, cluster_id, top_n_phi) else NULL

  member_values <- releves_in_clusters(
    x,
    clusters = cluster_label,
    values = TRUE,
    return = "vector"
  )
  member_values <- member_values[order(names(member_values))]

  plot_scores <- .score_member_plots(member_values, topo_species, vm)
  prototype_plots <- .pick_prototypes(plot_scores, n_prototype_plots)
  borderline_plots <- .pick_borderlines(
    plot_scores,
    n_borderline_plots,
    exclude_plots = prototype_plots$plot
  )

  cover_summary <- if (isTRUE(include_cover)) {
    .cover_summary(names(member_values), topo_species, vm)
  } else {
    NULL
  }

  parent_id <- .parent_id(CM, cluster_id)
  depth <- .depth_of(CM, cluster_id)
  children_df <- .children_info(CM, cluster_id, if ("species" %in% names(x)) x$species else colnames(CS))
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

  plot_values_kind <- .as_plot_values_kind(PC)

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
      cover_summary$evidence_id[i] <- add_evidence(
        "cover_summary",
        "cover_summary",
        paste0("Cover summary for species ", cover_summary$species[i], " in ", cluster_label),
        as.list(cover_summary[i, c("species", "mean_cover", "median_cover",
                                   "freq_in_member_plots"), drop = FALSE]),
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
      source = list(
        object_class = "cocktail",
        has_species_cluster_phi = isTRUE(phi_available),
        has_vegmatrix = !is.null(vm),
        plot_values = plot_values_kind
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

  class(out) <- c("cluster_evidence", class(out))
  out
}

.format_cluster_evidence_prompt <- function(x) {
  if (!inherits(x, "cluster_evidence")) {
    stop("`x` must inherit from class `cluster_evidence`.")
  }

  lines <- c(
    paste0("Cluster: ", x$meta$cluster_id),
    paste0(
      "Metrics: h=", formatC(x$context$cluster_metrics$h, digits = 3L, format = "f"),
      ", k=", x$context$cluster_metrics$k,
      ", m=", x$context$cluster_metrics$m
    )
  )

  if (nrow(x$summaries$species_topological)) {
    topological <- paste0(
      x$summaries$species_topological$species,
      " [", x$summaries$species_topological$evidence_id, "]"
    )
    lines <- c(lines, paste("Topological species:", paste(topological, collapse = "; ")))
  } else {
    lines <- c(lines, "Topological species: none")
  }

  if (!is.null(x$summaries$species_phi) && nrow(x$summaries$species_phi)) {
    phi_lines <- paste0(
      x$summaries$species_phi$species,
      " (phi=", formatC(x$summaries$species_phi$phi, digits = 3L, format = "f"), ") [",
      x$summaries$species_phi$evidence_id, "]"
    )
    lines <- c(lines, paste("Phi-ranked species:", paste(phi_lines, collapse = "; ")))
  }

  lines <- c(
    lines,
    paste0(
      "Plot membership: n=",
      x$summaries$plots_membership$n_member_plots,
      " [", x$summaries$plots_membership$evidence_id, "]"
    )
  )

  if (nrow(x$summaries$plots_prototype)) {
    proto_lines <- paste0(
      x$summaries$plots_prototype$plot,
      " (score=",
      formatC(x$summaries$plots_prototype$support_score, digits = 3L, format = "f"),
      ") [",
      x$summaries$plots_prototype$evidence_id,
      "]"
    )
    lines <- c(lines, paste("Prototype plots:", paste(proto_lines, collapse = "; ")))
  }

  if (nrow(x$summaries$plots_borderline)) {
    borderline_lines <- paste0(
      x$summaries$plots_borderline$plot,
      " (score=",
      formatC(x$summaries$plots_borderline$support_score, digits = 3L, format = "f"),
      ") [",
      x$summaries$plots_borderline$evidence_id,
      "]"
    )
    lines <- c(lines, paste("Borderline plots:", paste(borderline_lines, collapse = "; ")))
  }

  if (!is.null(x$summaries$cover_summary) && nrow(x$summaries$cover_summary)) {
    cover_lines <- paste0(
      x$summaries$cover_summary$species,
      " (mean_cover=",
      formatC(x$summaries$cover_summary$mean_cover, digits = 2L, format = "f"),
      ", freq=",
      formatC(x$summaries$cover_summary$freq_in_member_plots, digits = 2L, format = "f"),
      ") [",
      x$summaries$cover_summary$evidence_id,
      "]"
    )
    lines <- c(lines, paste("Cover summary:", paste(cover_lines, collapse = "; ")))
  }

  if (length(x$limitations$warnings) || length(x$limitations$unsupported_inferences)) {
    lim_lines <- c(x$limitations$warnings, x$limitations$unsupported_inferences)
    lines <- c(lines, "Limitations:", paste0("- ", lim_lines))
  }

  paste(lines, collapse = "\n")
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
