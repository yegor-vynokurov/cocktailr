#' Plot a cluster-level `hclust` dendrogram in one call
#'
#' @description
#' Convenience wrapper for the common workflow
#' `cluster_phi_dist() -> hclust() -> label_hclust_leaves() -> plot()`.
#'
#' It is designed for small cluster-overview figures where you want to go from
#' a Cocktail result object directly to a cluster dendrogram, optionally with
#' human-readable leaf labels taken from a saved cluster label registry.
#'
#' The lower-level functions remain available and are still the better choice
#' when you need full manual control over each step.
#'
#' @param x A Cocktail object returned by [cocktail_cluster()].
#' @param clusters Optional cluster identifiers to plot. Can be numeric cluster
#'   IDs, character labels like `"c_12"`, or a list of such values. If `NULL`,
#'   the function selects score-ranked clusters via [select_clusters()].
#' @param top_n Integer or `NULL`. Used only when `clusters = NULL`. The wrapper
#'   then selects up to the first `top_n` score-ranked clusters. Default: `10`.
#'   Set `top_n = NULL` to use all eligible clusters selected by
#'   [select_clusters()].
#'   This default keeps the one-call plot readable on typical datasets.
#' @param min_phi,min_k,min_score,mode Selection arguments forwarded to
#'   [select_clusters()] when `clusters = NULL`.
#' @param method Clustering method forwarded to [stats::hclust()]. Default:
#'   `"average"`.
#' @param label_leaves Logical. If `TRUE`, try to replace leaf labels using
#'   [label_hclust_leaves()]. Default: `TRUE`.
#' @param label_registry `NULL`, `"auto"`, a `cluster_label_registry` table, or
#'   a `cluster_label_batch_result` object returned by [label_clusters()].
#'   Used only when `label_leaves = TRUE`. Default: `"auto"`.
#' @param label_field Character scalar naming the registry column to use for
#'   leaf replacement. Default: `"hclust_label_compact"`.
#' @param fallback One of `"keep"` or `"cluster"`, forwarded to
#'   [label_hclust_leaves()].
#' @param warn_missing Logical. Forwarded to [label_hclust_leaves()].
#' @param file Optional output path. If `NULL`, draw on the current graphics
#'   device. If non-`NULL`, only `.pdf` and `.png` outputs are supported.
#' @param main Optional plot title. Default:
#'   `"Cluster dendrogram (co-membership phi distance)"`.
#' @param width_in,height_in Plot size in inches for saved PDF/PNG output.
#'   Defaults: `width_in = 10`, `height_in = 10`.
#' @param png_res Resolution in dpi for PNG output.
#' @param ... Additional graphical arguments forwarded to [graphics::plot()].
#'
#' @return Invisibly returns a list with components:
#'   \describe{
#'     \item{clusters}{Cluster labels actually used in the plot.}
#'     \item{dist}{The \code{dist} object returned by [cluster_phi_dist()].}
#'     \item{hclust}{The raw \code{hclust} object.}
#'     \item{hclust_plot}{The plotted \code{hclust} object, potentially with
#'     relabeled leaves.}
#'     \item{label_registry}{The resolved label registry used for leaf relabeling,
#'     or `NULL`.}
#'     \item{file}{Normalized output path when `file` was used, otherwise `NULL`.}
#'   }
#'
#' @examples
#' \dontrun{
#' cluster_hclust_plot(
#'   x = res,
#'   top_n = 10,
#'   label_registry = "auto"
#' )
#'
#' cluster_hclust_plot(
#'   x = res,
#'   clusters = c("c_12", "c_25", "c_35"),
#'   label_registry = "auto",
#'   label_field = "plot_label_short",
#'   file = "temp/reports/cluster_hclust/demo.png"
#' )
#' }
#'
#' @export
cluster_hclust_plot <- function(
    x,
    clusters = NULL,
    top_n = 10L,
    min_phi = 0.2,
    min_k = 1L,
    min_score = 0,
    mode = c("strict", "top"),
    method = "average",
    label_leaves = TRUE,
    label_registry = "auto",
    label_field = "hclust_label_compact",
    fallback = c("keep", "cluster"),
    warn_missing = TRUE,
    file = NULL,
    main = "Cluster dendrogram (co-membership phi distance)",
    width_in = 10,
    height_in = 10,
    png_res = 150,
    ...
) {
  mode <- match.arg(mode)
  label_leaves <- .arg_single_flag(label_leaves, "label_leaves")
  warn_missing <- .arg_single_flag(warn_missing, "warn_missing")
  label_field <- .arg_scalar_character(label_field, "label_field")
  fallback <- match.arg(fallback)

  if (!is.null(top_n)) {
    top_n <- .arg_positive_integer(top_n, "top_n")
  }
  if (!is.numeric(min_phi) || length(min_phi) != 1L || is.na(min_phi)) {
    stop("`min_phi` must be a single numeric value.", call. = FALSE)
  }
  if (!is.numeric(min_k) || length(min_k) != 1L || is.na(min_k)) {
    stop("`min_k` must be a single integer-like value.", call. = FALSE)
  }
  min_k <- as.integer(min_k)
  if (!is.numeric(min_score) || length(min_score) != 1L || is.na(min_score)) {
    stop("`min_score` must be a single numeric value.", call. = FALSE)
  }
  method <- .arg_scalar_character(method, "method")
  if (!is.null(file)) {
    file <- .arg_scalar_character(file, "file")
  }
  width_in <- as.numeric(width_in)
  height_in <- as.numeric(height_in)
  png_res <- as.numeric(png_res)
  if (!is.finite(width_in) || width_in <= 0) {
    stop("`width_in` must be a single positive numeric value.", call. = FALSE)
  }
  if (!is.finite(height_in) || height_in <= 0) {
    stop("`height_in` must be a single positive numeric value.", call. = FALSE)
  }
  if (!is.finite(png_res) || png_res <= 0) {
    stop("`png_res` must be a single positive numeric value.", call. = FALSE)
  }

  clusters_used <- .resolve_cluster_hclust_plot_clusters(
    x = x,
    clusters = clusters,
    top_n = top_n,
    min_phi = min_phi,
    min_k = min_k,
    min_score = min_score,
    mode = mode
  )

  if (length(clusters_used) < 2L) {
    stop(
      "Need at least two clusters to build a cluster-level hclust plot. ",
      "Adjust `clusters` or the selection filters.",
      call. = FALSE
    )
  }

  dist_obj <- cluster_phi_dist(x = x, clusters = clusters_used)
  hc <- stats::hclust(dist_obj, method = method)

  reg <- NULL
  hc_plot <- hc
  if (isTRUE(label_leaves)) {
    reg <- .resolve_cocktail_plot_label_registry(label_registry, x)
    hc_plot <- label_hclust_leaves(
      hc = hc,
      label_registry = reg,
      x = x,
      label_field = label_field,
      fallback = fallback,
      warn_missing = warn_missing
    )
  }

  file_out <- .draw_cluster_hclust_plot(
    hc = hc_plot,
    file = file,
    main = main,
    width_in = width_in,
    height_in = height_in,
    png_res = png_res,
    ...
  )

  invisible(list(
    clusters = clusters_used,
    dist = dist_obj,
    hclust = hc,
    hclust_plot = hc_plot,
    label_registry = reg,
    file = file_out
  ))
}

.resolve_cluster_hclust_plot_clusters <- function(
    x,
    clusters,
    top_n,
    min_phi,
    min_k,
    min_score,
    mode
) {
  if (!is.null(clusters)) {
    return(.normalize_cluster_labels(
      clusters = clusters,
      n_nodes = nrow(x$Cluster.species)
    ))
  }

  selected <- select_clusters(
    x = x,
    min_phi = min_phi,
    min_k = min_k,
    min_score = min_score,
    mode = mode,
    return = "labels"
  )

  if (!is.null(top_n)) {
    selected <- utils::head(selected, top_n)
  }

  selected
}

.draw_cluster_hclust_plot <- function(
    hc,
    file,
    main,
    width_in,
    height_in,
    png_res,
    ...
) {
  if (is.null(file)) {
    graphics::plot(stats::as.dendrogram(hc), main = main, ...)
    return(NULL)
  }

  file <- .resolve_cocktailr_output_path(file)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  file_lower <- tolower(file)

  if (grepl("\\.pdf$", file_lower)) {
    grDevices::pdf(file = file, width = width_in, height = height_in)
  } else if (grepl("\\.png$", file_lower)) {
    grDevices::png(
      filename = file,
      width = width_in,
      height = height_in,
      units = "in",
      res = png_res
    )
  } else {
    stop(
      "`file` must be NULL or end with `.pdf` or `.png` for `cluster_hclust_plot()`.",
      call. = FALSE
    )
  }

  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::plot(stats::as.dendrogram(hc), main = main, ...)

  normalizePath(file, winslash = "/", mustWork = TRUE)
}
