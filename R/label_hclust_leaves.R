#' Replace `hclust` leaf labels using saved cluster labels
#'
#' @description
#' Small helper for base-R `hclust` workflows built from Cocktail cluster IDs,
#' such as `cluster_phi_dist() |> hclust() |> plot()`. It rewrites leaf labels
#' like `"c_12"` using a [cluster_label_registry()] table or a batch result from
#' [label_clusters()].
#'
#' This is intended for low-cluster-count overview plots where replacing the
#' leaf labels themselves is readable. For full Cocktail dendrograms,
#' [cocktail_plot()] remains the recommended path: it keeps stable numeric IDs
#' on the dendrogram and adds a separate legend/caption with human-readable
#' labels.
#'
#' @param hc An object of class `"hclust"`.
#' @param label_registry `NULL`, `"auto"`, a `cluster_label_registry` table, or
#'   a `cluster_label_batch_result` object returned by [label_clusters()]. If
#'   `"auto"`, the helper tries to load the most relevant saved
#'   `cluster_label_registry.csv`.
#' @param x Optional Cocktail object used only when `label_registry = "auto"` to
#'   prefer a dataset-aware saved registry.
#' @param label_field Character scalar naming the registry column to use for
#'   replacement. Common choices are `"hclust_label_compact"`,
#'   `"plot_label_short"`, `"legend_label"`, and `"display_label"`. Default:
#'   `"plot_label_short"`.
#' @param fallback One of `"keep"` or `"cluster"`. `"keep"` leaves unmatched or
#'   empty replacements unchanged. `"cluster"` falls back to normalized cluster
#'   IDs such as `"c_12"`.
#' @param warn_missing Logical. If `TRUE`, warn when some leaves could not be
#'   replaced with a non-empty label from the registry.
#'
#' @return A modified `hclust` object. The original leaf labels are preserved in
#'   `attr(x, "cocktailr_original_labels")`.
#'
#' @examples
#' \dontrun{
#' D <- cluster_phi_dist(
#'   x = res,
#'   clusters = c("c_12", "c_25", "c_35")
#' )
#'
#' hc <- hclust(D, method = "average")
#'
#' run <- label_clusters(
#'   x = res,
#'   clusters = c("c_12", "c_25", "c_35"),
#'   model = "gemma4:12b",
#'   variant = "label_primary_v1",
#'   labels_for_imgs = TRUE
#' )
#'
#' hc_labeled <- label_hclust_leaves(
#'   hc,
#'   label_registry = run$label_registry,
#'   label_field = "legend_label"
#' )
#'
#' plot(hc_labeled, cex = 0.8)
#' }
#'
#' @export
label_hclust_leaves <- function(
    hc,
    label_registry = "auto",
    x = NULL,
    label_field = "plot_label_short",
    fallback = c("keep", "cluster"),
    warn_missing = TRUE
) {
  if (!inherits(hc, "hclust")) {
    stop("`hc` must inherit from `hclust`.", call. = FALSE)
  }

  label_field <- .arg_scalar_character(label_field, "label_field")
  fallback <- match.arg(fallback)
  warn_missing <- .arg_single_flag(warn_missing, "warn_missing")

  current_labels <- .label_hclust_leaf_label_vector(hc$labels, "hc$labels")
  source_labels <- .label_hclust_source_labels(hc, current_labels)

  reg <- .resolve_cocktail_plot_label_registry(label_registry, x)
  if (is.null(reg) || !nrow(reg)) {
    return(hc)
  }

  if (!label_field %in% names(reg)) {
    stop(
      "`label_field` was not found in `label_registry`. Available columns include: ",
      paste(utils::head(names(reg), 12L), collapse = ", "),
      if (length(names(reg)) > 12L) ", ..." else ".",
      call. = FALSE
    )
  }

  registry_keys <- .label_hclust_normalize_cluster_ids(reg$cluster)
  source_keys <- .label_hclust_normalize_cluster_ids(source_labels)
  idx <- match(source_keys, registry_keys)

  replacement <- source_labels
  matched <- !is.na(idx)
  resolved <- rep(FALSE, length(source_labels))

  if (any(matched)) {
    candidate <- reg[[label_field]][idx[matched]]
    candidate <- as.character(candidate)
    candidate_ok <- !is.na(candidate) & nzchar(candidate)
    replacement_idx <- which(matched)
    replacement[replacement_idx[candidate_ok]] <- candidate[candidate_ok]
    resolved[replacement_idx[candidate_ok]] <- TRUE
  }

  unresolved <- !resolved

  if (identical(fallback, "cluster")) {
    fallback_ids <- .label_hclust_fallback_ids(source_labels)
    fallback_ok <- !is.na(fallback_ids) & nzchar(fallback_ids)
    replacement[unresolved & fallback_ok] <- fallback_ids[unresolved & fallback_ok]
  }

  if (isTRUE(warn_missing)) {
    unresolved_n <- sum(unresolved)
    if (unresolved_n > 0L) {
      warning(
        unresolved_n,
        " hclust leaf label(s) were left unchanged because no non-empty `",
        label_field,
        "` value was available in the registry.",
        call. = FALSE
      )
    }
  }

  attr(hc, "cocktailr_original_labels") <- source_labels
  hc$labels <- replacement
  hc
}

.label_hclust_source_labels <- function(hc, current_labels) {
  original <- attr(hc, "cocktailr_original_labels", exact = TRUE)
  if (is.null(original)) {
    return(current_labels)
  }

  original <- .label_hclust_leaf_label_vector(
    original,
    "attr(hc, 'cocktailr_original_labels')"
  )
  if (length(original) != length(current_labels)) {
    return(current_labels)
  }

  original
}

.label_hclust_leaf_label_vector <- function(x, name) {
  if (is.null(x) || !length(x)) {
    stop("`", name, "` must be a non-empty label vector.", call. = FALSE)
  }

  out <- as.character(x)
  if (!length(out) || anyNA(out) || any(!nzchar(out))) {
    stop("`", name, "` must be a non-empty label vector.", call. = FALSE)
  }

  out
}

.label_hclust_normalize_cluster_ids <- function(x) {
  vapply(x, .label_hclust_normalize_cluster_id, character(1))
}

.label_hclust_normalize_cluster_id <- function(x) {
  x <- .as_scalar_character(x)
  if (is.na(x) || !nzchar(x)) {
    return(NA_character_)
  }

  if (grepl("^c_[0-9]+$", x)) {
    return(x)
  }

  direct_match <- regmatches(x, regexpr("^c_[0-9]+", x, perl = TRUE))
  if (length(direct_match) == 1L && nzchar(direct_match)) {
    return(direct_match)
  }

  if (grepl("^[0-9]+$", x)) {
    return(paste0("c_", x))
  }

  numeric_prefix <- regmatches(x, regexpr("^[0-9]+", x, perl = TRUE))
  if (length(numeric_prefix) == 1L && nzchar(numeric_prefix)) {
    return(paste0("c_", numeric_prefix))
  }

  NA_character_
}

.label_hclust_fallback_ids <- function(source_labels) {
  out <- .label_hclust_normalize_cluster_ids(source_labels)
  out[is.na(out) | !nzchar(out)] <- source_labels[is.na(out) | !nzchar(out)]
  out
}
