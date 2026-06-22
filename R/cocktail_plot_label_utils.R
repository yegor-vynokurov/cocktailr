# Internal helpers for plot-time use of cluster label registries.
#
# These utilities keep `cocktail_plot()` focused on geometry while the
# label-registry parsing and legend/caption assembly stay testable on their own.

.normalize_cocktail_plot_label_registry <- function(label_registry) {
  if (is.null(label_registry)) {
    return(NULL)
  }

  if (inherits(label_registry, "cluster_label_batch_result")) {
    label_registry <- label_registry$label_registry %||% cluster_label_registry(label_registry)
  }

  if (!is.data.frame(label_registry)) {
    stop(
      "`label_registry` must be NULL, a `cluster_label_registry` table, or a `cluster_label_batch_result` object.",
      call. = FALSE
    )
  }

  required_cols <- c("cluster", "legend_label", "review_file")
  missing_cols <- setdiff(required_cols, names(label_registry))
  if (length(missing_cols)) {
    stop(
      "`label_registry` is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  reg <- label_registry
  reg$cluster <- vapply(reg$cluster, .as_scalar_character, character(1))
  keep <- !is.na(reg$cluster) & nzchar(reg$cluster)
  reg <- reg[keep, , drop = FALSE]
  if (!nrow(reg)) {
    return(reg[0, , drop = FALSE])
  }
  reg <- reg[!duplicated(reg$cluster), , drop = FALSE]
  rownames(reg) <- NULL
  reg
}

.resolve_cocktail_plot_label_registry <- function(label_registry, x) {
  if (is.null(label_registry)) {
    return(NULL)
  }

  if (is.character(label_registry) && length(label_registry) == 1L) {
    if (identical(label_registry, "auto")) {
      return(.load_cocktail_plot_label_registry_auto(x))
    }

    stop(
      "`label_registry` may be NULL, \"auto\", a `cluster_label_registry` table, or a `cluster_label_batch_result` object.",
      call. = FALSE
    )
  }

  .normalize_cocktail_plot_label_registry(label_registry)
}

.default_cluster_label_registry_root <- function() {
  .resolve_cocktailr_output_path(file.path("temp", "reports", "cluster_reviews"))
}

.cocktail_plot_dataset_info <- function(x) {
  info <- x$dataset %||% list()
  if (!is.list(info)) {
    info <- list()
  }

  path <- .as_scalar_character(info$path)
  label <- .as_scalar_character(info$label)
  type <- .as_scalar_character(info$type)

  if (is.na(label) && !is.na(path) && nzchar(path)) {
    label <- tools::file_path_sans_ext(basename(path))
  }

  folder_slug <- if (!is.na(label) && nzchar(label)) {
    .sanitize_review_path_component(label)
  } else if (!is.na(path) && nzchar(path)) {
    .sanitize_review_path_component(tools::file_path_sans_ext(basename(path)))
  } else {
    NA_character_
  }

  list(
    type = if (!is.na(type) && nzchar(type)) type else NA_character_,
    label = if (!is.na(label) && nzchar(label)) label else NA_character_,
    path = if (!is.na(path) && nzchar(path)) normalizePath(path, winslash = "/", mustWork = FALSE) else NA_character_,
    folder_slug = folder_slug
  )
}

.expected_cluster_label_registry_file <- function(x, root_dir) {
  dataset <- .cocktail_plot_dataset_info(x)
  if (is.na(dataset$folder_slug) || !nzchar(dataset$folder_slug)) {
    return(NA_character_)
  }

  file.path(root_dir, dataset$folder_slug, .cluster_label_registry_basename())
}

.load_cocktail_plot_label_registry_auto <- function(x) {
  root_dir <- .default_cluster_label_registry_root()
  if (is.null(root_dir) || is.na(root_dir) || !nzchar(root_dir) || !dir.exists(root_dir)) {
    warning(
      "Automatic label-registry lookup did not find the default registry root. ",
      "Run `label_clusters(..., labels_for_imgs = TRUE)` first, or pass `label_registry` explicitly.",
      call. = FALSE
    )
    return(NULL)
  }

  expected_file <- .expected_cluster_label_registry_file(x, root_dir)
  if (!is.na(expected_file) && file.exists(expected_file)) {
    return(.read_cluster_label_registry_file(expected_file))
  }

  candidates <- list.files(
    root_dir,
    pattern = paste0("^", .cluster_label_registry_basename(), "$"),
    recursive = TRUE,
    full.names = TRUE
  )
  if (!length(candidates)) {
    warning(
      "Automatic label-registry lookup did not find a saved `cluster_label_registry.csv` under ",
      .cocktail_plot_relative_display_path(root_dir),
      ". Run `label_clusters(..., labels_for_imgs = TRUE)` first, or pass `label_registry` explicitly.",
      call. = FALSE
    )
    return(NULL)
  }

  if (length(candidates) > 1L) {
    info <- file.info(candidates)
    ord <- order(info$mtime, decreasing = TRUE, na.last = TRUE)
    chosen <- candidates[[ord[[1L]]]]
    warning(
      "Automatic label-registry lookup found multiple saved registries and is using the most recent one: ",
      .cocktail_plot_relative_display_path(chosen),
      ". Pass `label_registry` explicitly to avoid ambiguity.",
      call. = FALSE
    )
    return(.read_cluster_label_registry_file(chosen))
  }

  .read_cluster_label_registry_file(candidates[[1L]])
}

.cocktail_plot_registry_page_subset <- function(label_registry, label_ids) {
  if (is.null(label_registry) || !length(label_ids)) {
    return(NULL)
  }

  cluster_labels <- paste0("c_", unique(as.integer(label_ids)))
  idx <- match(cluster_labels, label_registry$cluster)
  idx <- idx[!is.na(idx)]
  if (!length(idx)) {
    return(NULL)
  }

  out <- label_registry[idx, , drop = FALSE]
  out$cluster <- cluster_labels[match(out$cluster, cluster_labels)]
  rownames(out) <- NULL
  out
}

.cocktail_plot_relative_display_path <- function(path) {
  path <- .as_scalar_character(path)
  if (is.na(path) || !nzchar(path)) {
    return(NA_character_)
  }

  if (!.is_absolute_output_path(path)) {
    return(gsub("\\\\", "/", path))
  }

  path_norm <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- .cocktailr_source_root()
  if (!is.null(root) && nzchar(root)) {
    root_norm <- normalizePath(root, winslash = "/", mustWork = FALSE)
    prefix <- paste0(root_norm, "/")
    if (startsWith(path_norm, prefix)) {
      return(substr(path_norm, nchar(prefix) + 1L, nchar(path_norm)))
    }
  }

  path_norm
}

.cocktail_plot_common_review_dir <- function(files) {
  files <- vapply(files, .cocktail_plot_relative_display_path, character(1))
  files <- files[!is.na(files) & nzchar(files)]
  if (!length(files)) {
    return(NA_character_)
  }

  dirs <- dirname(files)
  dirs <- unique(dirs)
  if (length(dirs) == 1L) {
    return(dirs[[1]])
  }

  split_dirs <- strsplit(dirs, "/", fixed = TRUE)
  min_len <- min(lengths(split_dirs))
  common <- character(0)

  for (i in seq_len(min_len)) {
    piece <- vapply(split_dirs, `[`, character(1), i)
    if (length(unique(piece)) != 1L) {
      break
    }
    common <- c(common, piece[[1]])
  }

  if (!length(common)) {
    return(NA_character_)
  }

  paste(common, collapse = "/")
}

.cocktail_plot_legend_lines <- function(label_registry_page, max_entries = 12L) {
  if (is.null(label_registry_page) || !nrow(label_registry_page)) {
    return(character(0))
  }

  max_entries <- max(1L, as.integer(max_entries))
  common_dir <- .cocktail_plot_common_review_dir(label_registry_page$review_file)

  lines <- character(0)
  if (!is.na(common_dir) && nzchar(common_dir)) {
    lines <- c(lines, paste0("Label reviews: ", common_dir, "/"))
  } else {
    lines <- c(lines, "Label reviews: paths not recorded.")
  }

  shown_n <- min(nrow(label_registry_page), max_entries)
  page_slice <- label_registry_page[seq_len(shown_n), , drop = FALSE]

  entry_lines <- vapply(seq_len(nrow(page_slice)), function(i) {
    entry <- page_slice[i, , drop = FALSE]
    filename <- basename(.as_scalar_character(entry$review_file[[1]]))
    filename <- if (!is.na(filename) && nzchar(filename)) {
      paste0(" (", filename, ")")
    } else {
      ""
    }
    paste0(entry$legend_label[[1]], filename)
  }, character(1))

  lines <- c(lines, entry_lines)

  remaining <- nrow(label_registry_page) - shown_n
  if (remaining > 0L) {
    lines <- c(lines, paste0("... +", remaining, " more labeled cluster(s) on this page"))
  }

  if (.cocktail_plot_registry_has_speculative(label_registry_page)) {
    lines <- c(
      lines,
      "* tentative / speculative label; strict validation did not accept a stable evidence-backed label"
    )
  }

  lines
}

.cocktail_plot_registry_has_speculative <- function(label_registry_page) {
  if (is.null(label_registry_page) || !nrow(label_registry_page)) {
    return(FALSE)
  }

  if ("is_speculative" %in% names(label_registry_page)) {
    vals <- label_registry_page$is_speculative
    vals <- vals[!is.na(vals)]
    if (length(vals) && any(vals)) {
      return(TRUE)
    }
  }

  if ("plot_marker" %in% names(label_registry_page)) {
    markers <- vapply(label_registry_page$plot_marker, .as_scalar_character, character(1))
    if (any(!is.na(markers) & nzchar(markers))) {
      return(TRUE)
    }
  }

  if ("review_status" %in% names(label_registry_page)) {
    status <- vapply(label_registry_page$review_status, .as_scalar_character, character(1))
    if (any(status == "speculative", na.rm = TRUE)) {
      return(TRUE)
    }
  }

  FALSE
}
