# Internal helpers for plot-time use of cluster label registries.
#
# These utilities keep `cocktail_plot()` focused on geometry while the
# label-registry parsing and legend/caption assembly stay testable on their own.

.parse_cocktail_plot_clusters_arg <- function(v) {
  if (is.null(v)) {
    return(NULL)
  }

  parse_one <- function(x) {
    if (is.character(x)) {
      as.integer(sub("^c_", "", x))
    } else {
      as.integer(x)
    }
  }

  if (is.data.frame(v)) {
    if (!nrow(v) || !ncol(v)) {
      return(list())
    }

    if (ncol(v) == 1L) {
      v <- v[[1L]]
    } else {
      out <- vector("list", nrow(v))
      for (i in seq_len(nrow(v))) {
        out[[i]] <- parse_one(unlist(v[i, , drop = FALSE], use.names = FALSE))
      }
      return(out)
    }
  }

  if (is.matrix(v)) {
    if (!nrow(v) || !ncol(v)) {
      return(list())
    }

    if (ncol(v) == 1L) {
      v <- v[, 1L]
    } else {
      out <- vector("list", nrow(v))
      for (i in seq_len(nrow(v))) {
        out[[i]] <- parse_one(v[i, ])
      }
      return(out)
    }
  }

  if (is.list(v)) {
    return(lapply(v, parse_one))
  }

  ids <- parse_one(v)
  lapply(as.list(ids), identity)
}

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
  reg <- .cluster_label_registry_add_hclust_label_compact(reg)
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

.cocktail_plot_truncate_text <- function(text, width = 80L) {
  text <- .as_scalar_character(text)
  if (is.na(text) || !nzchar(text)) {
    return("")
  }

  width <- suppressWarnings(as.integer(width))
  if (is.na(width) || width < 4L) {
    return(text)
  }

  if (nchar(text, type = "width") <= width) {
    return(text)
  }

  paste0(substr(text, 1L, width - 3L), "...")
}

.cocktail_plot_wrap_text <- function(text, width = 80L, continuation_indent = 2L) {
  text <- .as_scalar_character(text)
  if (is.na(text) || !nzchar(text)) {
    return(character(0))
  }

  width <- suppressWarnings(as.integer(width))
  width <- if (is.na(width)) 80L else max(12L, width)

  continuation_indent <- suppressWarnings(as.integer(continuation_indent))
  continuation_indent <- if (is.na(continuation_indent)) 0L else max(0L, continuation_indent)

  wrapped <- strwrap(
    text,
    width = width,
    exdent = continuation_indent
  )
  if (!length(wrapped)) {
    return(text)
  }

  wrapped
}

.cocktail_plot_legend_layout <- function(
    label_registry_page,
    max_entries = 12L,
    two_column_threshold = 8L,
    entry_width_one_col = 108L,
    entry_width_two_col = 52L,
    footer_wrap_width = 96L
) {
  empty_layout <- list(
    header_lines = character(0),
    body_columns = list(character(0), character(0)),
    footer_lines = character(0),
    n_columns = 0L
  )

  if (is.null(label_registry_page) || !nrow(label_registry_page)) {
    return(empty_layout)
  }

  max_entries <- max(1L, as.integer(max_entries))
  shown_n <- min(nrow(label_registry_page), max_entries)
  page_slice <- label_registry_page[seq_len(shown_n), , drop = FALSE]

  common_dir <- .cocktail_plot_common_review_dir(label_registry_page$review_file)
  header_text <- if (!is.na(common_dir) && nzchar(common_dir)) {
    paste0("Label reviews: ", common_dir, "/")
  } else {
    "Label reviews: paths not recorded."
  }
  header_lines <- header_text

  n_columns <- if (nrow(page_slice) >= as.integer(two_column_threshold)) 2L else 1L
  entry_width <- if (n_columns == 2L) entry_width_two_col else entry_width_one_col

  entry_blocks <- lapply(seq_len(nrow(page_slice)), function(i) {
    entry <- page_slice[i, , drop = FALSE]
    filename <- basename(.as_scalar_character(entry$review_file[[1]]))
    filename <- if (!is.na(filename) && nzchar(filename)) {
      paste0(" (", filename, ")")
    } else {
      ""
    }

    .cocktail_plot_truncate_text(
      paste0(entry$legend_label[[1]], filename),
      width = entry_width
    )
  })

  if (n_columns == 2L) {
    split_at <- ceiling(length(entry_blocks) / 2)
    left_blocks <- entry_blocks[seq_len(split_at)]
    right_blocks <- if (split_at < length(entry_blocks)) {
      entry_blocks[(split_at + 1L):length(entry_blocks)]
    } else {
      list()
    }
  } else {
    left_blocks <- entry_blocks
    right_blocks <- list()
  }

  flatten_blocks <- function(blocks) {
    if (!length(blocks)) {
      return(character(0))
    }
    as.character(unlist(blocks, use.names = FALSE))
  }

  footer_lines <- character(0)
  remaining <- nrow(label_registry_page) - shown_n
  if (remaining > 0L) {
    footer_lines <- c(
      footer_lines,
      .cocktail_plot_wrap_text(
        paste0("... +", remaining, " more labeled cluster(s) on this page"),
        width = footer_wrap_width,
        continuation_indent = 2L
      )
    )
  }

  if (.cocktail_plot_registry_has_speculative(label_registry_page)) {
    footer_lines <- c(
      footer_lines,
      .cocktail_plot_wrap_text(
        "* tentative / speculative label; strict validation did not accept a stable evidence-backed label",
        width = footer_wrap_width,
        continuation_indent = 2L
      )
    )
  }

  list(
    header_lines = header_lines,
    body_columns = list(
      flatten_blocks(left_blocks),
      flatten_blocks(right_blocks)
    ),
    footer_lines = footer_lines,
    n_columns = n_columns
  )
}

.cocktail_plot_caption_has_content <- function(caption_layout) {
  if (is.null(caption_layout) || !is.list(caption_layout)) {
    return(FALSE)
  }

  any(c(
    length(caption_layout$header_lines),
    length(caption_layout$body_columns[[1L]]),
    length(caption_layout$body_columns[[2L]]),
    length(caption_layout$footer_lines)
  ) > 0L)
}

.cocktail_plot_caption_row_count <- function(caption_layout) {
  if (!.cocktail_plot_caption_has_content(caption_layout)) {
    return(0L)
  }

  n_body_rows <- max(
    length(caption_layout$body_columns[[1L]]),
    length(caption_layout$body_columns[[2L]])
  )

  as.integer(max(
    1L,
    length(caption_layout$header_lines) +
      n_body_rows +
      length(caption_layout$footer_lines)
  ))
}

.cocktail_plot_footer_panel_height <- function(
    caption_layout,
    min_height = 0.9,
    max_height = 2.1,
    base_height = 0.35,
    per_row_height = 0.26
) {
  total_rows <- .cocktail_plot_caption_row_count(caption_layout)
  if (total_rows <= 0L) {
    return(0)
  }

  footer_height <- base_height + per_row_height * total_rows
  min(max_height, max(min_height, footer_height))
}

.cocktail_plot_legend_lines <- function(label_registry_page, max_entries = 12L) {
  layout <- .cocktail_plot_legend_layout(
    label_registry_page = label_registry_page,
    max_entries = max_entries
  )

  c(
    layout$header_lines,
    layout$body_columns[[1L]],
    layout$body_columns[[2L]],
    layout$footer_lines
  )
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

.cocktail_plot_stack_band_rows <- function(
    bands_df,
    gap_frac = 0.08,
    stack_height_frac = 0.26
) {
  if (is.null(bands_df) || !is.data.frame(bands_df) || nrow(bands_df) <= 1L) {
    return(bands_df)
  }

  required_cols <- c("x0", "x1", "y0", "y1")
  if (!all(required_cols %in% names(bands_df))) {
    return(bands_df)
  }

  ord <- order(bands_df$x0, bands_df$x1, decreasing = FALSE, na.last = TRUE)
  lane_end <- numeric(0)
  lane_idx <- integer(nrow(bands_df))

  for (row_idx in ord) {
    placed <- FALSE
    if (length(lane_end)) {
      for (lane in seq_along(lane_end)) {
        if (is.finite(lane_end[[lane]]) && lane_end[[lane]] < bands_df$x0[[row_idx]]) {
          lane_idx[[row_idx]] <- lane
          lane_end[[lane]] <- bands_df$x1[[row_idx]]
          placed <- TRUE
          break
        }
      }
    }

    if (!placed) {
      lane_end <- c(lane_end, bands_df$x1[[row_idx]])
      lane_idx[[row_idx]] <- length(lane_end)
    }
  }

  n_lanes <- max(lane_idx)
  if (!is.finite(n_lanes) || n_lanes <= 1L) {
    return(bands_df)
  }

  y_low <- min(bands_df$y0, bands_df$y1, na.rm = TRUE)
  y_high <- max(bands_df$y0, bands_df$y1, na.rm = TRUE)
  total_height <- y_high - y_low
  if (!is.finite(total_height) || total_height <= 0) {
    return(bands_df)
  }

  stack_height_frac <- suppressWarnings(as.numeric(stack_height_frac))
  if (!is.finite(stack_height_frac) || stack_height_frac <= 0 || stack_height_frac > 1) {
    stack_height_frac <- 0.26
  }

  stack_height <- total_height * stack_height_frac
  stack_y_low <- y_high - stack_height
  lane_height <- stack_height / n_lanes
  gap_frac <- suppressWarnings(as.numeric(gap_frac))
  if (!is.finite(gap_frac) || gap_frac < 0) {
    gap_frac <- 0
  }
  gap_abs <- min(lane_height * gap_frac, lane_height / 3)

  out <- bands_df
  for (i in seq_len(nrow(out))) {
    lane <- lane_idx[[i]]
    lane_top <- y_high - (lane - 1L) * lane_height
    lane_bottom <- max(stack_y_low, y_high - lane * lane_height)
    out$y1[[i]] <- lane_top - gap_abs / 2
    out$y0[[i]] <- lane_bottom + gap_abs / 2
  }

  out
}
