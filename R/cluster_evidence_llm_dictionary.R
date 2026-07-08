# Internal helpers for the user-editable LLM axis dictionary.
#
# The dictionary translates numeric ecological-axis values into fixed English
# phrases. It is intentionally literal: the lookup only maps a value to the
# configured phrase for its numeric interval and does not infer habitat type.

.cluster_evidence_llm_axis_dictionary_file_name <- function(version = "v1") {
  version <- .arg_scalar_character(version, "version")
  paste0("llm_axis_dictionary_", version, ".csv")
}

.cluster_evidence_llm_axis_dictionary_required_columns <- function() {
  c("axis", "lower", "upper", "label", "phrase")
}

.cluster_evidence_llm_axis_dictionary_supported_axes <- function() {
  c("l", "m", "n", "r", "t", "s")
}

.cluster_evidence_llm_axis_dictionary_override_path <- function() {
  path <- getOption("cocktailr.llm_axis_dictionary_path", NULL)
  if (is.null(path)) {
    return(NULL)
  }

  .arg_scalar_character(
    path,
    'getOption("cocktailr.llm_axis_dictionary_path")'
  )
}

.cluster_evidence_llm_normalize_axis <- function(axis) {
  axis <- .as_scalar_character(axis)
  if (is.na(axis) || !nzchar(trimws(axis))) {
    return(NA_character_)
  }

  axis <- tolower(trimws(axis))

  switch(
    axis,
    l = "l",
    light = "l",
    m = "m",
    moisture = "m",
    n = "n",
    nutrient = "n",
    nutrients = "n",
    r = "r",
    reaction = "r",
    t = "t",
    temperature = "t",
    s = "s",
    salinity = "s",
    NA_character_
  )
}

.cluster_evidence_llm_axis_dictionary_path <- function(
    path = NULL,
    version = "v1"
) {
  if (is.null(path)) {
    path <- .cluster_evidence_llm_axis_dictionary_override_path()
  }

  if (!is.null(path)) {
    path <- .resolve_cocktailr_output_path(path)
    if (!file.exists(path)) {
      stop("LLM axis dictionary file does not exist: ", path, call. = FALSE)
    }

    return(normalizePath(path, winslash = "/", mustWork = TRUE))
  }

  file_name <- .cluster_evidence_llm_axis_dictionary_file_name(version)
  packaged <- system.file("extdata", file_name, package = "cocktailr")
  if (nzchar(packaged) && file.exists(packaged)) {
    return(normalizePath(packaged, winslash = "/", mustWork = TRUE))
  }

  root <- .cocktailr_source_root()
  if (!is.null(root) && nzchar(root)) {
    candidate <- file.path(root, "inst", "extdata", file_name)
    if (file.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }

  stop(
    "Could not locate the LLM axis dictionary file `",
    file_name,
    "`. Expected it under `inst/extdata/` in the cocktailr source tree.",
    call. = FALSE
  )
}

.validate_cluster_evidence_llm_axis_dictionary <- function(
    dictionary,
    source_path = NULL
) {
  if (!is.data.frame(dictionary)) {
    stop("`dictionary` must be a data frame.", call. = FALSE)
  }

  required <- .cluster_evidence_llm_axis_dictionary_required_columns()
  missing_columns <- setdiff(required, names(dictionary))
  if (length(missing_columns)) {
    stop(
      "LLM axis dictionary is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!nrow(dictionary)) {
    stop("LLM axis dictionary must contain at least one row.", call. = FALSE)
  }

  dictionary$axis <- vapply(
    dictionary$axis,
    .cluster_evidence_llm_normalize_axis,
    character(1)
  )

  if (anyNA(dictionary$axis)) {
    bad_rows <- which(is.na(dictionary$axis))
    stop(
      "LLM axis dictionary contains unsupported axis names at row(s): ",
      paste(bad_rows, collapse = ", "),
      ". Supported axes: ",
      paste(.cluster_evidence_llm_axis_dictionary_supported_axes(), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  dictionary$lower <- suppressWarnings(as.numeric(dictionary$lower))
  dictionary$upper <- suppressWarnings(as.numeric(dictionary$upper))
  dictionary$label <- trimws(as.character(dictionary$label))
  dictionary$phrase <- trimws(as.character(dictionary$phrase))

  if (any(!is.finite(dictionary$lower)) || any(!is.finite(dictionary$upper))) {
    stop("LLM axis dictionary `lower` and `upper` must be finite numeric values.", call. = FALSE)
  }
  if (any(dictionary$lower < 0) || any(dictionary$upper > 10)) {
    stop("LLM axis dictionary ranges must stay within the 0-10 axis scale.", call. = FALSE)
  }
  if (any(dictionary$lower >= dictionary$upper)) {
    stop("Every LLM axis dictionary row must satisfy `lower < upper`.", call. = FALSE)
  }
  if (any(!nzchar(dictionary$label))) {
    stop("Every LLM axis dictionary row must have a non-empty `label`.", call. = FALSE)
  }
  if (any(!nzchar(dictionary$phrase))) {
    stop("Every LLM axis dictionary row must have a non-empty `phrase`.", call. = FALSE)
  }

  axis_groups <- split(dictionary, dictionary$axis, drop = TRUE)
  for (axis_code in names(axis_groups)) {
    axis_table <- axis_groups[[axis_code]]
    ord <- order(axis_table$lower, axis_table$upper)
    axis_table <- axis_table[ord, , drop = FALSE]

    if (nrow(axis_table) > 1L) {
      overlaps <- axis_table$lower[-1L] < axis_table$upper[-nrow(axis_table)]
      if (any(overlaps)) {
        stop(
          "LLM axis dictionary contains overlapping intervals for axis `",
          axis_code,
          "`.",
          call. = FALSE
        )
      }
    }
  }

  attr(dictionary, "source_path") <- source_path
  dictionary
}

.read_cluster_evidence_llm_axis_dictionary <- function(
    path = NULL,
    version = "v1"
) {
  source_path <- .cluster_evidence_llm_axis_dictionary_path(
    path = path,
    version = version
  )

  dictionary <- utils::read.csv(
    source_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )

  dictionary <- .validate_cluster_evidence_llm_axis_dictionary(
    dictionary = dictionary,
    source_path = source_path
  )

  dictionary <- dictionary[order(
    dictionary$axis,
    dictionary$lower,
    dictionary$upper,
    dictionary$label,
    dictionary$phrase
  ), , drop = FALSE]
  rownames(dictionary) <- NULL
  attr(dictionary, "source_path") <- source_path
  attr(dictionary, "version") <- version
  dictionary
}

.cluster_evidence_llm_axis_dictionary_lookup <- function(
    axis,
    value,
    dictionary = NULL,
    path = NULL,
    version = "v1"
) {
  axis_code <- .cluster_evidence_llm_normalize_axis(axis)
  if (is.na(axis_code)) {
    stop(
      "Unsupported axis `",
      .as_scalar_character(axis),
      "`. Supported axes: ",
      paste(.cluster_evidence_llm_axis_dictionary_supported_axes(), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  value <- suppressWarnings(as.numeric(value))
  if (!is.finite(value)) {
    return(NULL)
  }

  dictionary <- if (is.null(dictionary)) {
    .read_cluster_evidence_llm_axis_dictionary(
      path = path,
      version = version
    )
  } else {
    .validate_cluster_evidence_llm_axis_dictionary(
      dictionary = dictionary,
      source_path = attr(dictionary, "source_path")
    )
  }

  axis_table <- dictionary[dictionary$axis == axis_code, , drop = FALSE]
  if (!nrow(axis_table)) {
    return(NULL)
  }

  axis_max_upper <- max(axis_table$upper, na.rm = TRUE)
  tolerance <- sqrt(.Machine$double.eps)
  is_last_bin <- abs(axis_table$upper - axis_max_upper) <= tolerance

  hit <- axis_table$lower <= value & (
    value < axis_table$upper |
      (is_last_bin & abs(value - axis_table$upper) <= tolerance)
  )

  hit_idx <- which(hit)
  if (!length(hit_idx)) {
    return(NULL)
  }
  if (length(hit_idx) > 1L) {
    stop(
      "LLM axis dictionary matched multiple intervals for axis `",
      axis_code,
      "` and value ",
      format(value, trim = TRUE),
      ".",
      call. = FALSE
    )
  }

  out <- axis_table[hit_idx, , drop = FALSE]
  out$axis_name <- .semantic_axis_display_name(axis_code)
  rownames(out) <- NULL
  out
}

.cluster_evidence_llm_axis_phrase <- function(
    axis,
    value,
    dictionary = NULL,
    path = NULL,
    version = "v1"
) {
  match <- .cluster_evidence_llm_axis_dictionary_lookup(
    axis = axis,
    value = value,
    dictionary = dictionary,
    path = path,
    version = version
  )

  if (is.null(match) || !nrow(match)) {
    return(NA_character_)
  }

  match$phrase[[1L]]
}
