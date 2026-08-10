.life_form_overlay_dictionary_file_name <- function(version = "v1") {
  version <- .arg_scalar_character(version, "version")
  paste0("life_form_overlay_dictionary_", version, ".csv")
}

.life_form_overlay_dictionary_required_columns <- function() {
  c("metric", "lower", "upper", "label", "phrase")
}

.life_form_overlay_dictionary_supported_metrics <- function() {
  c(
    "dominant_life_form_share",
    "life_form_richness",
    "fixed_assignment_share",
    "mixed_assignment_share",
    "unmatched_species_share",
    "species_to_life_form_compression"
  )
}

.life_form_overlay_dictionary_override_path <- function() {
  path <- getOption("cocktailr.life_form_overlay_dictionary_path", NULL)
  if (is.null(path)) {
    return(NULL)
  }

  .arg_scalar_character(
    path,
    'getOption("cocktailr.life_form_overlay_dictionary_path")'
  )
}

.life_form_overlay_dictionary_path <- function(path = NULL, version = "v1") {
  if (is.null(path)) {
    path <- .life_form_overlay_dictionary_override_path()
  }

  if (!is.null(path)) {
    path <- .resolve_cocktailr_output_path(path)
    if (!file.exists(path)) {
      stop("Life-form overlay dictionary file does not exist: ", path, call. = FALSE)
    }

    return(normalizePath(path, winslash = "/", mustWork = TRUE))
  }

  file_name <- .life_form_overlay_dictionary_file_name(version)
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
    "Could not locate the life-form overlay dictionary file `",
    file_name,
    "`. Expected it under `inst/extdata/` in the cocktailr source tree.",
    call. = FALSE
  )
}

.validate_life_form_overlay_dictionary <- function(
    dictionary,
    source_path = NULL
) {
  if (!is.data.frame(dictionary)) {
    stop("`dictionary` must be a data frame.", call. = FALSE)
  }

  required <- .life_form_overlay_dictionary_required_columns()
  missing_columns <- setdiff(required, names(dictionary))
  if (length(missing_columns)) {
    stop(
      "Life-form overlay dictionary is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!nrow(dictionary)) {
    stop("Life-form overlay dictionary must contain at least one row.", call. = FALSE)
  }

  dictionary$metric <- trimws(as.character(dictionary$metric))
  dictionary$lower <- suppressWarnings(as.numeric(dictionary$lower))
  dictionary$upper <- suppressWarnings(as.numeric(dictionary$upper))
  dictionary$label <- trimws(as.character(dictionary$label))
  dictionary$phrase <- trimws(as.character(dictionary$phrase))

  if (any(!dictionary$metric %in% .life_form_overlay_dictionary_supported_metrics())) {
    bad_rows <- which(!dictionary$metric %in% .life_form_overlay_dictionary_supported_metrics())
    stop(
      "Life-form overlay dictionary contains unsupported metric names at row(s): ",
      paste(bad_rows, collapse = ", "),
      ". Supported metrics: ",
      paste(.life_form_overlay_dictionary_supported_metrics(), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (any(!is.finite(dictionary$lower)) || any(!is.finite(dictionary$upper))) {
    stop(
      "Life-form overlay dictionary `lower` and `upper` must be finite numeric values.",
      call. = FALSE
    )
  }
  if (any(dictionary$lower < 0)) {
    stop("Life-form overlay dictionary ranges must be non-negative.", call. = FALSE)
  }
  if (any(dictionary$lower >= dictionary$upper)) {
    stop(
      "Every life-form overlay dictionary row must satisfy `lower < upper`.",
      call. = FALSE
    )
  }
  if (any(!nzchar(dictionary$label))) {
    stop(
      "Every life-form overlay dictionary row must have a non-empty `label`.",
      call. = FALSE
    )
  }
  if (any(!nzchar(dictionary$phrase))) {
    stop(
      "Every life-form overlay dictionary row must have a non-empty `phrase`.",
      call. = FALSE
    )
  }

  metric_groups <- split(dictionary, dictionary$metric, drop = TRUE)
  for (metric_name in names(metric_groups)) {
    metric_table <- metric_groups[[metric_name]]
    ord <- order(metric_table$lower, metric_table$upper)
    metric_table <- metric_table[ord, , drop = FALSE]

    if (nrow(metric_table) > 1L) {
      overlaps <- metric_table$lower[-1L] < metric_table$upper[-nrow(metric_table)]
      if (any(overlaps)) {
        stop(
          "Life-form overlay dictionary contains overlapping intervals for metric `",
          metric_name,
          "`. ",
          call. = FALSE
        )
      }
    }
  }

  attr(dictionary, "source_path") <- source_path
  dictionary
}

.read_life_form_overlay_dictionary <- function(path = NULL, version = "v1") {
  source_path <- .life_form_overlay_dictionary_path(
    path = path,
    version = version
  )

  dictionary <- utils::read.csv(
    source_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )

  dictionary <- .validate_life_form_overlay_dictionary(
    dictionary = dictionary,
    source_path = source_path
  )

  dictionary <- dictionary[order(
    dictionary$metric,
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

.life_form_overlay_dictionary_lookup <- function(
    metric,
    value,
    dictionary = NULL,
    path = NULL,
    version = "v1"
) {
  metric <- .arg_scalar_character(metric, "metric")
  if (!metric %in% .life_form_overlay_dictionary_supported_metrics()) {
    stop(
      "Unsupported metric `",
      metric,
      "`. Supported metrics: ",
      paste(.life_form_overlay_dictionary_supported_metrics(), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  value <- suppressWarnings(as.numeric(value))
  if (!is.finite(value)) {
    return(NULL)
  }

  dictionary <- if (is.null(dictionary)) {
    .read_life_form_overlay_dictionary(
      path = path,
      version = version
    )
  } else {
    .validate_life_form_overlay_dictionary(
      dictionary = dictionary,
      source_path = attr(dictionary, "source_path")
    )
  }

  metric_table <- dictionary[dictionary$metric == metric, , drop = FALSE]
  if (!nrow(metric_table)) {
    return(NULL)
  }

  max_upper <- max(metric_table$upper, na.rm = TRUE)
  tolerance <- sqrt(.Machine$double.eps)
  is_last_bin <- abs(metric_table$upper - max_upper) <= tolerance
  in_bin <- (value >= metric_table$lower) &
    ((value < metric_table$upper) | (is_last_bin & value <= metric_table$upper))
  matched <- metric_table[in_bin, , drop = FALSE]

  if (!nrow(matched)) {
    return(NULL)
  }
  if (nrow(matched) > 1L) {
    stop(
      "Life-form overlay dictionary matched multiple intervals for metric `",
      metric,
      "` at value ",
      format(value),
      ".",
      call. = FALSE
    )
  }

  matched
}
