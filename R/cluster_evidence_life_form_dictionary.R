.life_form_dictionary_file_name <- function(version = "v1") {
  version <- .arg_scalar_character(version, "version")
  paste0("life_form_dictionary_", version, ".csv")
}

.life_form_dictionary_required_columns <- function() {
  c("raw_flag", "label", "phrase", "priority")
}

.life_form_dictionary_override_path <- function() {
  path <- getOption("cocktailr.life_form_dictionary_path", NULL)
  if (is.null(path)) {
    return(NULL)
  }

  .arg_scalar_character(
    path,
    'getOption("cocktailr.life_form_dictionary_path")'
  )
}

.life_form_dictionary_path <- function(
    path = NULL,
    version = "v1"
) {
  if (is.null(path)) {
    path <- .life_form_dictionary_override_path()
  }

  if (!is.null(path)) {
    path <- .resolve_cocktailr_output_path(path)
    if (!file.exists(path)) {
      stop("Life-form dictionary file does not exist: ", path, call. = FALSE)
    }

    return(normalizePath(path, winslash = "/", mustWork = TRUE))
  }

  file_name <- .life_form_dictionary_file_name(version)
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
    "Could not locate the life-form dictionary file `",
    file_name,
    "`. Expected it under `inst/extdata/` in the cocktailr source tree.",
    call. = FALSE
  )
}

.validate_life_form_dictionary <- function(
    dictionary,
    source_path = NULL
) {
  if (!is.data.frame(dictionary)) {
    stop("`dictionary` must be a data frame.", call. = FALSE)
  }

  required <- .life_form_dictionary_required_columns()
  missing_columns <- setdiff(required, names(dictionary))
  if (length(missing_columns)) {
    stop(
      "Life-form dictionary is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!nrow(dictionary)) {
    stop("Life-form dictionary must contain at least one row.", call. = FALSE)
  }

  dictionary$raw_flag <- .clean_names_simple(dictionary$raw_flag)
  dictionary$label <- trimws(as.character(dictionary$label))
  dictionary$phrase <- trimws(as.character(dictionary$phrase))
  dictionary$priority <- suppressWarnings(as.integer(dictionary$priority))

  if (any(!nzchar(dictionary$raw_flag))) {
    stop("Every life-form dictionary row must have a non-empty `raw_flag`.", call. = FALSE)
  }
  if (any(duplicated(dictionary$raw_flag))) {
    stop("Life-form dictionary `raw_flag` values must be unique.", call. = FALSE)
  }
  if (any(!nzchar(dictionary$label))) {
    stop("Every life-form dictionary row must have a non-empty `label`.", call. = FALSE)
  }
  if (any(!nzchar(dictionary$phrase))) {
    stop("Every life-form dictionary row must have a non-empty `phrase`.", call. = FALSE)
  }
  if (any(!is.finite(dictionary$priority))) {
    stop("Life-form dictionary `priority` must contain finite integer values.", call. = FALSE)
  }

  dictionary <- dictionary[order(
    dictionary$priority,
    dictionary$label,
    dictionary$raw_flag
  ), , drop = FALSE]
  rownames(dictionary) <- NULL
  attr(dictionary, "source_path") <- source_path
  dictionary
}

.read_life_form_dictionary <- function(
    path = NULL,
    version = "v1"
) {
  source_path <- .life_form_dictionary_path(
    path = path,
    version = version
  )

  dictionary <- utils::read.csv(
    source_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )

  dictionary <- .validate_life_form_dictionary(
    dictionary = dictionary,
    source_path = source_path
  )

  attr(dictionary, "source_path") <- source_path
  attr(dictionary, "version") <- version
  dictionary
}
