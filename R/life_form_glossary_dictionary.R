.life_form_glossary_dictionary_file_name <- function(version = "v1") {
  version <- .arg_scalar_character(version, "version")
  paste0("life_form_glossary_dictionary_", version, ".csv")
}

.life_form_glossary_dictionary_required_columns <- function() {
  c(
    "label",
    "short_definition",
    "interpretation_hint",
    "source_name",
    "source_url"
  )
}

.life_form_glossary_dictionary_path <- function(
    path = NULL,
    version = "v1"
) {
  if (!is.null(path)) {
    path <- .resolve_cocktailr_output_path(path)
    if (!file.exists(path)) {
      stop(
        "Life-form glossary dictionary file does not exist: ",
        path,
        call. = FALSE
      )
    }

    return(normalizePath(path, winslash = "/", mustWork = TRUE))
  }

  file_name <- .life_form_glossary_dictionary_file_name(version)
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
    "Could not locate the life-form glossary dictionary file `",
    file_name,
    "`. Expected it under `inst/extdata/` in the cocktailr source tree.",
    call. = FALSE
  )
}

.validate_life_form_glossary_dictionary <- function(
    dictionary,
    source_path = NULL
) {
  if (!is.data.frame(dictionary)) {
    stop("`dictionary` must be a data frame.", call. = FALSE)
  }

  required <- .life_form_glossary_dictionary_required_columns()
  missing_columns <- setdiff(required, names(dictionary))
  if (length(missing_columns)) {
    stop(
      "Life-form glossary dictionary is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!nrow(dictionary)) {
    stop(
      "Life-form glossary dictionary must contain at least one row.",
      call. = FALSE
    )
  }

  dictionary$label <- trimws(as.character(dictionary$label))
  dictionary$short_definition <- trimws(as.character(dictionary$short_definition))
  dictionary$interpretation_hint <- trimws(as.character(dictionary$interpretation_hint))
  dictionary$source_name <- trimws(as.character(dictionary$source_name))
  dictionary$source_url <- trimws(as.character(dictionary$source_url))

  if (any(!nzchar(dictionary$label))) {
    stop(
      "Every life-form glossary row must have a non-empty `label`.",
      call. = FALSE
    )
  }
  if (any(duplicated(dictionary$label))) {
    stop(
      "Life-form glossary dictionary `label` values must be unique.",
      call. = FALSE
    )
  }
  if (any(!nzchar(dictionary$short_definition))) {
    stop(
      "Every life-form glossary row must have a non-empty `short_definition`.",
      call. = FALSE
    )
  }
  if (any(!nzchar(dictionary$interpretation_hint))) {
    stop(
      "Every life-form glossary row must have a non-empty `interpretation_hint`.",
      call. = FALSE
    )
  }

  rownames(dictionary) <- NULL
  attr(dictionary, "source_path") <- source_path
  dictionary
}

.read_life_form_glossary_dictionary <- function(path = NULL, version = "v1") {
  source_path <- .life_form_glossary_dictionary_path(
    path = path,
    version = version
  )

  dictionary <- utils::read.csv(
    source_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )

  dictionary <- .validate_life_form_glossary_dictionary(
    dictionary = dictionary,
    source_path = source_path
  )

  attr(dictionary, "source_path") <- source_path
  attr(dictionary, "version") <- version
  dictionary
}
