.cluster_label_validation_label_tier <- function(validation) {
  tier <- .as_scalar_character(validation$label_tier)
  if (!is.na(tier) && nzchar(tier)) {
    return(tier)
  }

  if (isTRUE(validation$is_speculative)) {
    return("speculative")
  }

  if (identical(validation$output_status, "labeled")) {
    return("accepted")
  }

  NA_character_
}

.cluster_label_validation_plot_marker <- function(validation) {
  marker <- .as_scalar_character(validation$plot_marker)
  if (!is.na(marker) && nzchar(marker)) {
    return(marker)
  }

  if (isTRUE(validation$is_speculative)) {
    return("*")
  }

  ""
}

.cluster_label_display_with_marker <- function(label, marker = "") {
  label <- .as_scalar_character(label)
  marker <- .as_scalar_character(marker)

  if (is.na(label) || !nzchar(label)) {
    return(NA_character_)
  }
  if (is.na(marker) || !nzchar(marker)) {
    return(label)
  }

  paste0(label, marker)
}

.cluster_label_missing_for_confidence_from_output <- function(output) {
  if (!is.list(output) || !length(output)) {
    return(NA_character_)
  }

  items <- output$not_confirmed_by_data %||% list()
  if (!is.list(items) || !length(items)) {
    return(NA_character_)
  }

  parts <- vapply(items, function(item) {
    statement <- .as_scalar_character(item$statement)
    reason <- .as_scalar_character(item$reason)

    if (.is_non_empty_scalar_character(statement) &&
        .is_non_empty_scalar_character(reason)) {
      return(paste0(statement, " (", reason, ")"))
    }
    if (.is_non_empty_scalar_character(statement)) {
      return(statement)
    }
    if (.is_non_empty_scalar_character(reason)) {
      return(reason)
    }

    NA_character_
  }, character(1))

  parts <- unique(parts[!is.na(parts) & nzchar(parts)])
  if (!length(parts)) {
    return(NA_character_)
  }

  paste(parts, collapse = "; ")
}
