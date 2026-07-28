# Internal helpers for Ollama transport, JSON parsing, and run-folder logging.
#
# These utilities are intentionally provider- and artifact-focused so the
# higher-level workflow functions can concentrate on stage control and repair.

.structured_stage_status <- function(parsed_output) {
  parsed_output$status %||% parsed_output$decision %||% "unknown"
}

.write_workflow_metadata <- function(log_paths, metadata) {
  if (is.null(log_paths$metadata)) {
    return(invisible(NULL))
  }
  .write_text_file(
    log_paths$metadata,
    jsonlite::toJSON(
      metadata,
      auto_unbox = TRUE,
      null = "null",
      pretty = TRUE
    )
  )
  invisible(NULL)
}

.write_workflow_final_output <- function(log_paths, final_output) {
  if (is.null(log_paths$output)) {
    return(invisible(NULL))
  }
  .write_text_file(
    log_paths$output,
    jsonlite::prettify(
      jsonlite::toJSON(final_output, auto_unbox = TRUE, null = "null")
    )
  )
  invisible(NULL)
}

.prefix_named_fields <- function(x, prefix) {
  if (!length(x)) {
    return(list())
  }

  stats::setNames(as.list(x), paste0(prefix, names(x)))
}

.attach_cluster_label_parse_info <- function(parsed_output, parse_info) {
  attr(parsed_output, "cluster_label_parse_info") <- parse_info %||% list()
  parsed_output
}

.cluster_label_parse_info <- function(parsed_output) {
  attr(parsed_output, "cluster_label_parse_info") %||% list()
}

.cluster_label_text_diagnostic_fields <- function(text, prefix = "") {
  text <- .null_default(.as_scalar_character(text), "")
  if (is.na(text)) {
    text <- ""
  }

  line_vec <- if (nzchar(text)) {
    strsplit(text, "\n", fixed = TRUE)[[1L]]
  } else {
    character(0)
  }

  .prefix_named_fields(
    c(
      chars = .cluster_evidence_prompt_char_count(text),
      words = .cluster_label_word_count(text),
      lines = length(line_vec),
      nonempty_lines = sum(nzchar(trimws(line_vec)))
    ),
    prefix
  )
}

.cluster_label_payload_diagnostic_fields <- function(payload) {
  payload <- payload %||% list()
  messages <- payload$messages %||% list()
  payload_json <- jsonlite::toJSON(
    payload,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )

  out <- c(
    .prefix_named_fields(
      c(
        message_count = length(messages),
        json_chars = .cluster_evidence_prompt_char_count(payload_json),
        json_words = .cluster_label_word_count(payload_json)
      ),
      "request_"
    )
  )

  for (role in c("system", "user", "assistant")) {
    role_contents <- vapply(messages, function(msg) {
      msg_role <- .as_scalar_character(msg$role %||% NULL)
      if (!identical(msg_role, role)) {
        return("")
      }
      .null_default(.as_scalar_character(msg$content %||% NULL), "")
    }, character(1))
    role_contents <- role_contents[nzchar(role_contents)]
    role_text <- if (length(role_contents)) {
      paste(role_contents, collapse = "\n")
    } else {
      ""
    }

    out <- c(
      out,
      .prefix_named_fields(
        c(message_count = length(role_contents)),
        paste0("request_", role, "_")
      ),
      .cluster_label_text_diagnostic_fields(
        role_text,
        prefix = paste0("request_", role, "_text_")
      )
    )
  }

  out
}

.cluster_label_parse_diagnostic_fields <- function(parsed_output) {
  if (inherits(parsed_output, "error")) {
    return(list())
  }

  parse_info <- .cluster_label_parse_info(parsed_output)
  if (!is.list(parse_info) || !length(parse_info)) {
    return(list())
  }

  out <- list(
    parse_parser_type = .as_scalar_character(parse_info$parser_type %||% NULL),
    parse_parsing_rule = .as_scalar_character(parse_info$parsing_rule %||% NULL),
    parse_code_fence_salvaged = isTRUE(parse_info$code_fence_salvaged),
    parse_nonempty_line_count = as.integer(parse_info$nonempty_line_count %||% NA_integer_),
    parse_candidate_count = as.integer(parse_info$candidate_count %||% NA_integer_)
  )

  logical_fields <- c(
    label_decision_text = "parse_extracted_label_decision_text",
    canonical_label = "parse_extracted_canonical_label",
    display_label = "parse_extracted_display_label",
    category_label = "parse_extracted_category_label",
    subcategory_labels = "parse_extracted_subcategory_labels",
    label_summary = "parse_extracted_label_summary",
    abstain_reason = "parse_extracted_abstain_reason",
    explanation = "parse_extracted_explanation"
  )

  for (nm in names(logical_fields)) {
    if (!is.null(parse_info[[nm]])) {
      out[[logical_fields[[nm]]]] <- isTRUE(parse_info[[nm]])
    }
  }

  out
}

.cluster_label_stage_text_artifact <- function(parsed_output, stage_name) {
  parse_info <- .cluster_label_parse_info(parsed_output)
  artifact <- list(
    stage_name = stage_name,
    parser_type = .as_scalar_character(parse_info$parser_type %||% NULL),
    parsing_rule = .as_scalar_character(parse_info$parsing_rule %||% NULL),
    code_fence_salvaged = isTRUE(parse_info$code_fence_salvaged)
  )

  if (identical(stage_name, "label_selection")) {
    canonical_label <- .as_scalar_character(parsed_output$canonical_label)
    display_label <- .as_scalar_character(parsed_output$display_label)
    label_summary <- .as_scalar_character(parsed_output$label_summary)
    abstain_reason <- .as_scalar_character(parsed_output$abstain_reason)

    artifact <- c(
      artifact,
      list(
        status = .as_scalar_character(parsed_output$status %||% NULL),
        canonical_label = if (.is_non_empty_scalar_character(canonical_label)) canonical_label else NULL,
        display_label = if (.is_non_empty_scalar_character(display_label)) display_label else NULL,
        label_summary = if (.is_non_empty_scalar_character(label_summary)) label_summary else NULL,
        abstain_reason = if (.is_non_empty_scalar_character(abstain_reason)) abstain_reason else NULL,
        extracted_canonical_label = isTRUE(parse_info$canonical_label),
        extracted_display_label = isTRUE(parse_info$display_label),
        extracted_label_summary = isTRUE(parse_info$label_summary),
        extracted_abstain_reason = isTRUE(parse_info$abstain_reason)
      )
    )
  } else if (identical(stage_name, "label_decision")) {
    canonical_label <- .as_scalar_character(parsed_output$canonical_label)
    display_label <- .as_scalar_character(parsed_output$display_label)
    label_decision_text <- .as_scalar_character(parsed_output$label_decision_text)

    artifact <- c(
      artifact,
      list(
        status = .as_scalar_character(parsed_output$status %||% NULL),
        label_decision_text = if (.is_non_empty_scalar_character(label_decision_text)) label_decision_text else NULL,
        canonical_label = if (.is_non_empty_scalar_character(canonical_label)) canonical_label else NULL,
        display_label = if (.is_non_empty_scalar_character(display_label)) display_label else NULL,
        extracted_label_decision_text = isTRUE(parse_info$label_decision_text),
        extracted_canonical_label = isTRUE(parse_info$canonical_label),
        extracted_display_label = isTRUE(parse_info$display_label),
        extracted_inline_abstain_reason = isTRUE(parse_info$inline_abstain_reason)
      )
    )
  } else if (identical(stage_name, "draft_analysis")) {
    draft_analysis <- .as_scalar_character(parsed_output$draft_analysis)
    artifact <- c(
      artifact,
      list(
        status = .as_scalar_character(parsed_output$status %||% NULL),
        draft_analysis = if (.is_non_empty_scalar_character(draft_analysis)) draft_analysis else NULL,
        candidate_count = as.integer(parse_info$candidate_count %||% 0L),
        candidate_labels = parse_info$candidate_labels %||% list()
      )
    )
  } else if (identical(stage_name, "category_decision")) {
    category_label <- .as_scalar_character(parsed_output$category_label)
    artifact <- c(
      artifact,
      list(
        status = .as_scalar_character(parsed_output$status %||% NULL),
        category_label = if (.is_non_empty_scalar_character(category_label)) category_label else NULL,
        extracted_category_label = isTRUE(parse_info$category_label)
      )
    )
  } else if (identical(stage_name, "subcategory_decision")) {
    subcategory_labels <- parsed_output$subcategory_labels %||% character(0)
    artifact <- c(
      artifact,
      list(
        status = .as_scalar_character(parsed_output$status %||% NULL),
        subcategory_labels = .cluster_label_stage_subcategory_text(subcategory_labels),
        extracted_subcategory_labels = isTRUE(parse_info$subcategory_labels)
      )
    )
  } else if (identical(stage_name, "post_label_subcategorization")) {
    category_label <- .as_scalar_character(parsed_output$category_label)
    subcategory_labels <- parsed_output$subcategory_labels %||% character(0)
    artifact <- c(
      artifact,
      list(
        status = .as_scalar_character(parsed_output$status %||% NULL),
        category_label = if (.is_non_empty_scalar_character(category_label)) category_label else NULL,
        subcategory_labels = .cluster_label_stage_subcategory_text(subcategory_labels),
        extracted_category_label = isTRUE(parse_info$category_label),
        extracted_subcategory_labels = isTRUE(parse_info$subcategory_labels)
      )
    )
  } else if (identical(stage_name, "label_summary")) {
    label_summary <- .as_scalar_character(parsed_output$label_summary)
    artifact <- c(
      artifact,
      list(
        status = .as_scalar_character(parsed_output$status %||% NULL),
        label_summary = if (.is_non_empty_scalar_character(label_summary)) label_summary else NULL,
        extracted_label_summary = isTRUE(parse_info$label_summary)
      )
    )
  } else if (identical(stage_name, "abstain_reason")) {
    abstain_reason <- .as_scalar_character(parsed_output$abstain_reason)
    artifact <- c(
      artifact,
      list(
        status = .as_scalar_character(parsed_output$status %||% NULL),
        abstain_reason = if (.is_non_empty_scalar_character(abstain_reason)) abstain_reason else NULL,
        extracted_abstain_reason = isTRUE(parse_info$abstain_reason)
      )
    )
  } else if (identical(stage_name, "explanation")) {
    explanation <- .as_scalar_character(parsed_output$explanation)
    artifact <- c(
      artifact,
      list(
        status = .as_scalar_character(parsed_output$status %||% NULL),
        explanation = if (.is_non_empty_scalar_character(explanation)) explanation else NULL
      )
    )
  }

  artifact
}

.cluster_label_block_diagnostic_fields <- function(blocks, block_id) {
  empty <- list(
    status = NA_character_,
    item_count_full = NA_integer_,
    item_count_used = NA_integer_,
    chars_full = NA_integer_,
    chars_used = NA_integer_
  )

  if (!is.data.frame(blocks) || !nrow(blocks) || !("id" %in% names(blocks))) {
    return(empty)
  }

  row <- blocks[blocks$id == block_id, , drop = FALSE]
  if (!nrow(row)) {
    return(empty)
  }

  list(
    status = .as_scalar_character(row$status[[1L]]),
    item_count_full = as.integer(row$item_count_full[[1L]] %||% NA_integer_),
    item_count_used = as.integer(row$item_count_used[[1L]] %||% NA_integer_),
    chars_full = as.integer(row$chars_full[[1L]] %||% NA_integer_),
    chars_used = as.integer(row$chars_used[[1L]] %||% NA_integer_)
  )
}

.cluster_label_prompt_bundle_diagnostic_fields <- function(prompt_bundle) {
  prompt_bundle <- prompt_bundle %||% list()
  budget <- prompt_bundle$evidence_budget %||% list()
  blocks <- budget$blocks %||% NULL

  out <- c(
    .cluster_label_text_diagnostic_fields(
      prompt_bundle$system %||% "",
      prefix = "prompt_system_"
    ),
    .cluster_label_text_diagnostic_fields(
      prompt_bundle$user %||% "",
      prefix = "prompt_user_"
    ),
    .cluster_label_text_diagnostic_fields(
      prompt_bundle$evidence_text %||% "",
      prefix = "prompt_evidence_used_"
    ),
    .cluster_label_text_diagnostic_fields(
      prompt_bundle$evidence_text_full %||% "",
      prefix = "prompt_evidence_full_"
    ),
    .cluster_label_text_diagnostic_fields(
      prompt_bundle$schema_prompt_text %||% "",
      prefix = "prompt_schema_"
    ),
    .cluster_label_text_diagnostic_fields(
      paste(
        .null_default(.as_scalar_character(prompt_bundle$system %||% NULL), ""),
        .null_default(.as_scalar_character(prompt_bundle$user %||% NULL), ""),
        sep = "\n"
      ),
      prefix = "prompt_total_"
    ),
    .prefix_named_fields(
      c(
        label_mode_requested = .as_scalar_character(
          prompt_bundle$label_mode_requested %||% NULL
        ),
        label_mode_effective = .as_scalar_character(
          prompt_bundle$label_mode_effective %||% NULL
        )
      ),
      "prompt_"
    )
  )

  for (block_id in c(
    "species_topological",
    "species_phi",
    "plots_membership",
    "plots_prototype",
    "plots_borderline",
    "cover_summary",
    "user_added_data",
    "semantic_axes",
    "semantic_unmatched_species",
    "limitations"
  )) {
    out <- c(
      out,
      .prefix_named_fields(
        .cluster_label_block_diagnostic_fields(blocks, block_id),
        paste0("block_", block_id, "_")
      )
    )
  }

  out
}

.cluster_label_evidence_diagnostic_fields <- function(evidence) {
  evidence <- evidence %||% list()
  summaries <- evidence$summaries %||% list()
  meta <- evidence$meta %||% list()

  n_rows_or_zero <- function(x) {
    if (is.data.frame(x)) {
      return(nrow(x))
    }
    0L
  }

  list(
    evidence_topological_species_count = n_rows_or_zero(
      summaries$species_topological %||% NULL
    ),
    evidence_phi_species_count = n_rows_or_zero(
      summaries$species_phi %||% NULL
    ),
    evidence_member_plots_count = as.integer(
      summaries$plots_membership$n_member_plots %||% 0L
    ),
    evidence_prototype_plots_count = n_rows_or_zero(
      summaries$plots_prototype %||% NULL
    ),
    evidence_borderline_plots_count = n_rows_or_zero(
      summaries$plots_borderline %||% NULL
    ),
    evidence_cover_species_count = n_rows_or_zero(
      summaries$cover_summary %||% NULL
    ),
    evidence_semantic_axes_count = n_rows_or_zero(
      summaries$semantic_axes %||% NULL
    ),
    evidence_semantic_unmatched_species_count = length(
      summaries$semantic_unmatched_species %||% character(0)
    ),
    evidence_user_added_data_present = isTRUE(
      meta$user_added_data_present %||% FALSE
    ),
    evidence_user_added_data_entry_count = as.integer(
      meta$user_added_data_entry_count %||% 0L
    ),
    evidence_user_added_data_truncated = isTRUE(
      meta$user_added_data_truncated %||% FALSE
    )
  )
}

.write_stage_attempt_diagnostics <- function(
    log_paths,
    attempt,
    evidence,
    provider,
    model,
    variant,
    stage_name,
    prompt_bundle,
    endpoint,
    payload_attempt,
    content,
    parsed_output
) {
  if (is.null(log_paths$attempt_diagnostics_prefix)) {
    return(invisible(NULL))
  }

  parsed_status <- if (inherits(parsed_output, "error")) {
    NA_character_
  } else {
    .structured_stage_status(parsed_output)
  }

  diagnostics <- c(
    list(
      started_at = log_paths$started_at,
      finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      cluster_id = evidence$meta$cluster_id,
      provider = provider,
      model = model,
      variant = variant,
      task_type = prompt_bundle$task_type %||% "label",
      stage_name = stage_name,
      schema_path = prompt_bundle$schema_path,
      catalog_path = prompt_bundle$catalog_path,
      base_url = endpoint,
      run_dir = log_paths$run_dir,
      attempt_index = as.integer(attempt),
      is_retry_attempt = as.integer(attempt) > 1L,
      parse_ok = !inherits(parsed_output, "error"),
      parsed_output_status = parsed_status,
      parse_error = if (inherits(parsed_output, "error")) {
        conditionMessage(parsed_output)
      } else {
        NA_character_
      }
    ),
    .cluster_label_evidence_diagnostic_fields(evidence),
    .cluster_label_prompt_budget_log_fields(prompt_bundle),
    .cluster_label_prompt_bundle_diagnostic_fields(prompt_bundle),
    .cluster_label_payload_diagnostic_fields(payload_attempt),
    .cluster_label_parse_diagnostic_fields(parsed_output),
    .cluster_label_text_diagnostic_fields(
      content,
      prefix = "response_content_"
    )
  )

  .write_text_file(
    paste0(
      log_paths$attempt_diagnostics_prefix,
      "_attempt",
      attempt,
      ".json"
    ),
    jsonlite::toJSON(
      diagnostics,
      auto_unbox = TRUE,
      null = "null",
      pretty = TRUE
    )
  )

  invisible(NULL)
}

.ollama_chat_request <- function(url, payload, timeout_sec) {
  body_json <- jsonlite::toJSON(
    payload,
    auto_unbox = TRUE,
    null = "null",
    pretty = FALSE
  )

  handle <- curl::new_handle()
  curl::handle_setheaders(
    handle,
    "Content-Type" = "application/json",
    "Accept" = "application/json"
  )
  curl::handle_setopt(
    handle,
    customrequest = "POST",
    postfields = body_json,
    timeout = timeout_sec
  )

  resp <- curl::curl_fetch_memory(url, handle = handle)
  body_text <- rawToChar(resp$content)

  parsed <- tryCatch(
    jsonlite::fromJSON(body_text, simplifyVector = FALSE),
    error = function(e) NULL
  )

  list(
    status_code = resp$status_code,
    body_text = body_text,
    parsed = parsed
  )
}

.ensure_ollama_envelope <- function(resp) {
  if (!is.list(resp) || is.null(resp$status_code) || is.null(resp$body_text)) {
    stop("`request_fn` must return a list with at least `status_code` and `body_text`.")
  }

  parsed <- resp$parsed %||% tryCatch(
    jsonlite::fromJSON(resp$body_text, simplifyVector = FALSE),
    error = function(e) NULL
  )

  if (resp$status_code >= 300) {
    error_message <- if (is.list(parsed) && !is.null(parsed$error)) {
      parsed$error
    } else {
      resp$body_text
    }
    stop("Ollama request failed [", resp$status_code, "]: ", error_message)
  }

  if (!is.list(parsed)) {
    stop("Ollama returned a non-JSON response envelope.")
  }

  if (!is.null(parsed$error)) {
    stop("Ollama returned an error: ", parsed$error)
  }

  list(
    status_code = resp$status_code,
    body_text = resp$body_text,
    parsed = parsed
  )
}

.extract_ollama_message_content <- function(parsed_outer) {
  content <- parsed_outer$message$content %||% NULL

  if (is.null(content) || !is.character(content) || !nzchar(trimws(content))) {
    stop("Ollama response did not contain a non-empty `message$content` field.")
  }

  content
}

.parse_cluster_label_json <- function(content, required_fields, cluster_id) {
  parsed <- jsonlite::fromJSON(content, simplifyVector = FALSE)

  if (!is.list(parsed) || is.null(names(parsed))) {
    stop("LLM output did not parse to a JSON object.")
  }

  missing_fields <- setdiff(required_fields, names(parsed))
  if (length(missing_fields)) {
    stop(
      "LLM output is missing required top-level fields: ",
      paste(missing_fields, collapse = ", "),
      "."
    )
  }

  if (!identical(parsed$cluster_id, cluster_id)) {
    stop(
      "LLM output returned cluster_id = '",
      parsed$cluster_id,
      "' but expected '",
      cluster_id,
      "'."
    )
  }

  if (!identical(parsed$status, "labeled") &&
      !identical(parsed$status, "abstain")) {
    stop("LLM output must set `status` to either 'labeled' or 'abstain'.")
  }

  parsed
}

.parse_cluster_label_gate_json <- function(content, required_fields, cluster_id) {
  parsed <- jsonlite::fromJSON(content, simplifyVector = FALSE)

  if (!is.list(parsed) || is.null(names(parsed))) {
    stop("LLM gate output did not parse to a JSON object.")
  }

  missing_fields <- setdiff(required_fields, names(parsed))
  if (length(missing_fields)) {
    stop(
      "LLM gate output is missing required top-level fields: ",
      paste(missing_fields, collapse = ", "),
      "."
    )
  }

  if (!identical(parsed$cluster_id, cluster_id)) {
    stop(
      "LLM gate output returned cluster_id = '",
      parsed$cluster_id,
      "' but expected '",
      cluster_id,
      "'."
    )
  }

  if (!identical(parsed$decision, "label") &&
      !identical(parsed$decision, "abstain")) {
    stop("LLM gate output must set `decision` to either 'label' or 'abstain'.")
  }

  parsed
}

.infer_cluster_label_selection_status <- function(parsed) {
  canonical_label <- .as_scalar_character(parsed$canonical_label)
  display_label <- .as_scalar_character(parsed$display_label)
  label_summary <- .as_scalar_character(parsed$label_summary)
  abstain_reason <- .as_scalar_character(parsed$abstain_reason)

  has_canonical <- .is_non_empty_scalar_character(canonical_label)
  has_display <- .is_non_empty_scalar_character(display_label)
  has_summary <- .is_non_empty_scalar_character(label_summary)
  has_abstain_reason <- .is_non_empty_scalar_character(abstain_reason)

  if (has_canonical || has_display) {
    if (!has_canonical || !has_display) {
      return(list(
        ok = FALSE,
        status = NULL,
        message = paste(
          "LLM label-selection output must either provide both",
          "`canonical_label` and `display_label`, or leave both null."
        )
      ))
    }
    if (!has_summary) {
      return(list(
        ok = FALSE,
        status = NULL,
        message = "LLM labeled selection output must provide non-empty `label_summary`."
      ))
    }
    if (has_abstain_reason) {
      return(list(
        ok = FALSE,
        status = NULL,
        message = paste(
          "LLM labeled selection output must set `abstain_reason` to null."
        )
      ))
    }
    return(list(ok = TRUE, status = "labeled", message = NULL))
  }

  if (has_abstain_reason) {
    if (has_summary) {
      return(list(
        ok = FALSE,
        status = NULL,
        message = paste(
          "LLM abstaining label-selection output must leave",
          "`label_summary` empty."
        )
      ))
    }
    return(list(ok = TRUE, status = "abstain", message = NULL))
  }

  list(
    ok = FALSE,
    status = NULL,
    message = paste(
      "LLM label-selection output must either provide a label triplet",
      "(`canonical_label`, `display_label`, `label_summary`) or a",
      "non-empty `abstain_reason`."
    )
  )
}

.cluster_label_selection_text_field_markers <- function() {
  c(
    "CANONICAL_LABEL:",
    "DISPLAY_LABEL:",
    "LABEL_SUMMARY:",
    "ABSTAIN_REASON:"
  )
}

.unwrap_cluster_label_text_code_fence <- function(content) {
  content <- .as_scalar_character(content)
  if (is.na(content)) {
    return(content)
  }

  normalized <- gsub("\r\n?", "\n", content, perl = TRUE)
  trimmed <- trimws(normalized)
  fence_pattern <- "^```[[:alnum:]_-]*[ \t]*\n([\\s\\S]*?)\n```$"

  if (grepl(fence_pattern, trimmed, perl = TRUE)) {
    return(sub(fence_pattern, "\\1", trimmed, perl = TRUE))
  }

  trimmed
}

.cluster_label_text_code_fence_info <- function(content) {
  content <- .as_scalar_character(content)
  if (is.na(content)) {
    return(list(text = content, code_fence_salvaged = FALSE))
  }

  normalized <- gsub("\r\n?", "\n", content, perl = TRUE)
  trimmed <- trimws(normalized)
  fence_pattern <- "^```[[:alnum:]_-]*[ \t]*\n([\\s\\S]*?)\n```$"

  if (grepl(fence_pattern, trimmed, perl = TRUE)) {
    return(list(
      text = sub(fence_pattern, "\\1", trimmed, perl = TRUE),
      code_fence_salvaged = TRUE
    ))
  }

  list(text = trimmed, code_fence_salvaged = FALSE)
}

.parse_cluster_label_selection_text <- function(content, cluster_id) {
  content <- .as_scalar_character(content)
  if (is.na(content) || !nzchar(trimws(content))) {
    stop("LLM label-selection output must be non-empty plain text.")
  }

  unwrap_info <- .cluster_label_text_code_fence_info(content)
  normalized <- unwrap_info$text
  lines <- strsplit(normalized, "\n", fixed = TRUE)[[1]]
  lines <- lines[nzchar(trimws(lines))]
  markers <- .cluster_label_selection_text_field_markers()

  if (!length(lines)) {
    stop("LLM label-selection output did not contain any selection field lines.")
  }

  if (length(lines) != length(markers)) {
    stop(
      "LLM label-selection output must contain exactly four field lines: ",
      paste(markers, collapse = ", "),
      "."
    )
  }

  values <- setNames(vector("list", length(markers)), markers)

  for (i in seq_along(markers)) {
    marker <- markers[[i]]
    line <- lines[[i]]

    if (!startsWith(line, marker)) {
      stop(
        "LLM label-selection output line ",
        i,
        " must start with `",
        marker,
        "`."
      )
    }

    value <- trimws(sub(paste0("^", marker), "", line))
    values[[marker]] <- if (nzchar(value)) value else NULL
  }

  parsed <- list(
    schema_version = "0.1.0",
    cluster_id = cluster_id,
    canonical_label = values[["CANONICAL_LABEL:"]],
    display_label = values[["DISPLAY_LABEL:"]],
    label_summary = values[["LABEL_SUMMARY:"]],
    abstain_reason = values[["ABSTAIN_REASON:"]]
  )

  inferred <- .infer_cluster_label_selection_status(parsed)
  if (!isTRUE(inferred$ok)) {
    stop(inferred$message)
  }

  parsed$status <- inferred$status
  .attach_cluster_label_parse_info(
    parsed,
    list(
      parser_type = "selection_text_v1",
      parsing_rule = "strict_four_line_markers_same_order",
      code_fence_salvaged = isTRUE(unwrap_info$code_fence_salvaged),
      nonempty_line_count = length(lines),
      canonical_label = !is.null(parsed$canonical_label),
      display_label = !is.null(parsed$display_label),
      label_summary = !is.null(parsed$label_summary),
      abstain_reason = !is.null(parsed$abstain_reason)
    )
  )
}

.parse_cluster_label_plain_text_step <- function(
    content,
    cluster_id,
    field_name,
    status,
    parser_type,
    forbid_abstain_token = FALSE
) {
  content <- .as_scalar_character(content)
  if (is.na(content) || !nzchar(trimws(content))) {
    stop("LLM plain-text stage output must be non-empty.")
  }

  unwrap_info <- .cluster_label_text_code_fence_info(content)
  text <- trimws(unwrap_info$text)
  if (!nzchar(text)) {
    stop("LLM plain-text stage output became empty after trimming.")
  }

  if (isTRUE(forbid_abstain_token) &&
      grepl("^ABSTAIN(?:\\b|[[:punct:][:space:]].*)?$", text, ignore.case = TRUE, perl = TRUE)) {
    stop(
      "LLM ",
      gsub("_", "-", field_name, fixed = TRUE),
      " output must not be the plain abstain token at this stage."
    )
  }

  out <- list(
    cluster_id = cluster_id,
    status = status
  )
  out[[field_name]] <- text

  parse_info <- list(
    parser_type = parser_type,
    parsing_rule = "plain_nonempty_text",
    code_fence_salvaged = isTRUE(unwrap_info$code_fence_salvaged),
    nonempty_line_count = sum(nzchar(strsplit(text, "\n", fixed = TRUE)[[1L]]))
  )
  parse_info[[field_name]] <- TRUE

  .attach_cluster_label_parse_info(out, parse_info)
}

.parse_cluster_label_label_decision_text <- function(content, cluster_id) {
  content <- .as_scalar_character(content)
  if (is.na(content) || !nzchar(trimws(content))) {
    stop("LLM label-decision output must be non-empty plain text.")
  }

  unwrap_info <- .cluster_label_text_code_fence_info(content)
  normalized <- unwrap_info$text
  lines <- strsplit(normalized, "\n", fixed = TRUE)[[1L]]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  if (!length(lines)) {
    stop("LLM label-decision output did not contain any usable text.")
  }

  extract_marker_value <- function(marker) {
    idx <- grep(
      paste0("^", gsub("([.^$|()\\[\\]{}*+?\\\\])", "\\\\\\1", marker, perl = TRUE)),
      lines,
      perl = TRUE,
      ignore.case = TRUE
    )
    if (!length(idx)) {
      return(NULL)
    }
    value <- trimws(sub(
      paste0("^", gsub("([.^$|()\\[\\]{}*+?\\\\])", "\\\\\\1", marker, perl = TRUE)),
      "",
      lines[[idx[[1L]]]],
      ignore.case = TRUE,
      perl = TRUE
    ))
    if (nzchar(value)) value else NULL
  }

  display_from_legacy <- extract_marker_value("DISPLAY_LABEL:")
  abstain_from_legacy <- extract_marker_value("ABSTAIN_REASON:")
  label_from_fields <- extract_marker_value("LABEL:")
  category_label <- extract_marker_value("CATEGORY_LABEL:")
  subcategory_labels <- .parse_cluster_label_subcategory_labels(
    extract_marker_value("SUBCATEGORY_LABELS:")
  )

  parse_rule <- "single_short_answer"
  inline_abstain_reason <- FALSE
  decision_text <- paste(lines, collapse = " ")
  decision_text <- gsub("\\s+", " ", trimws(decision_text), perl = TRUE)

  if (.is_non_empty_scalar_character(label_from_fields)) {
    decision_text <- label_from_fields
    parse_rule <- "experimental_category_fields"
  } else if (.is_non_empty_scalar_character(display_from_legacy)) {
    decision_text <- display_from_legacy
    parse_rule <- "legacy_display_label_salvage"
  } else if (.is_non_empty_scalar_character(abstain_from_legacy)) {
    decision_text <- "ABSTAIN"
    parse_rule <- "legacy_abstain_reason_salvage"
    inline_abstain_reason <- TRUE
  }

  if (grepl("^ABSTAIN(?:\\b|[[:punct:][:space:]].*)?$", decision_text, ignore.case = TRUE, perl = TRUE)) {
    remainder <- trimws(sub("^ABSTAIN\\b[[:punct:][:space:]]*", "", decision_text, ignore.case = TRUE, perl = TRUE))
    parsed <- list(
      schema_version = "0.1.0",
      cluster_id = cluster_id,
      status = "abstain",
      label_decision_text = if (nzchar(decision_text)) decision_text else "ABSTAIN",
      canonical_label = NULL,
      display_label = NULL,
      category_label = NULL,
      subcategory_labels = character(0)
    )

    .attach_cluster_label_parse_info(
      parsed,
      list(
        parser_type = "label_decision_text_v2",
        parsing_rule = parse_rule,
        code_fence_salvaged = isTRUE(unwrap_info$code_fence_salvaged),
        nonempty_line_count = length(lines),
        label_decision_text = TRUE,
        canonical_label = FALSE,
        display_label = FALSE,
        category_label = FALSE,
        subcategory_labels = FALSE,
        inline_abstain_reason = inline_abstain_reason || nzchar(remainder)
      )
    )
  } else {
    display_label <- .normalize_cluster_label_candidate_display(decision_text)
    canonical_label <- .cluster_label_candidate_to_canonical(display_label)

    if (is.na(display_label) || !nzchar(display_label)) {
      stop(
        "LLM label-decision output must be a short plain-text label or `ABSTAIN`. ",
        "Could not normalize a usable short label from: `",
        decision_text,
        "`."
      )
    }

    if (is.na(canonical_label) || !nzchar(canonical_label)) {
      stop(
        "LLM label-decision output produced a label that could not be normalized into canonical snake_case: `",
        decision_text,
        "`."
      )
    }

    parsed <- list(
      schema_version = "0.1.0",
      cluster_id = cluster_id,
      status = "labeled",
      label_decision_text = display_label,
      canonical_label = canonical_label,
      display_label = display_label,
      category_label = if (.is_non_empty_scalar_character(category_label)) {
        trimws(category_label)
      } else {
        NULL
      },
      subcategory_labels = subcategory_labels
    )

    .attach_cluster_label_parse_info(
      parsed,
      list(
        parser_type = "label_decision_text_v2",
        parsing_rule = parse_rule,
        code_fence_salvaged = isTRUE(unwrap_info$code_fence_salvaged),
        nonempty_line_count = length(lines),
        label_decision_text = TRUE,
        canonical_label = TRUE,
        display_label = TRUE,
        category_label = .is_non_empty_scalar_character(category_label),
        subcategory_labels = length(subcategory_labels) > 0L,
        inline_abstain_reason = FALSE
      )
    )
  }
}

.cluster_label_stage_subcategory_text <- function(x) {
  x <- .as_character_vector(x)
  x <- trimws(x)
  x <- x[nzchar(x)]
  if (!length(x)) {
    return(NULL)
  }
  paste(x, collapse = "; ")
}

.parse_cluster_label_subcategory_labels <- function(text) {
  text <- .as_scalar_character(text)
  if (is.na(text) || !nzchar(trimws(text))) {
    return(character(0))
  }

  parts <- unlist(strsplit(text, "\\s*[;|]\\s*", perl = TRUE), use.names = FALSE)
  parts <- trimws(parts)
  unique(parts[nzchar(parts)])
}

.parse_cluster_label_category_decision_text <- function(content, cluster_id) {
  parsed <- .parse_cluster_label_clean_name_text(
    content = content,
    cluster_id = cluster_id,
    stage_name = "category_decision",
    field_name = "category_label",
    allow_semicolon = FALSE,
    allow_none = FALSE,
    allow_abstain = TRUE
  )

  status <- if (identical(toupper(parsed$value), "ABSTAIN")) {
    "abstain"
  } else {
    "category_ready"
  }

  out <- list(
    schema_version = "0.1.0",
    cluster_id = cluster_id,
    status = status,
    category_label = if (identical(status, "category_ready")) parsed$value else NULL,
    label_decision_text = if (identical(status, "abstain")) "ABSTAIN" else parsed$value
  )

  .attach_cluster_label_parse_info(
    out,
    c(
      parsed$parse_info,
      list(category_label = identical(status, "category_ready"))
    )
  )
}

.parse_cluster_label_subcategory_decision_text <- function(content, cluster_id) {
  parsed <- .parse_cluster_label_clean_name_text(
    content = content,
    cluster_id = cluster_id,
    stage_name = "subcategory_decision",
    field_name = "subcategory_labels",
    allow_semicolon = TRUE,
    allow_none = TRUE,
    allow_abstain = FALSE
  )

  subcategory_labels <- if (identical(tolower(parsed$value), "none")) {
    character(0)
  } else {
    parts <- unlist(strsplit(parsed$value, "\\s*;\\s*", perl = TRUE), use.names = FALSE)
    parts <- trimws(parts)
    parts[nzchar(parts)]
  }

  out <- list(
    schema_version = "0.1.0",
    cluster_id = cluster_id,
    status = "subcategory_ready",
    subcategory_labels = unique(subcategory_labels)
  )

  .attach_cluster_label_parse_info(
    out,
    c(
      parsed$parse_info,
      list(subcategory_labels = length(subcategory_labels) > 0L)
    )
  )
}

.parse_cluster_label_general_name_decision_text <- function(content, cluster_id) {
  parsed <- .parse_cluster_label_clean_name_text(
    content = content,
    cluster_id = cluster_id,
    stage_name = "general_name_decision",
    field_name = "category_label",
    allow_semicolon = FALSE,
    allow_none = FALSE,
    allow_abstain = TRUE
  )

  status <- if (identical(toupper(parsed$value), "ABSTAIN")) {
    "abstain"
  } else {
    "category_ready"
  }

  out <- list(
    schema_version = "0.1.0",
    cluster_id = cluster_id,
    status = status,
    category_label = if (identical(status, "category_ready")) parsed$value else NULL,
    label_decision_text = if (identical(status, "abstain")) "ABSTAIN" else parsed$value
  )

  .attach_cluster_label_parse_info(
    out,
    c(
      parsed$parse_info,
      list(category_label = identical(status, "category_ready"))
    )
  )
}

.parse_cluster_label_uniqueness_detail_decision_text <- function(content, cluster_id) {
  parsed <- .parse_cluster_label_clean_name_text(
    content = content,
    cluster_id = cluster_id,
    stage_name = "uniqueness_detail_decision",
    field_name = "subcategory_labels",
    allow_semicolon = FALSE,
    allow_none = TRUE,
    allow_abstain = FALSE
  )

  details <- if (identical(tolower(parsed$value), "none")) {
    character(0)
  } else {
    parsed$value
  }

  out <- list(
    schema_version = "0.1.0",
    cluster_id = cluster_id,
    status = "subcategory_ready",
    subcategory_labels = details
  )

  .attach_cluster_label_parse_info(
    out,
    c(
      parsed$parse_info,
      list(subcategory_labels = length(details) > 0L)
    )
  )
}

.parse_cluster_label_post_label_subcategorization_text <- function(content, cluster_id) {
  unwrap_info <- .cluster_label_text_code_fence_info(content)
  text <- unwrap_info$text
  lines <- unlist(strsplit(text, "\n", fixed = TRUE), use.names = FALSE)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  field_value <- function(prefix_pattern) {
    hit <- grep(
      paste0("^\\**\\s*", prefix_pattern, "\\s*\\**\\s*:"),
      lines,
      ignore.case = TRUE,
      perl = TRUE,
      value = TRUE
    )
    if (!length(hit)) {
      return(NULL)
    }
    value <- trimws(sub("^[^:]+:\\s*", "", hit[[1L]], perl = TRUE))
    trimws(gsub("^\\**|\\**$", "", value, perl = TRUE))
  }

  group_name <- field_value("GENERAL[ _]+GROUP[ _]+NAME")
  details_text <- field_value("UNIQUENESS[ _]+DETAILS")

  if (!.is_non_empty_scalar_character(group_name)) {
    stop("LLM post-label subcategorization output must provide GENERAL_GROUP_NAME.")
  }

  .validate_cluster_label_clean_name_value(
    group_name,
    stage_name = "post_label_subcategorization",
    field_name = "general_group_name",
    allow_semicolon = FALSE,
    allow_none = FALSE,
    allow_abstain = FALSE
  )

  details <- if (is.null(details_text) ||
                 !nzchar(trimws(details_text)) ||
                 identical(tolower(trimws(details_text)), "none")) {
    character(0)
  } else {
    .parse_cluster_label_subcategory_labels(details_text)
  }

  if (length(details)) {
    for (detail in details) {
      .validate_cluster_label_clean_name_value(
        detail,
        stage_name = "post_label_subcategorization",
        field_name = "uniqueness_details",
        allow_semicolon = FALSE,
        allow_none = FALSE,
        allow_abstain = FALSE
      )
    }
  }

  out <- list(
    schema_version = "0.1.0",
    cluster_id = cluster_id,
    status = "subcategorization_ready",
    category_label = group_name,
    subcategory_labels = details
  )

  .attach_cluster_label_parse_info(
    out,
    list(
      parser_type = "post_label_subcategorization_text_v1",
      parsing_rule = "general_group_and_uniqueness_fields",
      code_fence_salvaged = isTRUE(unwrap_info$code_fence_salvaged),
      nonempty_line_count = length(lines),
      category_label = TRUE,
      subcategory_labels = length(details) > 0L
    )
  )
}

.parse_cluster_label_clean_name_text <- function(
    content,
    cluster_id,
    stage_name,
    field_name,
    allow_semicolon,
    allow_none,
    allow_abstain
) {
  content <- .as_scalar_character(content)
  if (is.na(content) || !nzchar(trimws(content))) {
    stop("LLM ", stage_name, " output must be a non-empty clean name.")
  }

  unwrap_info <- .cluster_label_text_code_fence_info(content)
  if (isTRUE(unwrap_info$code_fence_salvaged)) {
    stop("LLM ", stage_name, " output must not use code fences.")
  }

  normalized <- trimws(unwrap_info$text)
  lines <- strsplit(normalized, "\n", fixed = TRUE)[[1L]]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  if (stage_name %in% c("post_label_uniqueness", "uniqueness_detail_decision")) {
    if (length(lines) < 1L) {
      stop("LLM ", stage_name, " output must contain at least one non-empty line.")
    }
    value <- paste(lines, collapse = " ")
  } else if (length(lines) != 1L) {
    stop("LLM ", stage_name, " output must contain exactly one non-empty line.")
  } else {
    value <- lines[[1L]]
  }

  value <- gsub("\\s+", " ", value, perl = TRUE)
  value <- trimws(value)

  if (stage_name %in% c("post_label_uniqueness", "uniqueness_detail_decision")) {
    forbidden_prefix_pattern <- "^(LABEL|CATEGORY|CATEGORY_LABEL|SUBCATEGORY|SUBCATEGORY_LABELS)\\s*:"
    if (grepl(forbidden_prefix_pattern, value, ignore.case = TRUE, perl = TRUE)) {
      stop("LLM ", stage_name, " output must not include technical prefixes.")
    }
    value <- .normalize_cluster_label_post_label_uniqueness_value(value)
  }

  .validate_cluster_label_clean_name_value(
    value = value,
    stage_name = stage_name,
    field_name = field_name,
    allow_semicolon = allow_semicolon,
    allow_none = allow_none,
    allow_abstain = allow_abstain
  )

  list(
    value = value,
    parse_info = list(
      parser_type = paste0(stage_name, "_clean_text_v1"),
      parsing_rule = "clean_single_answer",
      code_fence_salvaged = FALSE,
      nonempty_line_count = length(lines),
      cluster_id = cluster_id
    )
  )
}

.validate_cluster_label_clean_name_value <- function(
    value,
    stage_name,
    field_name,
    allow_semicolon,
    allow_none,
    allow_abstain
) {
  if (!nzchar(value)) {
    stop("LLM ", stage_name, " output must not be empty.")
  }

  forbidden_prefix_pattern <- "^(LABEL|CATEGORY|CATEGORY_LABEL|SUBCATEGORY|SUBCATEGORY_LABELS)\\s*:"
  if (grepl(forbidden_prefix_pattern, value, ignore.case = TRUE, perl = TRUE)) {
    stop("LLM ", stage_name, " output must not include technical prefixes.")
  }

  if (stage_name %in% c("post_label_uniqueness", "uniqueness_detail_decision")) {
    value <- .normalize_cluster_label_post_label_uniqueness_value(value)
    if (!nzchar(value)) {
      stop("LLM ", stage_name, " output must not be empty after normalization.")
    }
  }

  lower <- tolower(value)
  if (identical(lower, "none")) {
    if (isTRUE(allow_none)) {
      return(invisible(TRUE))
    }
    stop("LLM ", stage_name, " output must not use `none` for ", field_name, ".")
  }

  if (identical(toupper(value), "ABSTAIN")) {
    if (isTRUE(allow_abstain)) {
      return(invisible(TRUE))
    }
    stop("LLM ", stage_name, " output must not abstain at this stage.")
  }

  if (grepl("^['\"`].*['\"`]$", value, perl = TRUE)) {
    stop("LLM ", stage_name, " output must not wrap the name in quotes.")
  }

  if (grepl("^\\s*([-*]|[0-9]+[.)])\\s+", value, perl = TRUE)) {
    stop("LLM ", stage_name, " output must not use bullets or numbered lists.")
  }

  if (grepl("\\b(because|due to|based on|indicating|with evidence|the category is|the subcategory is)\\b", value, ignore.case = TRUE, perl = TRUE)) {
    stop("LLM ", stage_name, " output must not include explanatory prose.")
  }

  if (grepl("[_:,()\\[\\]{}]", value, perl = TRUE)) {
    stop("LLM ", stage_name, " output contains forbidden punctuation.")
  }

  if (grepl("\\.$", value, perl = TRUE)) {
    stop("LLM ", stage_name, " output must not end with a period.")
  }

  if (!isTRUE(allow_semicolon) && grepl(";", value, fixed = TRUE)) {
    stop("LLM ", stage_name, " output must not contain semicolons.")
  }

  if (grepl("\\b[A-Z]{2,}\\b", value, perl = TRUE)) {
    stop("LLM ", stage_name, " output must not contain all-caps marker text.")
  }

  allowed_pattern <- if (isTRUE(allow_semicolon)) {
    "^[[:alpha:]][[:alpha:][:digit:] /;'-]*[[:alpha:][:digit:]]$"
  } else {
    "^[[:alpha:]][[:alpha:][:digit:] /'-]*[[:alpha:][:digit:]]$"
  }
  if (!grepl(allowed_pattern, value, perl = TRUE)) {
    stop("LLM ", stage_name, " output is not a clean ecological name.")
  }

  invisible(TRUE)
}

.normalize_cluster_label_post_label_uniqueness_value <- function(value) {
  value <- gsub("[;_:,()\\[\\]{}\\.]+", " ", value, perl = TRUE)
  value <- gsub("[^[:alpha:][:digit:] /'-]+", " ", value, perl = TRUE)
  value <- gsub("\\s+", " ", value, perl = TRUE)
  trimws(value)
}

.parse_cluster_label_summary_text <- function(content, cluster_id) {
  .parse_cluster_label_plain_text_step(
    content = content,
    cluster_id = cluster_id,
    field_name = "label_summary",
    status = "summary_ready",
    parser_type = "label_summary_text_v2",
    forbid_abstain_token = TRUE
  )
}

.parse_cluster_label_abstain_reason_text <- function(content, cluster_id) {
  .parse_cluster_label_plain_text_step(
    content = content,
    cluster_id = cluster_id,
    field_name = "abstain_reason",
    status = "abstain_reason_ready",
    parser_type = "abstain_reason_text_v2"
  )
}

.parse_cluster_label_selection_json <- function(content, required_fields, cluster_id) {
  parsed <- jsonlite::fromJSON(content, simplifyVector = FALSE)

  if (!is.list(parsed) || is.null(names(parsed))) {
    stop("LLM label-selection output did not parse to a JSON object.")
  }

  missing_fields <- setdiff(required_fields, names(parsed))
  if (length(missing_fields)) {
    stop(
      "LLM label-selection output is missing required top-level fields: ",
      paste(missing_fields, collapse = ", "),
      "."
    )
  }

  if (!identical(parsed$cluster_id, cluster_id)) {
    stop(
      "LLM label-selection output returned cluster_id = '",
      parsed$cluster_id,
      "' but expected '",
      cluster_id,
      "'."
    )
  }

  inferred <- .infer_cluster_label_selection_status(parsed)
  if (!isTRUE(inferred$ok)) {
    stop(inferred$message)
  }

  explicit_status <- .as_scalar_character(parsed$status)
  if (!is.na(explicit_status) && nzchar(explicit_status)) {
    if (!explicit_status %in% c("labeled", "abstain")) {
      stop(
        "LLM label-selection output must set `status` to either ",
        "'labeled' or 'abstain' when the field is present."
      )
    }
    if (!identical(explicit_status, inferred$status)) {
      stop(
        "LLM label-selection output returned an explicit `status` that ",
        "does not match the label/abstain fields."
      )
    }
  }

  parsed$status <- inferred$status
  parsed
}

.parse_cluster_label_explanation_text <- function(content, cluster_id) {
  .parse_cluster_label_plain_text_step(
    content = content,
    cluster_id = cluster_id,
    field_name = "explanation",
    status = "explanation_ready",
    parser_type = "explanation_text_v1"
  )
}

.parse_cluster_label_draft_text <- function(content, cluster_id) {
  content <- .as_scalar_character(content)
  if (is.na(content) || !nzchar(trimws(content))) {
    stop("LLM draft-analysis output must be non-empty text.")
  }

  draft_analysis <- trimws(content)
  candidates <- .extract_cluster_label_candidates_from_draft(draft_analysis)

  .attach_cluster_label_parse_info(
    list(
      cluster_id = cluster_id,
      status = "draft_ready",
      draft_analysis = draft_analysis
    ),
    list(
      parser_type = "draft_text_v1",
      parsing_rule = "plain_nonempty_text",
      nonempty_line_count = sum(nzchar(strsplit(draft_analysis, "\n", fixed = TRUE)[[1]])),
      candidate_count = length(candidates),
      candidate_labels = candidates
    )
  )
}

.init_cluster_label_logs <- function(log_dir, cluster_id, model, variant) {
  if (is.null(log_dir)) {
    return(list(
      dir = NULL,
      date_dir = NULL,
      run_dir = NULL,
      started_at = NULL,
      metadata = NULL,
      evidence = NULL,
      system_prompt = NULL,
      user_prompt = NULL,
      request = NULL,
      request_prefix = NULL,
      response_prefix = NULL,
      response_content_prefix = NULL,
      attempt_diagnostics_prefix = NULL,
      parsed_text_fields = NULL,
      output = NULL,
      error = NULL
    ))
  }

  log_dir <- .resolve_cocktailr_output_path(log_dir)
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  started_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  date_part <- format(Sys.Date(), "%Y-%m-%d")
  time_part <- format(Sys.time(), "%H%M%S")
  run_stem <- paste(
    time_part,
    .safe_file_stub(cluster_id),
    .safe_file_stub(model),
    .safe_file_stub(variant),
    sep = "_"
  )
  root_dir <- normalizePath(log_dir, winslash = "/", mustWork = TRUE)
  date_dir <- file.path(root_dir, date_part)
  dir.create(date_dir, recursive = TRUE, showWarnings = FALSE)
  run_dir <- .unique_run_dir(date_dir, run_stem)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

  list(
    dir = root_dir,
    date_dir = normalizePath(date_dir, winslash = "/", mustWork = TRUE),
    run_dir = normalizePath(run_dir, winslash = "/", mustWork = TRUE),
    started_at = started_at,
    metadata = file.path(run_dir, "metadata.json"),
    evidence = file.path(run_dir, "evidence.txt"),
    system_prompt = file.path(run_dir, "system_prompt.md"),
    user_prompt = file.path(run_dir, "user_prompt.md"),
    request = file.path(run_dir, "request.json"),
    request_prefix = file.path(run_dir, "request"),
    response_prefix = file.path(run_dir, "response"),
    response_content_prefix = file.path(run_dir, "response_content"),
    attempt_diagnostics_prefix = file.path(run_dir, "attempt_diagnostics"),
    parsed_text_fields = file.path(run_dir, "parsed_text_fields.json"),
    output = file.path(run_dir, "parsed_output.json"),
    error = file.path(run_dir, "error.txt")
  )
}

.stage_log_paths <- function(stage_dir, started_at = NULL) {
  dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)
  run_dir <- normalizePath(stage_dir, winslash = "/", mustWork = TRUE)

  list(
    dir = dirname(run_dir),
    date_dir = dirname(run_dir),
    run_dir = run_dir,
    started_at = started_at,
    metadata = file.path(run_dir, "metadata.json"),
    evidence = file.path(run_dir, "evidence.txt"),
    system_prompt = file.path(run_dir, "system_prompt.md"),
    user_prompt = file.path(run_dir, "user_prompt.md"),
    request = file.path(run_dir, "request.json"),
    request_prefix = file.path(run_dir, "request"),
    response_prefix = file.path(run_dir, "response"),
    response_content_prefix = file.path(run_dir, "response_content"),
    attempt_diagnostics_prefix = file.path(run_dir, "attempt_diagnostics"),
    parsed_text_fields = file.path(run_dir, "parsed_text_fields.json"),
    output = file.path(run_dir, "parsed_output.json"),
    error = file.path(run_dir, "error.txt")
  )
}

.null_stage_log_paths <- function() {
  list(
    dir = NULL,
    date_dir = NULL,
    run_dir = NULL,
    started_at = NULL,
    metadata = NULL,
    evidence = NULL,
    system_prompt = NULL,
    user_prompt = NULL,
    request = NULL,
    request_prefix = NULL,
    response_prefix = NULL,
    response_content_prefix = NULL,
    attempt_diagnostics_prefix = NULL,
    parsed_text_fields = NULL,
    output = NULL,
    error = NULL
  )
}

.cluster_label_workflow_stage_names <- function(workflow_steps) {
  workflow_steps <- .arg_workflow_steps(workflow_steps, "workflow_steps")

  if (identical(workflow_steps, 2L)) {
    return(c("gate", "label"))
  }

  if (identical(workflow_steps, 3L)) {
    return(c("draft", "label", "summary", "abstain_reason", "subcategorization"))
  }

  "label"
}

.init_cluster_label_workflow_logs <- function(
    log_dir,
    cluster_id,
    model,
    variant,
    workflow_steps
) {
  stage_names <- .cluster_label_workflow_stage_names(workflow_steps)

  if (is.null(log_dir)) {
    return(list(
      dir = NULL,
      date_dir = NULL,
      run_dir = NULL,
      started_at = NULL,
      metadata = NULL,
      output = NULL,
      error = NULL,
      stages = stats::setNames(
        lapply(stage_names, function(...) .null_stage_log_paths()),
        stage_names
      )
    ))
  }

  log_dir <- .resolve_cocktailr_output_path(log_dir)
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  started_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  date_part <- format(Sys.Date(), "%Y-%m-%d")
  time_part <- format(Sys.time(), "%H%M%S")
  run_stem <- paste(
    time_part,
    .safe_file_stub(cluster_id),
    .safe_file_stub(model),
    .safe_file_stub(variant),
    paste0("w", workflow_steps),
    sep = "_"
  )
  root_dir <- normalizePath(log_dir, winslash = "/", mustWork = TRUE)
  date_dir <- file.path(root_dir, date_part)
  dir.create(date_dir, recursive = TRUE, showWarnings = FALSE)
  run_dir <- .unique_run_dir(date_dir, run_stem)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  run_dir <- normalizePath(run_dir, winslash = "/", mustWork = TRUE)

  list(
    dir = root_dir,
    date_dir = normalizePath(date_dir, winslash = "/", mustWork = TRUE),
    run_dir = run_dir,
    started_at = started_at,
    metadata = file.path(run_dir, "metadata.json"),
    output = file.path(run_dir, "parsed_output.json"),
    error = file.path(run_dir, "error.txt"),
    stages = stats::setNames(
      lapply(seq_along(stage_names), function(i) {
        .stage_log_paths(
          file.path(
            run_dir,
            paste0("stage", i, "_", stage_names[[i]])
          ),
          started_at = started_at
        )
      }),
      stage_names
    )
  )
}

.safe_file_stub <- function(x) {
  gsub("[^A-Za-z0-9._-]+", "_", x)
}

.unique_run_dir <- function(parent_dir, base_name) {
  candidate <- file.path(parent_dir, base_name)
  if (!dir.exists(candidate) && !file.exists(candidate)) {
    return(candidate)
  }

  for (i in seq_len(999L)) {
    candidate_i <- file.path(parent_dir, paste0(base_name, "_", sprintf("%02d", i)))
    if (!dir.exists(candidate_i) && !file.exists(candidate_i)) {
      return(candidate_i)
    }
  }

  stop("Could not allocate a unique LLM log run directory in ", parent_dir)
}

.write_text_file <- function(path, text) {
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(enc2utf8(text)), con)
}
