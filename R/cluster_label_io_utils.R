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

  if (!identical(parsed$status, "labeled") &&
      !identical(parsed$status, "abstain")) {
    stop(
      "LLM label-selection output must set `status` to either ",
      "'labeled' or 'abstain'."
    )
  }

  parsed
}

.parse_cluster_label_draft_text <- function(content, cluster_id) {
  content <- .as_scalar_character(content)
  if (is.na(content) || !nzchar(trimws(content))) {
    stop("LLM draft-analysis output must be non-empty text.")
  }

  list(
    cluster_id = cluster_id,
    status = "draft_ready",
    draft_analysis = trimws(content)
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
      response_prefix = NULL,
      response_content_prefix = NULL,
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
    response_prefix = file.path(run_dir, "response"),
    response_content_prefix = file.path(run_dir, "response_content"),
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
    response_prefix = file.path(run_dir, "response"),
    response_content_prefix = file.path(run_dir, "response_content"),
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
    response_prefix = NULL,
    response_content_prefix = NULL,
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
    return(c("draft", "label", "explanation"))
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
