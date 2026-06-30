#' Label one Cocktail cluster with a local LLM via Ollama
#'
#' @description
#' Builds a prompt from a \code{\link{cluster_evidence}} object, the packaged
#' JSON output schema, and a named prompt variant from the packaged prompt
#' catalog, then calls a local Ollama model to obtain one structured label /
#' interpretation result.
#'
#' The function is intentionally evidence-first and schema-first:
#' \itemize{
#'   \item the evidence object is the only data-derived input to the model
#'   \item the model is asked to return raw JSON that matches the packaged schema
#'   \item malformed outputs can be retried with a repair instruction
#' }
#'
#' This function currently supports \code{provider = "ollama"}.
#'
#' @param evidence A \code{"cluster_evidence"} object, typically produced by
#'   \code{\link{cluster_evidence}}.
#' @param provider Character scalar. Currently only \code{"ollama"} is
#'   supported.
#' @param model Character scalar naming the local model to use, for example
#'   \code{"gemma4:12b"}.
#' @param variant Character scalar naming the label-stage prompt variant from
#'   the packaged prompt catalog. The current main public values are
#'   \code{"label_primary_v1"}, \code{"label_soft_v1"}, and
#'   \code{"label_broad_v1"}. Older versioned prompt IDs are still accepted as
#'   compatibility aliases, but they resolve to the current public trio.
#' @param label_mode Character scalar controlling how freely the model may
#'   choose labels. Use \code{"open"} (default) for unrestricted label
#'   generation, \code{"constrained"} to force labeled answers to come from
#'   the packaged coarse vocabulary or a user override set via
#'   \code{options(cocktailr.cluster_label_vocabulary_path = ...)}, and
#'   \code{"dynamic"} to reuse candidate labels extracted from the draft stage.
#'   Dynamic mode currently requires \code{workflow_steps = 3}.
#' @param base_url Character scalar. Base URL of the Ollama server. Default
#'   reads \code{getOption("cocktailr.ollama_base_url", "http://localhost:11434")}.
#' @param schema_path Optional path to a JSON schema file. By default the
#'   packaged schema in \code{inst/schemas/cluster_label_output_schema.json} is
#'   used.
#' @param temperature Optional numeric override for generation temperature.
#'   Defaults to the selected prompt variant.
#' @param top_p Optional numeric override for \code{top_p}. Defaults to the
#'   selected prompt variant.
#' @param seed Optional integer override for generation seed. Default
#'   \code{42} when supported by the provider.
#' @param num_predict Optional integer override for the generation length cap.
#'   Default \code{2400} from the packaged prompt catalog.
#' @param prompt_budget_chars Optional integer character budget for the final
#'   assembled prompt messages. Default \code{10000}. Use \code{NULL} to keep
#'   the full evidence text without prompt-budget trimming.
#' @param keep_alive Optional Ollama keep-alive value, for example \code{"5m"}.
#' @param ollama_options Optional named list of additional Ollama generation
#'   options passed through under \code{request$options}. Use this for model-
#'   specific controls such as \code{num_ctx}. Explicit top-level arguments such
#'   as \code{temperature}, \code{top_p}, \code{seed}, and \code{num_predict}
#'   still take precedence over the same names inside \code{ollama_options}.
#' @param timeout_sec Numeric request timeout in seconds. Default \code{120}.
#' @param max_retries Integer >= 0. Number of repair retries after malformed
#'   JSON or missing required top-level fields. Default \code{1}.
#' @param workflow_steps Integer workflow length. Use \code{1} (default) for
#'   the existing one-step label workflow. Use \code{2} for a gate-then-label
#'   workflow where stage 1 decides \code{label / abstain} and stage 2 is
#'   executed only if the gate allows labeling. Use \code{3} for the staged
#'   draft-analysis -> label-selection -> explanation workflow, where the
#'   model first writes a freeform draft, then selects a short label through
#'   the public prompt ladder, and only after that produces the final
#'   structured explanation.
#' @param dry_run Logical; if \code{TRUE}, return the assembled prompt bundle
#'   and Ollama request payload without making a network request.
#' @param log_dir Optional directory for writing request / response artifacts.
#'   If \code{NULL} (default), no files are written. When a relative path is
#'   used and a local \code{cocktailr} source checkout can be detected, it is
#'   resolved against that package root.
#' @param request_fn Optional expert/testing override for the provider request
#'   function. It must accept \code{(url, payload, timeout_sec)} and return a
#'   list with components \code{status_code}, \code{body_text}, and optionally
#'   \code{parsed}.
#'
#' @return
#' If \code{dry_run = TRUE}, returns a list of class
#' \code{"cluster_label_request"} with assembled prompt and request payload.
#'
#' Otherwise returns a list of class \code{"cluster_label_result"} with:
#' \itemize{
#'   \item \code{cluster_id}
#'   \item \code{provider}
#'   \item \code{model}
#'   \item \code{variant}
#'   \item \code{prompt}
#'   \item \code{request}
#'   \item \code{response}
#'   \item \code{output}
#'   \item \code{attempts}
#'   \item \code{workflow_steps}
#'   \item \code{schema_path}
#'   \item \code{logs}
#'   \item \code{workflow} (present for multi-step workflows; for
#'     \code{workflow_steps = 2} it contains gate and label stages, and for
#'     \code{workflow_steps = 3} it contains draft, label-selection, and
#'     explanation stages)
#' }
#'
#' @examples
#' syn <- generate_synthetic_vegetation_data(
#'   n_plots_per_community = 4,
#'   n_transition_plots = 2,
#'   seed = 42
#' )
#' res <- cocktail_cluster(
#'   vegmatrix = syn$wide_matrix,
#'   progress = FALSE,
#'   plot_values = "rel_cover",
#'   species_cluster_phi = TRUE,
#'   save_vegmatrix = TRUE
#' )
#' ev <- cluster_evidence(res, cluster = "c_1", top_n_phi = 3)
#'
#' req <- llm_label_cluster(
#'   evidence = ev,
#'   model = "gemma4:12b",
#'   variant = "label_primary_v1",
#'   dry_run = TRUE
#' )
#'
#' names(req)
#' req$request$model
#'
#' @export
llm_label_cluster <- function(
    evidence,
    provider = "ollama",
    model,
    variant = "label_primary_v1",
    label_mode = "open",
    base_url = getOption("cocktailr.ollama_base_url", "http://localhost:11434"),
    schema_path = NULL,
    temperature = NULL,
    top_p = NULL,
    seed = NULL,
    num_predict = NULL,
    prompt_budget_chars = 10000L,
    keep_alive = NULL,
    ollama_options = NULL,
    timeout_sec = 120,
    max_retries = 1L,
    workflow_steps = 1L,
    dry_run = FALSE,
    log_dir = NULL,
    request_fn = NULL
) {
  if (!inherits(evidence, "cluster_evidence")) {
    stop("`evidence` must inherit from class `cluster_evidence`.")
  }

  provider <- .arg_scalar_character(provider, "provider")
  model <- .arg_scalar_character(model, "model")
  variant <- .arg_scalar_character(variant, "variant")
  label_mode <- .arg_cluster_label_mode(label_mode, "label_mode")

  if (!provider %in% "ollama") {
    stop("Only provider = 'ollama' is currently supported.")
  }

  max_retries <- .arg_non_negative_integer(max_retries, "max_retries")
  workflow_steps <- .arg_workflow_steps(workflow_steps, "workflow_steps")
  prompt_budget_chars <- .arg_nullable_positive_integer(
    prompt_budget_chars,
    "prompt_budget_chars"
  )
  ollama_options <- .arg_named_list_or_null(ollama_options, "ollama_options")
  request_fn <- request_fn %||% .ollama_chat_request

  if (identical(label_mode, "dynamic") && !identical(workflow_steps, 3L)) {
    stop("`label_mode = \"dynamic\"` currently requires `workflow_steps = 3`.")
  }

  if (identical(workflow_steps, 1L)) {
    return(.llm_label_cluster_one_step(
      evidence = evidence,
      provider = provider,
      model = model,
      variant = variant,
      label_mode = label_mode,
      base_url = base_url,
      schema_path = schema_path,
      temperature = temperature,
      top_p = top_p,
      seed = seed,
      num_predict = num_predict,
      prompt_budget_chars = prompt_budget_chars,
      keep_alive = keep_alive,
      ollama_options = ollama_options,
      timeout_sec = timeout_sec,
      max_retries = max_retries,
      dry_run = dry_run,
      log_dir = log_dir,
      request_fn = request_fn
    ))
  }

  if (identical(workflow_steps, 2L)) {
    return(.llm_label_cluster_two_step(
      evidence = evidence,
      provider = provider,
      model = model,
      variant = variant,
      label_mode = label_mode,
      base_url = base_url,
      schema_path = schema_path,
      temperature = temperature,
      top_p = top_p,
      seed = seed,
      num_predict = num_predict,
      prompt_budget_chars = prompt_budget_chars,
      keep_alive = keep_alive,
      ollama_options = ollama_options,
      timeout_sec = timeout_sec,
      max_retries = max_retries,
      dry_run = dry_run,
      log_dir = log_dir,
      request_fn = request_fn
    ))
  }

  .llm_label_cluster_three_step(
    evidence = evidence,
    provider = provider,
    model = model,
    variant = variant,
    label_mode = label_mode,
    base_url = base_url,
    schema_path = schema_path,
    temperature = temperature,
    top_p = top_p,
    seed = seed,
    num_predict = num_predict,
    prompt_budget_chars = prompt_budget_chars,
    keep_alive = keep_alive,
    ollama_options = ollama_options,
    timeout_sec = timeout_sec,
    max_retries = max_retries,
    dry_run = dry_run,
    log_dir = log_dir,
    request_fn = request_fn
  )
}

.default_structured_stage_repair_instruction <- function() {
  paste(
    "Your previous reply was invalid JSON or did not satisfy the",
    "required top-level contract.",
    "Return one repaired JSON object only.",
    "Do not add markdown, commentary, or code fences.",
    "Use the same schema and the same evidence IDs."
  )
}

.default_draft_stage_repair_instruction <- function() {
  paste(
    "Your previous reply was empty or unusable.",
    "Return non-empty plain text draft analysis only.",
    "Do not return JSON or code fences."
  )
}

.run_structured_llm_stage <- function(
    evidence,
    provider,
    model,
    variant,
    prompt_bundle,
    keep_alive,
    ollama_options,
    endpoint,
    timeout_sec,
    max_retries,
    request_fn,
    log_paths,
    parse_output_fn,
    stage_name,
    repair_instruction = .default_structured_stage_repair_instruction()
) {
  request_payload <- .build_ollama_label_request(
    model = model,
    prompt_bundle = prompt_bundle,
    keep_alive = keep_alive,
    ollama_options = ollama_options
  )

  request_json <- jsonlite::toJSON(
    request_payload,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )

  if (!is.null(log_paths$metadata)) {
    .write_text_file(
      log_paths$metadata,
      jsonlite::toJSON(
        c(
          list(
            started_at = log_paths$started_at,
            cluster_id = evidence$meta$cluster_id,
            provider = provider,
            model = model,
            variant = variant,
            task_type = prompt_bundle$task_type %||% "label",
            stage_name = stage_name,
            schema_path = prompt_bundle$schema_path,
            catalog_path = prompt_bundle$catalog_path,
            base_url = endpoint,
            status = "started"
          ),
          .cluster_label_prompt_budget_log_fields(prompt_bundle)
        ),
        auto_unbox = TRUE,
        null = "null",
        pretty = TRUE
      )
    )
  }

  if (!is.null(log_paths$evidence)) {
    .write_text_file(log_paths$evidence, prompt_bundle$evidence_text)
  }

  if (!is.null(log_paths$system_prompt)) {
    .write_text_file(log_paths$system_prompt, prompt_bundle$system)
  }

  if (!is.null(log_paths$user_prompt)) {
    .write_text_file(log_paths$user_prompt, prompt_bundle$user)
  }

  if (!is.null(log_paths$request)) {
    .write_text_file(log_paths$request, request_json)
  }

  messages_current <- request_payload$messages
  last_error <- NULL

  # Keep stage-level retries local to one prompt bundle. Structural repair
  # happens by appending the invalid reply and a constrained correction
  # instruction to the existing message stack.
  for (attempt in seq_len(max_retries + 1L)) {
    payload_attempt <- request_payload
    payload_attempt$messages <- messages_current

    resp <- request_fn(endpoint, payload_attempt, timeout_sec)
    parsed_outer <- .ensure_ollama_envelope(resp)

    if (!is.null(log_paths$response_prefix)) {
      .write_text_file(
        paste0(log_paths$response_prefix, "_attempt", attempt, "_envelope.json"),
        parsed_outer$body_text
      )
    }

    content <- .extract_ollama_message_content(parsed_outer$parsed)
    if (!is.null(log_paths$response_content_prefix)) {
      .write_text_file(
        paste0(log_paths$response_content_prefix, "_attempt", attempt, ".txt"),
        content
      )
    }

    parsed_output <- tryCatch(
      parse_output_fn(content),
      error = function(e) e
    )

    if (!inherits(parsed_output, "error")) {
      if (!is.null(log_paths$output)) {
        .write_text_file(
          log_paths$output,
          jsonlite::prettify(
            jsonlite::toJSON(parsed_output, auto_unbox = TRUE, null = "null")
          )
        )
      }

      if (!is.null(log_paths$metadata)) {
        .write_text_file(
          log_paths$metadata,
          jsonlite::toJSON(
            c(
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
                status = "success",
                attempts = attempt,
                output_status = .structured_stage_status(parsed_output),
                run_dir = log_paths$run_dir
              ),
              .cluster_label_prompt_budget_log_fields(prompt_bundle)
            ),
            auto_unbox = TRUE,
            null = "null",
            pretty = TRUE
          )
        )
      }

      return(list(
        cluster_id = evidence$meta$cluster_id,
        provider = provider,
        model = model,
        variant = variant,
        prompt = prompt_bundle,
        request = payload_attempt,
        response = list(
          status_code = parsed_outer$status_code,
          envelope = parsed_outer$parsed,
          raw = parsed_outer$body_text,
          content = content
        ),
        output = parsed_output,
        attempts = attempt,
        schema_path = prompt_bundle$schema_path,
        logs = log_paths
      ))
    }

    last_error <- parsed_output

    if (attempt > max_retries) {
      break
    }

    messages_current <- c(
      messages_current,
      list(
        list(role = "assistant", content = content),
        list(role = "user", content = repair_instruction)
      )
    )
  }

  if (!is.null(log_paths$error)) {
    .write_text_file(
      log_paths$error,
      paste(
        "Failed to obtain a valid structured LLM stage result.",
        paste0("Stage: ", stage_name),
        paste0("Attempts: ", max_retries + 1L),
        paste0("Error: ", conditionMessage(last_error)),
        sep = "\n"
      )
    )
  }

  if (!is.null(log_paths$metadata)) {
    .write_text_file(
      log_paths$metadata,
      jsonlite::toJSON(
        c(
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
            status = "failed",
            attempts = max_retries + 1L,
            run_dir = log_paths$run_dir,
            error = conditionMessage(last_error)
          ),
          .cluster_label_prompt_budget_log_fields(prompt_bundle)
        ),
        auto_unbox = TRUE,
        null = "null",
        pretty = TRUE
      )
    )
  }

  stop(
    "Failed to obtain a valid structured LLM stage result after ",
    max_retries + 1L,
    " attempt(s): ",
    conditionMessage(last_error)
  )
}

.cluster_label_prompt_budget_log_fields <- function(prompt_bundle) {
  budget <- prompt_bundle$evidence_budget %||% NULL
  if (is.null(budget)) {
    return(list())
  }

  list(
    prompt_budget_chars = budget$prompt_budget_chars %||% NULL,
    fixed_overhead_chars = budget$fixed_overhead_chars %||% NULL,
    schema_prompt_chars = budget$schema_prompt_chars %||% NULL,
    schema_text_chars = budget$schema_text_chars %||% NULL,
    evidence_budget_chars = budget$evidence_budget_chars %||% NULL,
    evidence_chars_full = budget$evidence_chars_full %||% NULL,
    evidence_chars_used = budget$evidence_chars_used %||% NULL,
    repair_previous_json_chars = budget$repair_previous_json_chars %||% NULL,
    total_prompt_chars = budget$total_prompt_chars %||% NULL,
    fits_within_budget = budget$fits_within_budget %||% NULL,
    fixed_overhead_exceeds_budget = budget$fixed_overhead_exceeds_budget %||% NULL,
    evidence_trimmed = budget$trimmed %||% NULL,
    evidence_kept_block_ids = budget$kept_block_ids %||% character(0),
    evidence_dropped_block_ids = budget$dropped_block_ids %||% character(0),
    evidence_truncated_block_ids = budget$truncated_block_ids %||% character(0),
    evidence_blocks = budget$blocks %||% NULL,
    repair_mode = budget$repair_mode %||% prompt_bundle$repair_prompt_type %||% NULL
  )
}

.gate_decision_to_cluster_label_output <- function(gate_output, evidence) {
  list(
    schema_version = "0.1.0",
    cluster_id = evidence$meta$cluster_id,
    status = "abstain",
    canonical_label = NULL,
    display_label = NULL,
    interpretation_summary = gate_output$decision_summary,
    basis_in_data = list(),
    key_species = gate_output$key_species %||% list(),
    external_knowledge = list(),
    not_confirmed_by_data = gate_output$not_confirmed_by_data %||% list(),
    confidence = gate_output$confidence,
    checks_to_run = gate_output$checks_to_run %||% list(),
    abstain_reason = gate_output$abstain_reason %||% gate_output$decision_summary,
    ontology_slots = gate_output$ontology_slots %||% NULL,
    contrastive_notes = NULL,
    report_recommendation = list(
      report_priority = "low",
      merge_candidate = NULL,
      notes = list("Abstained at workflow gate before final labeling.")
    )
  )
}

.cluster_label_selection_output_is_valid <- function(output) {
  status <- .as_scalar_character(output$status)
  canonical_label <- .as_scalar_character(output$canonical_label)
  display_label <- .as_scalar_character(output$display_label)
  label_summary <- .as_scalar_character(output$label_summary)
  abstain_reason <- .as_scalar_character(output$abstain_reason)

  if (!.is_non_empty_scalar_character(label_summary)) {
    return(list(ok = FALSE, message = "label_summary must be non-empty."))
  }

  if (identical(status, "abstain")) {
    if (!is.null(output$canonical_label) || !is.null(output$display_label)) {
      return(list(
        ok = FALSE,
        message = "abstaining selection outputs must set canonical_label and display_label to null."
      ))
    }
    if (!.is_non_empty_scalar_character(abstain_reason)) {
      return(list(ok = FALSE, message = "abstaining selection outputs must provide abstain_reason."))
    }
    return(list(ok = TRUE, message = NULL))
  }

  if (!identical(status, "labeled")) {
    return(list(ok = FALSE, message = "selection status must be either labeled or abstain."))
  }

  if (!.is_non_empty_scalar_character(canonical_label)) {
    return(list(ok = FALSE, message = "labeled selection outputs must provide canonical_label."))
  }
  if (!grepl("^[a-z0-9_]+$", canonical_label)) {
    return(list(ok = FALSE, message = "canonical_label must use lowercase snake_case."))
  }
  if (nchar(canonical_label, type = "chars") > .cluster_label_max_canonical_length()) {
    return(list(ok = FALSE, message = "canonical_label is too long for the final contract."))
  }

  if (!.is_non_empty_scalar_character(display_label)) {
    return(list(ok = FALSE, message = "labeled selection outputs must provide display_label."))
  }

  display_label_trimmed <- trimws(display_label)
  if (nchar(display_label_trimmed, type = "chars") > .cluster_label_max_display_length()) {
    return(list(ok = FALSE, message = "display_label is too long for the final contract."))
  }
  if (.cluster_label_word_count(display_label_trimmed) > .cluster_label_max_display_words()) {
    return(list(ok = FALSE, message = "display_label has too many words for the final contract."))
  }
  if (grepl(
    .cluster_label_forbidden_display_punctuation_pattern(),
    display_label_trimmed,
    perl = TRUE
  )) {
    return(list(ok = FALSE, message = "display_label contains forbidden punctuation."))
  }
  if (grepl("\\.$", display_label_trimmed, perl = TRUE)) {
    return(list(ok = FALSE, message = "display_label must not end with a period."))
  }
  if (!is.null(output$abstain_reason)) {
    return(list(ok = FALSE, message = "labeled selection outputs must set abstain_reason to null."))
  }

  list(ok = TRUE, message = NULL)
}

.cluster_label_prompt_contract_is_valid <- function(output, prompt_bundle) {
  if (!is.list(output) || !is.list(prompt_bundle)) {
    return(list(ok = TRUE, message = NULL))
  }

  status <- .as_scalar_character(output$status)
  if (!identical(status, "labeled")) {
    return(list(ok = TRUE, message = NULL))
  }

  label_mode <- .as_scalar_character(prompt_bundle$label_mode_effective %||% "open")
  canonical_label <- .as_scalar_character(output$canonical_label)
  display_label <- .as_scalar_character(output$display_label)

  if (identical(label_mode, "open")) {
    return(list(ok = TRUE, message = NULL))
  }

  if (identical(label_mode, "constrained")) {
    labels <- prompt_bundle$vocabulary_object$labels %||% list()
    allowed_canonical <- vapply(labels, function(x) {
      .as_scalar_character(x$canonical_label)
    }, character(1))
    allowed_display <- vapply(labels, function(x) {
      .as_scalar_character(x$display_label)
    }, character(1))

    match_idx <- match(canonical_label, allowed_canonical)
    if (is.na(match_idx)) {
      return(list(
        ok = FALSE,
        message = paste0(
          "labeled outputs in constrained mode must choose canonical_label from the configured vocabulary. Received `",
          canonical_label,
          "`."
        )
      ))
    }

    expected_display <- allowed_display[[match_idx]]
    if (.is_non_empty_scalar_character(expected_display) &&
        !identical(display_label, expected_display)) {
      return(list(
        ok = FALSE,
        message = paste0(
          "display_label must match the configured vocabulary entry for `",
          canonical_label,
          "`."
        )
      ))
    }

    return(list(ok = TRUE, message = NULL))
  }

  if (identical(label_mode, "dynamic")) {
    candidates <- prompt_bundle$dynamic_candidates %||% list()
    allowed_canonical <- vapply(candidates, function(x) {
      .as_scalar_character(x$canonical_label)
    }, character(1))

    if (!canonical_label %in% allowed_canonical) {
      return(list(
        ok = FALSE,
        message = paste0(
          "dynamic mode must reuse one of the draft-derived candidate canonical labels. Received `",
          canonical_label,
          "`."
        )
      ))
    }

    return(list(ok = TRUE, message = NULL))
  }

  list(ok = TRUE, message = NULL)
}

.assert_cluster_label_prompt_contract <- function(output, prompt_bundle, stage_name) {
  contract <- .cluster_label_prompt_contract_is_valid(output, prompt_bundle)
  if (!isTRUE(contract$ok)) {
    stop(
      "Prompt contract failed for ",
      stage_name,
      ": ",
      contract$message
    )
  }
  output
}

.cluster_label_selection_fallback_output <- function(evidence, failure_messages) {
  failure_messages <- .as_character_vector(failure_messages)
  fallback_reason <- if (length(failure_messages)) {
    paste(failure_messages, collapse = " | ")
  } else {
    "Every label-selection rung abstained or produced unusable label fields."
  }

  list(
    schema_version = "0.1.0",
    cluster_id = evidence$meta$cluster_id,
    status = "labeled",
    canonical_label = "chaotic_cluster",
    display_label = "chaotic cluster",
    label_summary = paste(
      "The selection ladder exhausted stricter options, so the workflow",
      "fell back to a broad noisy-cluster label."
    ),
    abstain_reason = NULL,
    fallback_reason = fallback_reason
  )
}

.cluster_label_summary_first_scalar <- function(summary_table, field) {
  if (!is.data.frame(summary_table) || !nrow(summary_table) || !field %in% names(summary_table)) {
    return(NA_character_)
  }
  .as_scalar_character(summary_table[[field]][[1]])
}

.cluster_label_selection_fallback_evidence_ids <- function(evidence) {
  topo <- evidence$summaries$species_topological %||% NULL
  phi <- evidence$summaries$species_phi %||% NULL
  proto <- evidence$summaries$plots_prototype %||% NULL
  cover <- evidence$summaries$cover_summary %||% NULL

  ids <- c(
    .cluster_label_summary_first_scalar(topo, "evidence_id"),
    .cluster_label_summary_first_scalar(phi, "evidence_id"),
    .cluster_label_summary_first_scalar(proto, "evidence_id"),
    .cluster_label_summary_first_scalar(cover, "evidence_id")
  )
  ids <- ids[!is.na(ids) & nzchar(ids)]

  evidence_index <- .cluster_evidence_index(evidence)
  if (nrow(evidence_index)) {
    non_limitation <- evidence_index$id[
      !is.na(evidence_index$id) &
        nzchar(evidence_index$id) &
        (is.na(evidence_index$type) | evidence_index$type != "limitation")
    ]
    ids <- c(ids, non_limitation)

    if (!length(ids)) {
      ids <- c(ids, evidence_index$id[!is.na(evidence_index$id) & nzchar(evidence_index$id)])
    }
  }

  unique(ids[!is.na(ids) & grepl("^E[0-9]+$", ids)])
}

.cluster_label_selection_fallback_key_species <- function(evidence) {
  topo <- evidence$summaries$species_topological %||% NULL
  phi <- evidence$summaries$species_phi %||% NULL

  topo_species <- .cluster_label_summary_first_scalar(topo, "species")
  topo_id <- .cluster_label_summary_first_scalar(topo, "evidence_id")
  phi_species <- .cluster_label_summary_first_scalar(phi, "species")
  phi_id <- .cluster_label_summary_first_scalar(phi, "evidence_id")

  entries <- list()

  if (.is_non_empty_scalar_character(topo_species) && .is_non_empty_scalar_character(topo_id)) {
    entries[[length(entries) + 1L]] <- list(
      species = topo_species,
      role = "topological",
      evidence_ids = c(topo_id)
    )
  }

  if (.is_non_empty_scalar_character(phi_species) &&
      .is_non_empty_scalar_character(phi_id) &&
      !identical(phi_species, topo_species)) {
    entries[[length(entries) + 1L]] <- list(
      species = phi_species,
      role = "phi_ranked",
      evidence_ids = c(phi_id)
    )
  }

  entries
}

.cluster_label_selection_fallback_final_output <- function(
    evidence,
    selection_output,
    failure_messages
) {
  selection_output <- selection_output %||% list()
  failure_messages <- .as_character_vector(failure_messages)

  fallback_reason <- .as_scalar_character(selection_output$fallback_reason)
  if (is.na(fallback_reason) || !nzchar(trimws(fallback_reason))) {
    fallback_reason <- if (length(failure_messages)) {
      paste(failure_messages, collapse = " | ")
    } else {
      "Every label-selection rung abstained or produced unusable label fields."
    }
  }

  evidence_ids <- utils::head(
    .cluster_label_selection_fallback_evidence_ids(evidence),
    3L
  )
  key_species <- .cluster_label_selection_fallback_key_species(evidence)

  topo_species <- .cluster_label_summary_first_scalar(
    evidence$summaries$species_topological %||% NULL,
    "species"
  )
  phi_species <- .cluster_label_summary_first_scalar(
    evidence$summaries$species_phi %||% NULL,
    "species"
  )

  species_note <- character(0)
  if (.is_non_empty_scalar_character(topo_species)) {
    species_note <- c(species_note, topo_species)
  }
  if (.is_non_empty_scalar_character(phi_species) && !identical(phi_species, topo_species)) {
    species_note <- c(species_note, phi_species)
  }
  species_note <- unique(species_note)
  canonical_label <- .as_scalar_character(selection_output$canonical_label)
  if (is.na(canonical_label) || !nzchar(canonical_label)) {
    canonical_label <- "chaotic_cluster"
  }
  display_label <- .as_scalar_character(selection_output$display_label)
  if (is.na(display_label) || !nzchar(display_label)) {
    display_label <- "chaotic cluster"
  }

  basis_statement <- if (length(species_note)) {
    paste(
      "The evidence bundle still points to a recurring compositional signal around",
      paste(species_note, collapse = " and "),
      "but the public label-selection cascade could not compress that signal into a stable short label."
    )
  } else {
    paste(
      "The evidence bundle still contains a recurring compositional signal,",
      "but the public label-selection cascade could not compress it into a stable short label."
    )
  }

  list(
    schema_version = "0.1.0",
    cluster_id = evidence$meta$cluster_id,
    status = "labeled",
    canonical_label = canonical_label,
    display_label = display_label,
    interpretation_summary = paste(
      "The staged labeling workflow exhausted the primary, soft, and broad",
      "selection rungs, so it finalized a broad chaotic-cluster placeholder",
      "for manual review instead of asking the model for another full explanation pass."
    ),
    basis_in_data = list(
      list(
        claim_id = "C1",
        statement = basis_statement,
        evidence_ids = evidence_ids,
        support_strength = "weak"
      )
    ),
    key_species = key_species,
    external_knowledge = list(),
    not_confirmed_by_data = list(
      list(
        statement = "A more specific ecological label is not confirmed.",
        reason = fallback_reason
      )
    ),
    confidence = list(
      score = 0.15,
      rationale = paste(
        "This broad fallback label is intentionally low-confidence because",
        "the entire selection cascade was exhausted."
      )
    ),
    checks_to_run = list(
      list(
        check = "Review this cluster manually before using the label as interpretation.",
        priority = "high",
        reason = fallback_reason
      )
    ),
    abstain_reason = NULL,
    report_recommendation = list(
      report_priority = "high",
      merge_candidate = NULL,
      notes = list(
        "Selection cascade exhausted all public rungs; human review is required."
      )
    )
  )
}

.cluster_label_explanation_placeholders <- function() {
  list(
    draft_analysis = paste(
      c(
        "Possible interpretations:",
        "- mixed meadow assemblage",
        "- dry meadow assemblage",
        "",
        "Main signal:",
        "- mixed meadow direction",
        "",
        "Candidate labels:",
        "- mixed meadow assemblage",
        "- dry meadow assemblage",
        "- transition edge assemblage",
        "",
        "What not to overclaim:",
        "- precise habitat naming"
      ),
      collapse = "\n"
    ),
    selection_json = jsonlite::toJSON(
      list(
        schema_version = "0.1.0",
        cluster_id = "c_0",
        status = "labeled",
        canonical_label = "placeholder_label",
        display_label = "placeholder label",
        label_summary = "Dry-run placeholder selection output.",
        abstain_reason = NULL
      ),
      auto_unbox = TRUE,
      null = "null",
      pretty = TRUE
    )
  )
}

.cluster_label_apply_selection_to_output <- function(output, selection_output) {
  if (!is.list(output) || !is.list(selection_output)) {
    return(output)
  }

  output$status <- selection_output$status %||% output$status
  output$canonical_label <- selection_output$canonical_label %||% NULL
  output$display_label <- selection_output$display_label %||% NULL
  output$abstain_reason <- selection_output$abstain_reason %||% NULL
  output
}

.build_cluster_label_selection_prompt <- function(
    evidence,
    selection_variant,
    draft_analysis_text,
    label_mode,
    dynamic_candidates,
    temperature,
    top_p,
    seed,
    num_predict,
    prompt_budget_chars
) {
  .build_cluster_label_prompt(
    evidence = evidence,
    variant = selection_variant,
    schema_path = .cluster_label_selection_schema_path(),
    temperature = temperature,
    top_p = top_p,
    seed = seed,
    num_predict = num_predict,
    prompt_budget_chars = prompt_budget_chars,
    label_mode = label_mode,
    dynamic_candidates = dynamic_candidates,
    extra_template_values = list(
      "{{DRAFT_ANALYSIS_TEXT}}" = draft_analysis_text
    )
  )
}

.build_cluster_label_explanation_prompt <- function(
    evidence,
    explanation_variant,
    draft_analysis_text,
    selection_output,
    schema_path,
    temperature,
    top_p,
    seed,
    num_predict,
    prompt_budget_chars
) {
  selection_json <- jsonlite::toJSON(
    selection_output,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )

  .build_cluster_label_prompt(
    evidence = evidence,
    variant = explanation_variant,
    schema_path = schema_path,
    temperature = temperature,
    top_p = top_p,
    seed = seed,
    num_predict = num_predict,
    prompt_budget_chars = prompt_budget_chars,
    extra_template_values = list(
      "{{DRAFT_ANALYSIS_TEXT}}" = draft_analysis_text,
      "{{LABEL_SELECTION_JSON}}" = selection_json
    )
  )
}

.label_selection_attempt_log_paths <- function(label_stage_log_paths, index, public_variant) {
  if (is.null(label_stage_log_paths$run_dir) || !nzchar(label_stage_log_paths$run_dir)) {
    return(.null_stage_log_paths())
  }

  .stage_log_paths(
    file.path(
      label_stage_log_paths$run_dir,
      paste0("attempt", index, "_", .safe_file_stub(public_variant))
    ),
    started_at = label_stage_log_paths$started_at
  )
}

.run_cluster_label_selection_ladder <- function(
    evidence,
    provider,
    model,
    variant,
    label_mode,
    draft_analysis_text,
    dynamic_candidates,
    base_url,
    temperature,
    top_p,
    seed,
    num_predict,
    prompt_budget_chars,
    keep_alive,
    ollama_options,
    endpoint,
    timeout_sec,
    max_retries,
    request_fn,
    label_stage_log_paths
) {
  cascade <- .cluster_label_selection_cascade_variants(variant)
  attempts <- list()
  failure_messages <- character(0)

  for (i in seq_along(cascade$public_variants)) {
    public_variant <- cascade$public_variants[[i]]
    selection_variant <- cascade$internal_variants[[i]]
    prompt_bundle <- .build_cluster_label_selection_prompt(
      evidence = evidence,
      selection_variant = selection_variant,
      draft_analysis_text = draft_analysis_text,
      label_mode = label_mode,
      dynamic_candidates = dynamic_candidates,
      temperature = temperature,
      top_p = top_p,
      seed = seed,
      num_predict = num_predict,
      prompt_budget_chars = prompt_budget_chars
    )
    .expect_prompt_task_type(prompt_bundle, "label_selection", selection_variant)

    stage_log_paths <- .label_selection_attempt_log_paths(
      label_stage_log_paths = label_stage_log_paths,
      index = i,
      public_variant = public_variant
    )

    attempt <- tryCatch(
      .run_structured_llm_stage(
        evidence = evidence,
        provider = provider,
        model = model,
        variant = selection_variant,
        prompt_bundle = prompt_bundle,
        keep_alive = keep_alive,
        ollama_options = ollama_options,
        endpoint = endpoint,
        timeout_sec = timeout_sec,
        max_retries = max_retries,
        request_fn = request_fn,
        log_paths = stage_log_paths,
        parse_output_fn = function(content) {
          output <- .parse_cluster_label_selection_json(
            content = content,
            required_fields = prompt_bundle$schema_required,
            cluster_id = evidence$meta$cluster_id
          )
          .assert_cluster_label_prompt_contract(
            output = output,
            prompt_bundle = prompt_bundle,
            stage_name = "label_selection"
          )
        },
        stage_name = "label_selection"
      ),
      error = function(e) e
    )

    if (inherits(attempt, "error")) {
      failure_messages <- c(
        failure_messages,
        paste0(public_variant, ": ", conditionMessage(attempt))
      )
      attempts[[length(attempts) + 1L]] <- list(
        variant = public_variant,
        selection_variant = selection_variant,
        result = "error",
        error = conditionMessage(attempt)
      )
      next
    }

    attempt$public_variant <- public_variant
    attempt$selection_variant <- selection_variant
    attempts[[length(attempts) + 1L]] <- attempt

    selection_contract <- .cluster_label_selection_output_is_valid(attempt$output)
    if (!isTRUE(selection_contract$ok)) {
      failure_messages <- c(
        failure_messages,
        paste0(public_variant, ": ", selection_contract$message)
      )
      next
    }

    if (identical(attempt$output$status, "abstain")) {
      failure_messages <- c(
        failure_messages,
        paste0(public_variant, ": abstained")
      )
      next
    }

    return(list(
      attempts = attempts,
      selection_output = attempt$output,
      selected_public_variant = public_variant,
      selected_selection_variant = selection_variant,
      selected_stage = attempt,
      exhausted = FALSE,
      failure_messages = failure_messages
    ))
  }

  fallback_output <- .cluster_label_selection_fallback_output(
    evidence = evidence,
    failure_messages = failure_messages
  )

  list(
    attempts = attempts,
    selection_output = fallback_output,
    selected_public_variant = "chaotic_cluster_fallback",
    selected_selection_variant = NULL,
    selected_stage = NULL,
    exhausted = TRUE,
    failure_messages = failure_messages
  )
}

.llm_label_cluster_one_step <- function(
    evidence,
    provider,
    model,
    variant,
    label_mode,
    base_url,
    schema_path,
    temperature,
    top_p,
    seed,
    num_predict,
    prompt_budget_chars,
    keep_alive,
    ollama_options,
    timeout_sec,
    max_retries,
    dry_run,
    log_dir,
    request_fn
) {
  prompt_bundle <- .build_cluster_label_prompt(
    evidence = evidence,
    variant = variant,
    schema_path = schema_path,
    temperature = temperature,
    top_p = top_p,
    seed = seed,
    num_predict = num_predict,
    prompt_budget_chars = prompt_budget_chars,
    label_mode = label_mode
  )
  .expect_prompt_task_type(prompt_bundle, "label", variant)

  request_payload <- .build_ollama_label_request(
    model = model,
    prompt_bundle = prompt_bundle,
    keep_alive = keep_alive,
    ollama_options = ollama_options
  )

  dry_run_out <- list(
    cluster_id = evidence$meta$cluster_id,
    provider = provider,
    model = model,
    variant = variant,
    workflow_steps = 1L,
    prompt = prompt_bundle,
    request = request_payload,
    schema_path = prompt_bundle$schema_path,
    workflow = NULL
  )
  class(dry_run_out) <- c("cluster_label_request", "list")

  if (isTRUE(dry_run)) {
    return(dry_run_out)
  }

  endpoint <- paste0(sub("/+$", "", base_url), "/api/chat")
  log_paths <- .init_cluster_label_logs(
    log_dir = log_dir,
    cluster_id = evidence$meta$cluster_id,
    model = model,
    variant = variant
  )

  out <- .run_structured_llm_stage(
    evidence = evidence,
    provider = provider,
    model = model,
    variant = variant,
    prompt_bundle = prompt_bundle,
    keep_alive = keep_alive,
    ollama_options = ollama_options,
    endpoint = endpoint,
    timeout_sec = timeout_sec,
    max_retries = max_retries,
    request_fn = request_fn,
    log_paths = log_paths,
    parse_output_fn = function(content) {
      output <- .parse_cluster_label_json(
        content = content,
        required_fields = prompt_bundle$schema_required,
        cluster_id = evidence$meta$cluster_id
      )
      .assert_cluster_label_prompt_contract(
        output = output,
        prompt_bundle = prompt_bundle,
        stage_name = "label"
      )
    },
    stage_name = "label"
  )
  out$workflow_steps <- 1L
  out$workflow <- NULL
  class(out) <- c("cluster_label_result", "list")
  out
}

.llm_label_cluster_two_step <- function(
    evidence,
    provider,
    model,
    variant,
    label_mode,
    base_url,
    schema_path,
    temperature,
    top_p,
    seed,
    num_predict,
    prompt_budget_chars,
    keep_alive,
    ollama_options,
    timeout_sec,
    max_retries,
    dry_run,
    log_dir,
    request_fn
) {
  gate_variant <- .default_cluster_label_gate_variant()

  gate_prompt_bundle <- .build_cluster_label_prompt(
    evidence = evidence,
    variant = gate_variant,
    schema_path = .cluster_label_gate_schema_path(),
    temperature = temperature,
    top_p = top_p,
    seed = seed,
    num_predict = num_predict,
    prompt_budget_chars = prompt_budget_chars
  )
  .expect_prompt_task_type(gate_prompt_bundle, "gate", gate_variant)

  label_prompt_bundle <- .build_cluster_label_prompt(
    evidence = evidence,
    variant = variant,
    schema_path = schema_path,
    temperature = temperature,
    top_p = top_p,
    seed = seed,
    num_predict = num_predict,
    prompt_budget_chars = prompt_budget_chars,
    label_mode = label_mode
  )
  .expect_prompt_task_type(label_prompt_bundle, "label", variant)

  gate_request_payload <- .build_ollama_label_request(
    model = model,
    prompt_bundle = gate_prompt_bundle,
    keep_alive = keep_alive,
    ollama_options = ollama_options
  )
  label_request_payload <- .build_ollama_label_request(
    model = model,
    prompt_bundle = label_prompt_bundle,
    keep_alive = keep_alive,
    ollama_options = ollama_options
  )

  dry_run_out <- list(
    cluster_id = evidence$meta$cluster_id,
    provider = provider,
    model = model,
    variant = variant,
    workflow_steps = 2L,
    prompt = gate_prompt_bundle,
    request = gate_request_payload,
    schema_path = label_prompt_bundle$schema_path,
    workflow = list(
      gate_variant = gate_variant,
      gate = list(
        variant = gate_variant,
        prompt = gate_prompt_bundle,
        request = gate_request_payload,
        schema_path = gate_prompt_bundle$schema_path
      ),
      label = list(
        variant = variant,
        prompt = label_prompt_bundle,
        request = label_request_payload,
        schema_path = label_prompt_bundle$schema_path
      )
    )
  )
  class(dry_run_out) <- c("cluster_label_request", "list")

  if (isTRUE(dry_run)) {
    return(dry_run_out)
  }

  endpoint <- paste0(sub("/+$", "", base_url), "/api/chat")
  workflow_logs <- .init_cluster_label_workflow_logs(
    log_dir = log_dir,
    cluster_id = evidence$meta$cluster_id,
    model = model,
    variant = variant,
    workflow_steps = 2L
  )

  .write_workflow_metadata(
    workflow_logs,
    list(
      started_at = workflow_logs$started_at,
      cluster_id = evidence$meta$cluster_id,
      provider = provider,
      model = model,
      variant = variant,
      workflow_steps = 2L,
      gate_variant = gate_variant,
      final_schema_path = label_prompt_bundle$schema_path,
      gate_schema_path = gate_prompt_bundle$schema_path,
      base_url = endpoint,
      status = "started",
      run_dir = workflow_logs$run_dir
    )
  )

  result <- tryCatch({
    gate_stage <- .run_structured_llm_stage(
      evidence = evidence,
      provider = provider,
      model = model,
      variant = gate_variant,
        prompt_bundle = gate_prompt_bundle,
        keep_alive = keep_alive,
        ollama_options = ollama_options,
        endpoint = endpoint,
        timeout_sec = timeout_sec,
      max_retries = max_retries,
      request_fn = request_fn,
      log_paths = workflow_logs$stages$gate,
      parse_output_fn = function(content) {
        .parse_cluster_label_gate_json(
          content = content,
          required_fields = gate_prompt_bundle$schema_required,
          cluster_id = evidence$meta$cluster_id
        )
      },
      stage_name = "gate"
    )

    label_stage <- NULL
    final_stage <- gate_stage
    final_output <- NULL

    if (identical(gate_stage$output$decision, "abstain")) {
      final_output <- .gate_decision_to_cluster_label_output(
        gate_output = gate_stage$output,
        evidence = evidence
      )
    } else {
      label_stage <- .run_structured_llm_stage(
        evidence = evidence,
        provider = provider,
        model = model,
        variant = variant,
        prompt_bundle = label_prompt_bundle,
        keep_alive = keep_alive,
        ollama_options = ollama_options,
        endpoint = endpoint,
        timeout_sec = timeout_sec,
        max_retries = max_retries,
        request_fn = request_fn,
        log_paths = workflow_logs$stages$label,
        parse_output_fn = function(content) {
          output <- .parse_cluster_label_json(
            content = content,
            required_fields = label_prompt_bundle$schema_required,
            cluster_id = evidence$meta$cluster_id
          )
          .assert_cluster_label_prompt_contract(
            output = output,
            prompt_bundle = label_prompt_bundle,
            stage_name = "label"
          )
        },
        stage_name = "label"
      )
      final_stage <- label_stage
      final_output <- label_stage$output
    }

    .write_workflow_final_output(workflow_logs, final_output)

    total_attempts <- gate_stage$attempts + (label_stage$attempts %||% 0L)
    .write_workflow_metadata(
      workflow_logs,
      list(
        started_at = workflow_logs$started_at,
        finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
        cluster_id = evidence$meta$cluster_id,
        provider = provider,
        model = model,
        variant = variant,
        workflow_steps = 2L,
        gate_variant = gate_variant,
        final_schema_path = label_prompt_bundle$schema_path,
        gate_schema_path = gate_prompt_bundle$schema_path,
        base_url = endpoint,
        status = "success",
        executed_stages = if (is.null(label_stage)) 1L else 2L,
        gate_decision = gate_stage$output$decision,
        final_output_status = final_output$status,
        attempts = total_attempts,
        run_dir = workflow_logs$run_dir
      )
    )

    out <- list(
      cluster_id = evidence$meta$cluster_id,
      provider = provider,
      model = model,
      variant = variant,
      workflow_steps = 2L,
      prompt = final_stage$prompt,
      request = final_stage$request,
      response = final_stage$response,
      output = final_output,
      attempts = total_attempts,
      schema_path = label_prompt_bundle$schema_path,
      logs = workflow_logs,
      workflow = list(
        gate_variant = gate_variant,
        gate = gate_stage,
        label = label_stage
      )
    )
    class(out) <- c("cluster_label_result", "list")
    out
  }, error = function(e) {
    if (!is.null(workflow_logs$error)) {
      .write_text_file(
        workflow_logs$error,
        paste(
          "Failed to obtain a valid multi-step cluster label result.",
          paste0("Error: ", conditionMessage(e)),
          sep = "\n"
        )
      )
    }

    .write_workflow_metadata(
      workflow_logs,
      list(
        started_at = workflow_logs$started_at,
        finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
        cluster_id = evidence$meta$cluster_id,
        provider = provider,
        model = model,
        variant = variant,
        workflow_steps = 2L,
        gate_variant = gate_variant,
        final_schema_path = label_prompt_bundle$schema_path,
        gate_schema_path = gate_prompt_bundle$schema_path,
        base_url = endpoint,
        status = "failed",
        run_dir = workflow_logs$run_dir,
        error = conditionMessage(e)
      )
    )

    stop(e)
  })

  result
}

.llm_label_cluster_three_step <- function(
    evidence,
    provider,
    model,
    variant,
    label_mode,
    base_url,
    schema_path,
    temperature,
    top_p,
    seed,
    num_predict,
    prompt_budget_chars,
    keep_alive,
    ollama_options,
    timeout_sec,
    max_retries,
    dry_run,
    log_dir,
    request_fn
) {
  draft_variant <- .default_cluster_label_draft_variant()
  explanation_variant <- .default_cluster_label_explanation_variant()
  cascade <- .cluster_label_selection_cascade_variants(variant)

  draft_prompt_bundle <- .build_cluster_label_prompt(
    evidence = evidence,
    variant = draft_variant,
    schema_path = NULL,
    temperature = temperature,
    top_p = top_p,
    seed = seed,
    num_predict = num_predict,
    prompt_budget_chars = prompt_budget_chars,
    include_schema = FALSE
  )
  .expect_prompt_task_type(draft_prompt_bundle, "draft", draft_variant)

  dry_placeholders <- .cluster_label_explanation_placeholders()
  dry_dynamic_candidates <- .extract_cluster_label_candidates_from_draft(
    dry_placeholders$draft_analysis
  )
  selection_placeholder_output <- list(
    schema_version = "0.1.0",
    cluster_id = evidence$meta$cluster_id,
    status = "labeled",
    canonical_label = "placeholder_label",
    display_label = "placeholder label",
    label_summary = "Dry-run placeholder selection output.",
    abstain_reason = NULL
  )

  label_stage_dry <- lapply(seq_along(cascade$public_variants), function(i) {
    public_variant <- cascade$public_variants[[i]]
    selection_variant <- cascade$internal_variants[[i]]
    prompt_bundle <- .build_cluster_label_selection_prompt(
      evidence = evidence,
      selection_variant = selection_variant,
      draft_analysis_text = dry_placeholders$draft_analysis,
      label_mode = label_mode,
      dynamic_candidates = dry_dynamic_candidates,
      temperature = temperature,
      top_p = top_p,
      seed = seed,
      num_predict = num_predict,
      prompt_budget_chars = prompt_budget_chars
    )
    .expect_prompt_task_type(prompt_bundle, "label_selection", selection_variant)

    list(
      variant = public_variant,
      selection_variant = selection_variant,
      prompt = prompt_bundle,
      request = .build_ollama_label_request(
        model = model,
        prompt_bundle = prompt_bundle,
        keep_alive = keep_alive,
        ollama_options = ollama_options
      ),
      schema_path = prompt_bundle$schema_path
    )
  })

  explanation_prompt_bundle <- .build_cluster_label_explanation_prompt(
    evidence = evidence,
    explanation_variant = explanation_variant,
    draft_analysis_text = dry_placeholders$draft_analysis,
    selection_output = selection_placeholder_output,
    schema_path = schema_path,
    temperature = temperature,
    top_p = top_p,
    seed = seed,
    num_predict = num_predict,
    prompt_budget_chars = prompt_budget_chars
  )
  .expect_prompt_task_type(
    explanation_prompt_bundle,
    "explanation",
    explanation_variant
  )

  draft_request_payload <- .build_ollama_label_request(
    model = model,
    prompt_bundle = draft_prompt_bundle,
    keep_alive = keep_alive,
    ollama_options = ollama_options
  )
  explanation_request_payload <- .build_ollama_label_request(
    model = model,
    prompt_bundle = explanation_prompt_bundle,
    keep_alive = keep_alive,
    ollama_options = ollama_options
  )

  dry_run_out <- list(
    cluster_id = evidence$meta$cluster_id,
    provider = provider,
    model = model,
    variant = variant,
    workflow_steps = 3L,
    prompt = explanation_prompt_bundle,
    request = explanation_request_payload,
    schema_path = explanation_prompt_bundle$schema_path,
    workflow = list(
      draft_variant = draft_variant,
      draft = list(
        variant = draft_variant,
        prompt = draft_prompt_bundle,
        request = draft_request_payload,
        schema_path = NULL
      ),
      label = list(
        variants = label_stage_dry
      ),
      explanation_variant = explanation_variant,
      explanation = list(
        variant = explanation_variant,
        prompt = explanation_prompt_bundle,
        request = explanation_request_payload,
        schema_path = explanation_prompt_bundle$schema_path
      )
    )
  )
  class(dry_run_out) <- c("cluster_label_request", "list")

  if (isTRUE(dry_run)) {
    return(dry_run_out)
  }

  endpoint <- paste0(sub("/+$", "", base_url), "/api/chat")
  workflow_logs <- .init_cluster_label_workflow_logs(
    log_dir = log_dir,
    cluster_id = evidence$meta$cluster_id,
    model = model,
    variant = variant,
    workflow_steps = 3L
  )

  .write_workflow_metadata(
    workflow_logs,
    list(
      started_at = workflow_logs$started_at,
      cluster_id = evidence$meta$cluster_id,
      provider = provider,
      model = model,
      variant = variant,
      workflow_steps = 3L,
      draft_variant = draft_variant,
      explanation_variant = explanation_variant,
      label_variants = unname(cascade$public_variants),
      final_schema_path = explanation_prompt_bundle$schema_path,
      base_url = endpoint,
      status = "started",
      run_dir = workflow_logs$run_dir
    )
  )

  result <- tryCatch({
    draft_stage <- .run_structured_llm_stage(
      evidence = evidence,
      provider = provider,
      model = model,
      variant = draft_variant,
      prompt_bundle = draft_prompt_bundle,
      keep_alive = keep_alive,
      ollama_options = ollama_options,
      endpoint = endpoint,
      timeout_sec = timeout_sec,
      max_retries = max_retries,
      request_fn = request_fn,
      log_paths = workflow_logs$stages$draft,
      parse_output_fn = function(content) {
        .parse_cluster_label_draft_text(
          content = content,
          cluster_id = evidence$meta$cluster_id
        )
      },
      stage_name = "draft_analysis",
      repair_instruction = .default_draft_stage_repair_instruction()
    )

    dynamic_candidates <- .extract_cluster_label_candidates_from_draft(
      draft_stage$output$draft_analysis
    )

    label_stage <- .run_cluster_label_selection_ladder(
      evidence = evidence,
      provider = provider,
      model = model,
      variant = variant,
      label_mode = label_mode,
      draft_analysis_text = draft_stage$output$draft_analysis,
      dynamic_candidates = dynamic_candidates,
      base_url = base_url,
      temperature = temperature,
      top_p = top_p,
      seed = seed,
      num_predict = num_predict,
      prompt_budget_chars = prompt_budget_chars,
      keep_alive = keep_alive,
      ollama_options = ollama_options,
      endpoint = endpoint,
      timeout_sec = timeout_sec,
      max_retries = max_retries,
      request_fn = request_fn,
      label_stage_log_paths = workflow_logs$stages$label
    )

    explanation_prompt_bundle <- .build_cluster_label_explanation_prompt(
      evidence = evidence,
      explanation_variant = explanation_variant,
      draft_analysis_text = draft_stage$output$draft_analysis,
      selection_output = label_stage$selection_output,
      schema_path = schema_path,
      temperature = temperature,
      top_p = top_p,
      seed = seed,
      num_predict = num_predict,
      prompt_budget_chars = prompt_budget_chars
    )
    .expect_prompt_task_type(
      explanation_prompt_bundle,
      "explanation",
      explanation_variant
    )

    if (isTRUE(label_stage$exhausted)) {
      fallback_output <- .cluster_label_selection_fallback_final_output(
        evidence = evidence,
        selection_output = label_stage$selection_output,
        failure_messages = label_stage$failure_messages
      )
      explanation_stage <- list(
        variant = explanation_variant,
        prompt = explanation_prompt_bundle,
        request = NULL,
        response = NULL,
        output = fallback_output,
        attempts = 0L,
        skipped = TRUE,
        skip_reason = "label_selection_exhausted"
      )
    } else {
      explanation_stage <- .run_structured_llm_stage(
        evidence = evidence,
        provider = provider,
        model = model,
        variant = explanation_variant,
        prompt_bundle = explanation_prompt_bundle,
        keep_alive = keep_alive,
        ollama_options = ollama_options,
        endpoint = endpoint,
        timeout_sec = timeout_sec,
        max_retries = max_retries,
        request_fn = request_fn,
        log_paths = workflow_logs$stages$explanation,
        parse_output_fn = function(content) {
          .parse_cluster_label_json(
            content = content,
            required_fields = explanation_prompt_bundle$schema_required,
            cluster_id = evidence$meta$cluster_id
          )
        },
        stage_name = "explanation"
      )

      explanation_stage$output <- .cluster_label_apply_selection_to_output(
        output = explanation_stage$output,
        selection_output = label_stage$selection_output
      )
    }
    .write_workflow_final_output(workflow_logs, explanation_stage$output)

    label_attempt_total <- sum(vapply(label_stage$attempts, function(x) {
      as.integer(x$attempts %||% 0L)
    }, integer(1)))
    total_attempts <- as.integer(
      (draft_stage$attempts %||% 0L) +
        label_attempt_total +
        (explanation_stage$attempts %||% 0L)
    )

    .write_workflow_metadata(
      workflow_logs,
      list(
        started_at = workflow_logs$started_at,
        finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
        cluster_id = evidence$meta$cluster_id,
        provider = provider,
        model = model,
        variant = variant,
        workflow_steps = 3L,
        draft_variant = draft_variant,
        explanation_variant = explanation_variant,
        label_variants = unname(cascade$public_variants),
        selected_label_variant = label_stage$selected_public_variant,
        label_stage_exhausted = isTRUE(label_stage$exhausted),
        label_stage_failure_reason = .as_scalar_character(
          label_stage$selection_output$fallback_reason %||%
            paste(label_stage$failure_messages %||% character(0), collapse = " | ")
        ),
        final_schema_path = explanation_prompt_bundle$schema_path,
        base_url = endpoint,
        status = "success",
        executed_stages = if (isTRUE(label_stage$exhausted)) 2L else 3L,
        attempts = total_attempts,
        final_output_status = explanation_stage$output$status,
        run_dir = workflow_logs$run_dir
      )
    )

    out <- list(
      cluster_id = evidence$meta$cluster_id,
      provider = provider,
      model = model,
      variant = variant,
      workflow_steps = 3L,
      prompt = explanation_stage$prompt,
      request = explanation_stage$request,
      response = explanation_stage$response,
      output = explanation_stage$output,
      attempts = total_attempts,
      schema_path = explanation_prompt_bundle$schema_path,
      logs = workflow_logs,
      workflow = list(
        draft_variant = draft_variant,
        draft = draft_stage,
        label = label_stage,
        explanation_variant = explanation_variant,
        explanation = explanation_stage
      )
    )
    class(out) <- c("cluster_label_result", "list")
    out
  }, error = function(e) {
    if (!is.null(workflow_logs$error)) {
      .write_text_file(
        workflow_logs$error,
        paste(
          "Failed to obtain a valid three-step cluster label result.",
          paste0("Error: ", conditionMessage(e)),
          sep = "\n"
        )
      )
    }

    .write_workflow_metadata(
      workflow_logs,
      list(
        started_at = workflow_logs$started_at,
        finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
        cluster_id = evidence$meta$cluster_id,
        provider = provider,
        model = model,
        variant = variant,
        workflow_steps = 3L,
        draft_variant = draft_variant,
        explanation_variant = explanation_variant,
        label_variants = unname(cascade$public_variants),
        final_schema_path = explanation_prompt_bundle$schema_path,
        base_url = endpoint,
        status = "failed",
        run_dir = workflow_logs$run_dir,
        error = conditionMessage(e)
      )
    )

    stop(e)
  })

  result
}
