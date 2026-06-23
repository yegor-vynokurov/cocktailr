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
#'   the packaged prompt catalog. Current label-stage values include
#'   \code{"concise_label_v1"}, \code{"conservative_interpretation_v1"},
#'   \code{"abstain_first_v1"}, \code{"strict_abstention_gate_v1"}, and the
#'   versioned speculative fallback variants
#'   \code{"speculative_fallback_v1"} to \code{"speculative_fallback_v9"}.
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
#'   Default \code{1200}.
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
#'   executed only if the gate allows labeling.
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
#'   \item \code{workflow} (present for multi-step workflows)
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
#'   variant = "strict_abstention_gate_v1",
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
    variant = "conservative_interpretation_v1",
    base_url = getOption("cocktailr.ollama_base_url", "http://localhost:11434"),
    schema_path = NULL,
    temperature = NULL,
    top_p = NULL,
    seed = NULL,
    num_predict = NULL,
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

  if (!provider %in% "ollama") {
    stop("Only provider = 'ollama' is currently supported.")
  }

  max_retries <- .arg_non_negative_integer(max_retries, "max_retries")
  workflow_steps <- .arg_workflow_steps(workflow_steps, "workflow_steps")
  ollama_options <- .arg_named_list_or_null(ollama_options, "ollama_options")
  request_fn <- request_fn %||% .ollama_chat_request

  if (identical(workflow_steps, 1L)) {
    return(.llm_label_cluster_one_step(
      evidence = evidence,
      provider = provider,
      model = model,
      variant = variant,
      base_url = base_url,
      schema_path = schema_path,
      temperature = temperature,
      top_p = top_p,
      seed = seed,
      num_predict = num_predict,
      keep_alive = keep_alive,
      ollama_options = ollama_options,
      timeout_sec = timeout_sec,
      max_retries = max_retries,
      dry_run = dry_run,
      log_dir = log_dir,
      request_fn = request_fn
    ))
  }

  .llm_label_cluster_two_step(
    evidence = evidence,
    provider = provider,
    model = model,
    variant = variant,
    base_url = base_url,
    schema_path = schema_path,
    temperature = temperature,
    top_p = top_p,
    seed = seed,
    num_predict = num_predict,
    keep_alive = keep_alive,
    ollama_options = ollama_options,
    timeout_sec = timeout_sec,
    max_retries = max_retries,
    dry_run = dry_run,
    log_dir = log_dir,
    request_fn = request_fn
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
    stage_name
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
        list(
          role = "user",
          content = paste(
            "Your previous reply was invalid JSON or did not satisfy the",
            "required top-level contract.",
            "Return one repaired JSON object only.",
            "Do not add markdown, commentary, or code fences.",
            "Use the same schema and the same evidence IDs."
          )
        )
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

.llm_label_cluster_one_step <- function(
    evidence,
    provider,
    model,
    variant,
    base_url,
    schema_path,
    temperature,
    top_p,
    seed,
    num_predict,
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
    num_predict = num_predict
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
      .parse_cluster_label_json(
        content = content,
        required_fields = prompt_bundle$schema_required,
        cluster_id = evidence$meta$cluster_id
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
    base_url,
    schema_path,
    temperature,
    top_p,
    seed,
    num_predict,
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
    num_predict = num_predict
  )
  .expect_prompt_task_type(gate_prompt_bundle, "gate", gate_variant)

  label_prompt_bundle <- .build_cluster_label_prompt(
    evidence = evidence,
    variant = variant,
    schema_path = schema_path,
    temperature = temperature,
    top_p = top_p,
    seed = seed,
    num_predict = num_predict
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
          .parse_cluster_label_json(
            content = content,
            required_fields = label_prompt_bundle$schema_required,
            cluster_id = evidence$meta$cluster_id
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
