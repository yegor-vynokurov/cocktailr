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
#'   \code{"abstain_first_v1"}, and
#'   \code{"strict_abstention_gate_v1"}.
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
    timeout_sec = timeout_sec,
    max_retries = max_retries,
    dry_run = dry_run,
    log_dir = log_dir,
    request_fn = request_fn
  )
}

.arg_scalar_character <- function(x, name) {
  if (missing(x) || is.null(x) || length(x) != 1L || !is.character(x) || !nzchar(x)) {
    stop("`", name, "` must be a non-empty character scalar.")
  }
  x
}

.arg_non_negative_integer <- function(x, name) {
  if (length(x) != 1L || !is.numeric(x) || !is.finite(x) || x < 0) {
    stop("`", name, "` must be a single non-negative integer.")
  }
  as.integer(x)
}

.arg_workflow_steps <- function(x, name) {
  x <- .arg_non_negative_integer(x, name)
  if (!x %in% c(1L, 2L)) {
    stop("`", name, "` must be either 1 or 2.")
  }
  x
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.package_asset_path <- function(...) {
  rel <- file.path(...)

  packaged <- system.file(..., package = "cocktailr")
  if (nzchar(packaged)) {
    return(normalizePath(packaged, winslash = "/", mustWork = TRUE))
  }

  ns_path <- tryCatch(
    getNamespaceInfo(asNamespace("cocktailr"), "path"),
    error = function(e) ""
  )

  if (is.character(ns_path) && nzchar(ns_path)) {
    candidates <- unique(c(
      file.path(ns_path, rel),
      file.path(ns_path, "inst", rel)
    ))
    existing <- candidates[file.exists(candidates)]
    if (length(existing)) {
      return(normalizePath(existing[[1L]], winslash = "/", mustWork = TRUE))
    }
  }

  ""
}

.cluster_label_prompt_catalog_path <- function() {
  path <- .package_asset_path("prompts", "cluster_labeling", "catalog.json")

  if (!nzchar(path)) {
    stop(
      "Could not locate the packaged cluster label prompt catalog. ",
      "Reinstall the package or use `pkgload::load_all()` from the package root."
    )
  }

  path
}

.cluster_label_schema_path <- function(
    schema_path = NULL,
    default_schema_name = "cluster_label_output_schema.json"
) {
  if (!is.null(schema_path)) {
    if (!file.exists(schema_path)) {
      stop("`schema_path` does not exist: ", schema_path)
    }
    return(normalizePath(schema_path, winslash = "/", mustWork = TRUE))
  }

  path <- .package_asset_path("schemas", default_schema_name)

  if (!nzchar(path)) {
    stop(
      "Could not locate the packaged schema asset '",
      default_schema_name,
      "'. ",
      "Reinstall the package or use `pkgload::load_all()` from the package root."
    )
  }

  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.cluster_label_gate_schema_path <- function(schema_path = NULL) {
  .cluster_label_schema_path(
    schema_path = schema_path,
    default_schema_name = "cluster_label_gate_schema.json"
  )
}

.read_text_file <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

.replace_fixed_scalar <- function(x, pattern, replacement) {
  gsub(pattern, replacement, x, fixed = TRUE)
}

.interpolate_prompt_template <- function(template, values) {
  out <- template
  for (nm in names(values)) {
    out <- .replace_fixed_scalar(out, nm, values[[nm]])
  }
  out
}

.read_cluster_label_schema <- function(schema_path = NULL) {
  path <- .cluster_label_schema_path(schema_path)
  text <- .read_text_file(path)
  parsed <- jsonlite::fromJSON(text, simplifyVector = FALSE)

  list(
    path = path,
    text = text,
    parsed = parsed,
    required = parsed$required %||% character(0)
  )
}

.read_cluster_label_prompt_catalog <- function() {
  path <- .cluster_label_prompt_catalog_path()
  text <- .read_text_file(path)
  parsed <- jsonlite::fromJSON(text, simplifyVector = FALSE)

  if (!is.list(parsed) || is.null(parsed$system_prompt_path) || is.null(parsed$variants)) {
    stop("The packaged cluster label prompt catalog is malformed.")
  }

  if (!length(parsed$variants) || is.null(names(parsed$variants))) {
    stop("The packaged cluster label prompt catalog must define named variants.")
  }

  list(
    path = path,
    dir = dirname(path),
    text = text,
    parsed = parsed
  )
}

.cluster_label_prompt_asset_path <- function(catalog, rel_path) {
  rel_path <- .arg_scalar_character(rel_path, "rel_path")
  path <- file.path(catalog$dir, rel_path)

  if (!file.exists(path)) {
    stop("Prompt asset referenced by the catalog does not exist: ", rel_path)
  }

  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.build_cluster_label_prompt <- function(
    evidence,
    variant,
    schema_path = NULL,
    temperature = NULL,
    top_p = NULL,
    seed = NULL,
    num_predict = NULL
) {
  catalog <- .read_cluster_label_prompt_catalog()
  catalog_def <- catalog$parsed
  variant_def <- catalog_def$variants[[variant]]

  if (is.null(variant_def)) {
    stop(
      "`variant` must be one of: ",
      paste(names(catalog_def$variants), collapse = ", "),
      "."
    )
  }

  schema <- .read_cluster_label_schema(schema_path)
  evidence_text <- .format_cluster_evidence_prompt(evidence)
  system_prompt_path <- .cluster_label_prompt_asset_path(
    catalog,
    catalog_def$system_prompt_path
  )
  user_prompt_path <- .cluster_label_prompt_asset_path(
    catalog,
    variant_def$user_prompt_path
  )

  template_values <- list(
    "{{CLUSTER_ID}}" = evidence$meta$cluster_id,
    "{{OUTPUT_SCHEMA_JSON}}" = schema$text,
    "{{CLUSTER_EVIDENCE_TEXT}}" = evidence_text
  )

  system_content <- .interpolate_prompt_template(
    .read_text_file(system_prompt_path),
    template_values
  )
  user_content <- .interpolate_prompt_template(
    .read_text_file(user_prompt_path),
    template_values
  )

  generation <- catalog_def$default_generation
  generation$temperature <- temperature %||% variant_def$temperature %||% generation$temperature
  generation$top_p <- top_p %||% variant_def$top_p %||% generation$top_p
  generation$seed <- as.integer(seed %||% generation$seed)
  generation$num_predict <- as.integer(num_predict %||% generation$num_predict)

  list(
    cluster_id = evidence$meta$cluster_id,
    variant = variant,
    task_type = variant_def$task_type %||% "label",
    catalog_path = catalog$path,
    schema_path = schema$path,
    schema_text = schema$text,
    schema_required = schema$required,
    schema_object = schema$parsed,
    evidence_text = evidence_text,
    system_path = system_prompt_path,
    user_path = user_prompt_path,
    system = system_content,
    user = user_content,
    messages = list(
      list(role = "system", content = system_content),
      list(role = "user", content = user_content)
    ),
    generation = generation
  )
}

.build_ollama_label_request <- function(model, prompt_bundle, keep_alive = NULL) {
  options <- Filter(
    Negate(is.null),
    list(
      temperature = prompt_bundle$generation$temperature,
      top_p = prompt_bundle$generation$top_p,
      seed = prompt_bundle$generation$seed,
      num_predict = prompt_bundle$generation$num_predict
    )
  )

  Filter(
    function(x) !is.null(x),
    list(
      model = model,
      messages = prompt_bundle$messages,
      stream = FALSE,
      think = FALSE,
      format = prompt_bundle$schema_object,
      options = options,
      keep_alive = keep_alive
    )
  )
}

.default_cluster_label_gate_variant <- function() {
  "gate_abstain_examples_v1"
}

.expect_prompt_task_type <- function(prompt_bundle, expected_task_type, variant) {
  actual_task_type <- prompt_bundle$task_type %||% "label"
  if (!identical(actual_task_type, expected_task_type)) {
    stop(
      "Prompt variant '", variant, "' is registered as task_type = '",
      actual_task_type, "' but this workflow requires task_type = '",
      expected_task_type, "'."
    )
  }
}

.structured_stage_status <- function(parsed_output) {
  parsed_output$status %||% parsed_output$decision %||% "unknown"
}

.run_structured_llm_stage <- function(
    evidence,
    provider,
    model,
    variant,
    prompt_bundle,
    keep_alive,
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
    keep_alive = keep_alive
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
    keep_alive = keep_alive
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
    keep_alive = keep_alive
  )
  label_request_payload <- .build_ollama_label_request(
    model = model,
    prompt_bundle = label_prompt_bundle,
    keep_alive = keep_alive
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

  if (!identical(parsed$status, "labeled") && !identical(parsed$status, "abstain")) {
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

  if (!identical(parsed$decision, "label") && !identical(parsed$decision, "abstain")) {
    stop("LLM gate output must set `decision` to either 'label' or 'abstain'.")
  }

  parsed
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

  log_dir <- .resolve_review_output_path(log_dir)
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

.init_cluster_label_workflow_logs <- function(log_dir, cluster_id, model, variant, workflow_steps) {
  if (is.null(log_dir)) {
    return(list(
      dir = NULL,
      date_dir = NULL,
      run_dir = NULL,
      started_at = NULL,
      metadata = NULL,
      output = NULL,
      error = NULL,
      stages = list(
        gate = .null_stage_log_paths(),
        label = .null_stage_log_paths()
      )
    ))
  }

  log_dir <- .resolve_review_output_path(log_dir)
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
    stages = list(
      gate = .stage_log_paths(file.path(run_dir, "stage1_gate"), started_at = started_at),
      label = .stage_log_paths(file.path(run_dir, "stage2_label"), started_at = started_at)
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
