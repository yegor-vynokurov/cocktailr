#' Label one Cocktail cluster with a local LLM via Ollama
#'
#' @description
#' Builds a prompt from a \code{\link{cluster_evidence}} object and a named
#' prompt variant from the packaged prompt catalog, then calls a local Ollama
#' model to obtain one staged cluster-labeling result.
#'
#' The function is intentionally evidence-first and code-assembled:
#' \itemize{
#'   \item the evidence object is the only data-derived input to the model
#'   \item model-facing stages return plain text only rather than JSON
#'   \item the final structured output is assembled programmatically
#'   \item malformed or incomplete stage outputs can be retried with a repair
#'     instruction
#' }
#'
#' For labeled outputs, \code{output$display_label} stores the full human label
#' while \code{output$canonical_label} stores the short/projected snake_case
#' form. Plot-facing shortening is handled later by
#' \code{\link{cluster_label_registry}} and downstream plot helpers rather than
#' by rewriting the saved \code{display_label}.
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
#' @param max_retries Integer >= 0. Number of local repair retries after a
#'   malformed or unusable plain-text stage reply. Default \code{1}.
#' @param workflow_steps Deprecated compatibility argument. The active runtime
#'   now always uses the fixed staged route
#'   \code{draft-analysis -> label-decision -> label-summary / abstain-reason}.
#' @param use_brainstorm Logical; if \code{TRUE} (default), run the draft
#'   analysis step before label decision. If \code{FALSE}, the downstream
#'   label-decision and summary / abstain-reason stages use the cluster
#'   evidence directly without a brainstorm pass.
#' @param short_label_with_llm Logical. If \code{TRUE}, the label-decision
#'   stage may use the optional internal shortening-repair prompt when a reply
#'   fails the short-label projection checks. Default \code{FALSE}; the default
#'   path avoids extra shortening-only LLM calls and still preserves the full
#'   stored label in \code{display_label}. The shortening-repair prompt is
#'   available only when the selected \code{internal_prompt_version} bundle
#'   includes that asset (for example the packaged \code{"v2"} bundle).
#' @param internal_prompt_version Character scalar naming the subdirectory
#'   under \code{inst/prompts/internal_cluster_labeling/} that contains the
#'   active internal service-prompt bundle. Default \code{"v1"}. Copy that
#'   folder to \code{"v2"}, \code{"v3"}, and so on when you want to iterate on
#'   the internal draft / decision / summary prompts without changing the
#'   currently active production set.
#' @param dry_run Logical; if \code{TRUE}, return the assembled prompt bundle
#'   and Ollama request payload without making a network request.
#' @param debug Logical. If \code{TRUE}, collect per-stage LLM debug logs
#'   (prompts, requests, responses, parsed artifacts, diagnostics). Default
#'   \code{FALSE}. When \code{FALSE}, no LLM stage-log files are written even
#'   if \code{log_dir} is supplied.
#' @param log_dir Optional directory for writing request / response artifacts.
#'   Used only when \code{debug = TRUE}. If \code{NULL}, the default debug
#'   root \code{temp/reports/cluster_label_debug} is used. When a relative path
#'   is used and a local \code{cocktailr} source checkout can be detected, it
#'   is resolved against that package root.
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
#'   \item \code{output} (including the full stored \code{display_label} and
#'     the projected \code{canonical_label})
#'   \item \code{attempts}
#'   \item \code{workflow_steps}
#'   \item \code{schema_path}
#'   \item \code{logs}
#'   \item \code{workflow} (present for multi-step workflows; for
#'     the active fixed pipeline it contains draft, label, summary,
#'     abstain-reason, and a programmatic explanation passthrough stage, with
#'     draft marked as skipped when \code{use_brainstorm = FALSE})
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
    workflow_steps = 3L,
    use_brainstorm = TRUE,
    short_label_with_llm = FALSE,
    internal_prompt_version = .default_cluster_label_internal_prompt_version(),
    dry_run = FALSE,
    debug = FALSE,
    log_dir = NULL,
    request_fn = NULL
) {
  if (!inherits(evidence, "cluster_evidence")) {
    stop("`evidence` must inherit from class `cluster_evidence`.")
  }

  provider <- .arg_scalar_character(provider, "provider")
  model <- .arg_scalar_character(model, "model")
  variant <- .arg_scalar_character(variant, "variant")
  label_mode <- .normalize_cluster_label_mode(label_mode, "label_mode")

  if (!provider %in% "ollama") {
    stop("Only provider = 'ollama' is currently supported.")
  }

  max_retries <- .arg_non_negative_integer(max_retries, "max_retries")
  workflow_steps <- .normalize_cluster_label_workflow_steps(
    workflow_steps,
    "workflow_steps"
  )
  use_brainstorm <- .arg_single_flag(use_brainstorm, "use_brainstorm")
  short_label_with_llm <- .arg_single_flag(
    short_label_with_llm,
    "short_label_with_llm"
  )
  internal_prompt_version <- .normalize_cluster_label_internal_prompt_version(
    internal_prompt_version
  )
  debug <- .arg_single_flag(debug, "debug")
  prompt_budget_chars <- .arg_nullable_positive_integer(
    prompt_budget_chars,
    "prompt_budget_chars"
  )
  ollama_options <- .arg_named_list_or_null(ollama_options, "ollama_options")
  request_fn <- request_fn %||% .ollama_chat_request
  log_dir <- .cluster_label_effective_log_dir(
    debug = debug,
    log_dir = log_dir
  )

  .llm_label_cluster_fixed_pipeline(
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
    workflow_steps = workflow_steps,
    use_brainstorm = use_brainstorm,
    short_label_with_llm = short_label_with_llm,
    internal_prompt_version = internal_prompt_version,
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

.default_selection_stage_repair_instruction <- function() {
  paste(
    "Your previous reply did not satisfy the label-selection contract.",
    "Return plain text only.",
    "Return exactly four lines in this exact order:",
    "CANONICAL_LABEL:, DISPLAY_LABEL:, LABEL_SUMMARY:, ABSTAIN_REASON:.",
    "Write each value on the same line after the colon.",
    "Do not add JSON, markdown, commentary, code fences, or a status field.",
    "If you choose a label, fill CANONICAL_LABEL, DISPLAY_LABEL, and LABEL_SUMMARY,",
    "and leave ABSTAIN_REASON empty.",
    "If you abstain, leave the first three fields empty and provide a non-empty ABSTAIN_REASON."
  )
}

.default_label_decision_stage_repair_instruction <- function() {
  paste(
    "Your previous reply did not satisfy the label-decision contract.",
    "Return plain text only.",
    "Return exactly one short answer.",
    "If you choose a label, return only the short label text.",
    "If you abstain, return only `ABSTAIN`.",
    "Do not add JSON, markdown, commentary, code fences, or multiple fields.",
    "Do not return `canonical_label`, `display_label`, `label_summary`, or `abstain_reason`."
  )
}

.cluster_label_decision_overlength_error_patterns <- function() {
  c(
    "display_label is too long for the final contract.",
    "display_label has too many words for the final contract.",
    "canonical_label is too long for the final contract."
  )
}

.is_cluster_label_decision_overlength_error <- function(message) {
  message <- .as_scalar_character(message)
  if (is.na(message) || !nzchar(message)) {
    return(FALSE)
  }

  patterns <- .cluster_label_decision_overlength_error_patterns()
  any(vapply(patterns, grepl, logical(1), x = message, fixed = TRUE))
}

.cluster_label_error_candidate_output <- function(x) {
  if (inherits(x, "error")) {
    candidate <- x$parsed_output_candidate %||%
      attr(x, "parsed_output_candidate", exact = TRUE)
    if (is.list(candidate)) {
      return(candidate)
    }
    return(NULL)
  }

  if (is.list(x)) {
    return(x)
  }

  NULL
}

.cluster_label_decision_repair_source_from_history <- function(
    repair_history,
    prompt_bundle
) {
  if (!is.list(prompt_bundle)) {
    return(NULL)
  }

  label_mode <- .as_scalar_character(prompt_bundle$label_mode_effective %||% "open")
  if (!identical(label_mode, "open")) {
    return(NULL)
  }

  repair_history <- repair_history %||% list()
  if (!length(repair_history)) {
    return(NULL)
  }

  repair_errors <- vapply(repair_history, function(entry) {
    .as_scalar_character(entry$error)
  }, character(1))
  repair_errors <- repair_errors[!is.na(repair_errors) & nzchar(repair_errors)]

  if (!length(repair_errors)) {
    return(NULL)
  }

  if (all(vapply(repair_errors, .is_cluster_label_decision_overlength_error, logical(1)))) {
    return("shortening_branch")
  }

  NULL
}

.cluster_label_decision_repair_source <- function(attempt, prompt_bundle) {
  if (!is.list(attempt) || !is.list(prompt_bundle)) {
    return(NULL)
  }

  if (!identical(.as_scalar_character(attempt$output$status), "labeled")) {
    return(NULL)
  }

  .cluster_label_decision_repair_source_from_history(
    repair_history = attempt$repair_history %||% list(),
    prompt_bundle = prompt_bundle
  )
}

.cluster_label_decision_failed_label_text <- function(
    content,
    parsed_output_candidate = NULL
) {
  candidate <- .cluster_label_error_candidate_output(parsed_output_candidate)
  candidate_label <- .as_scalar_character(
    candidate$display_label %||% candidate$label_decision_text %||% NULL
  )
  if (.is_non_empty_scalar_character(candidate_label)) {
    return(candidate_label)
  }

  content <- .as_scalar_character(content)
  if (is.na(content) || !nzchar(trimws(content))) {
    return("")
  }

  gsub("\\s+", " ", trimws(content), perl = TRUE)
}

.cluster_label_shortening_description_text <- function(draft_analysis_text) {
  draft_analysis_text <- .as_scalar_character(draft_analysis_text)

  main_signal_lines <- .cluster_label_draft_section_lines(
    draft_analysis_text,
    "main signal"
  )
  conflict_lines <- .cluster_label_draft_section_lines(
    draft_analysis_text,
    "noise or conflicts"
  )
  overclaim_lines <- .cluster_label_draft_section_lines(
    draft_analysis_text,
    "what not to overclaim"
  )

  parts <- character(0)
  if (length(main_signal_lines)) {
    parts <- c(
      parts,
      paste(
        "Main signal:",
        paste(utils::head(main_signal_lines, 2L), collapse = "; ")
      )
    )
  }
  if (length(conflict_lines)) {
    parts <- c(
      parts,
      paste(
        "Conflicts:",
        paste(utils::head(conflict_lines, 2L), collapse = "; ")
      )
    )
  }
  if (length(overclaim_lines)) {
    parts <- c(
      parts,
      paste(
        "Do not overclaim:",
        paste(utils::head(overclaim_lines, 2L), collapse = "; ")
      )
    )
  }

  description <- paste(parts[nzchar(parts)], collapse = "\n")
  if (!nzchar(description) && .is_non_empty_scalar_character(draft_analysis_text)) {
    description <- gsub("\\s+", " ", trimws(draft_analysis_text), perl = TRUE)
  }

  if (nchar(description, type = "chars") > 900L) {
    description <- paste0(substr(description, 1L, 897L), "...")
  }

  if (nzchar(description)) {
    description
  } else {
    "No extra description was available."
  }
}

.new_cluster_label_stage_failure <- function(
    message,
    prompt_bundle,
    request = NULL,
    response = NULL,
    output = NULL,
    attempts = NA_integer_,
    logs = NULL,
    repair_history = list()
) {
  structure(
    list(
      message = .as_scalar_character(message),
      call = NULL,
      prompt = prompt_bundle,
      request = request,
      response = response,
      output = output,
      attempts = as.integer(attempts),
      logs = logs,
      repair_history = repair_history
    ),
    class = c("cluster_label_stage_failure", "error", "condition")
  )
}

.default_label_summary_stage_repair_instruction <- function() {
  paste(
    "Your previous reply was empty or unusable.",
    "Return non-empty plain text summary only.",
    "Keep the already chosen label fixed.",
    "Do not return JSON or code fences."
  )
}

.default_abstain_reason_stage_repair_instruction <- function() {
  paste(
    "Your previous reply was empty or unusable.",
    "Return non-empty plain text abstain reason only.",
    "Keep the abstain decision fixed.",
    "Do not propose a new label.",
    "Do not return JSON or code fences."
  )
}

.default_explanation_stage_repair_instruction <- function() {
  paste(
    "Your previous reply was empty or unusable.",
    "Return non-empty plain text explanation only.",
    "Do not return JSON or code fences.",
    "Keep the explanation grounded in the same fixed selection result and evidence."
  )
}

.cluster_label_single_retry_budget <- function(max_retries) {
  as.integer(min(.arg_non_negative_integer(max_retries, "max_retries"), 1L))
}

.cluster_label_draft_section_lines <- function(draft_analysis_text, heading) {
  draft_analysis_text <- .as_scalar_character(draft_analysis_text)
  heading <- .as_scalar_character(heading)

  if (is.na(draft_analysis_text) || !nzchar(trimws(draft_analysis_text))) {
    return(character(0))
  }
  if (is.na(heading) || !nzchar(trimws(heading))) {
    return(character(0))
  }

  lines <- strsplit(draft_analysis_text, "\n", fixed = TRUE)[[1L]]
  lines <- trimws(lines)
  target_idx <- grep(
    paste0("^", heading, ":?$"),
    tolower(lines),
    perl = TRUE
  )
  if (!length(target_idx)) {
    return(character(0))
  }

  idx <- target_idx[[1L]]
  out <- character(0)
  if (idx < length(lines)) {
    for (line in lines[(idx + 1L):length(lines)]) {
      line_trim <- trimws(line)
      if (!nzchar(line_trim)) {
        if (length(out)) {
          break
        }
        next
      }
      lower_line <- tolower(line_trim)
      if (grepl(
        "^(possible interpretations|main signal|noise or conflicts|candidate labels|what not to overclaim)\\b",
        lower_line,
        perl = TRUE
      )) {
        break
      }
      line_trim <- gsub("^[[:space:]]*[-*]+[[:space:]]*", "", line_trim, perl = TRUE)
      if (nzchar(line_trim)) {
        out <- c(out, line_trim)
      }
    }
  }

  unique(out[nzchar(out)])
}

.cluster_label_failure_reason_text <- function(failure_messages) {
  failure_messages <- unique(.as_character_vector(failure_messages))
  if (!length(failure_messages)) {
    return(NA_character_)
  }
  paste(failure_messages, collapse = " | ")
}

.cluster_label_compact_stage_error_message <- function(message) {
  message <- .as_scalar_character(message)
  if (is.na(message) || !nzchar(message)) {
    return(NA_character_)
  }

  sub(
    "^Failed to obtain a valid(?: structured)? LLM stage result after [0-9]+ attempt\\(s\\):\\s*",
    "",
    message,
    perl = TRUE
  )
}

.cluster_label_selection_all_abstain_output <- function(
    evidence,
    abstain_reasons,
    failure_messages
) {
  abstain_reasons <- unique(.as_character_vector(abstain_reasons))
  failure_messages <- unique(.as_character_vector(failure_messages))

  abstain_reason <- if (length(abstain_reasons)) {
    paste(abstain_reasons, collapse = " | ")
  } else if (length(failure_messages)) {
    paste(
      "The selection ladder did not produce a stable short label.",
      paste(failure_messages, collapse = " | ")
    )
  } else {
    "The selection ladder did not produce a stable short label."
  }

  list(
    schema_version = "0.1.0",
    cluster_id = evidence$meta$cluster_id,
    status = "abstain",
    label_decision_text = "ABSTAIN",
    canonical_label = NULL,
    display_label = NULL,
    label_summary = NULL,
    abstain_reason = abstain_reason,
    failure_reason = .cluster_label_failure_reason_text(failure_messages)
  )
}

.cluster_label_programmatic_fallback_explanation <- function(
    selection_output,
    draft_analysis_text,
    failure_messages,
    explanation_error = NULL
) {
  selection_output <- selection_output %||% list()
  failure_messages <- unique(.as_character_vector(failure_messages))
  explanation_error <- .as_scalar_character(explanation_error)

  status <- .as_scalar_character(selection_output$status)
  label_summary <- .as_scalar_character(selection_output$label_summary)
  display_label <- .as_scalar_character(selection_output$display_label)
  abstain_reason <- .as_scalar_character(selection_output$abstain_reason)

  conflict_lines <- .cluster_label_draft_section_lines(
    draft_analysis_text,
    "noise or conflicts"
  )
  overclaim_lines <- .cluster_label_draft_section_lines(
    draft_analysis_text,
    "what not to overclaim"
  )
  main_signal_lines <- .cluster_label_draft_section_lines(
    draft_analysis_text,
    "main signal"
  )

  sentences <- character(0)
  if (identical(status, "labeled")) {
    display_label_text <- if (.is_non_empty_scalar_character(display_label)) {
      display_label
    } else {
      "the current label"
    }
    label_summary_text <- if (.is_non_empty_scalar_character(label_summary)) {
      label_summary
    } else {
      paste(
        "The workflow kept the broadest evidence-safe label rather than",
        "expanding into a heavier ecological claim."
      )
    }
    sentences <- c(
      sentences,
      paste(
        "A fixed short label was selected:",
        display_label_text,
        "."
      ),
      label_summary_text
    )
  } else {
    abstain_reason_text <- if (.is_non_empty_scalar_character(abstain_reason)) {
      abstain_reason
    } else {
      "the evidence stayed too mixed or weak for a stable short label"
    }
    sentences <- c(
      sentences,
      paste(
        "The workflow kept an abstain outcome because",
        abstain_reason_text,
        "."
      )
    )
  }

  if (length(main_signal_lines)) {
    sentences <- c(
      sentences,
      paste(
        "Main signal noted during draft analysis:",
        paste(utils::head(main_signal_lines, 2L), collapse = "; "),
        "."
      )
    )
  }

  if (length(conflict_lines)) {
    sentences <- c(
      sentences,
      paste(
        "Important conflicts remained:",
        paste(utils::head(conflict_lines, 2L), collapse = "; "),
        "."
      )
    )
  }

  if (length(overclaim_lines)) {
    sentences <- c(
      sentences,
      paste(
        "The draft also warned against overclaiming",
        paste(utils::head(overclaim_lines, 2L), collapse = "; "),
        "."
      )
    )
  }

  if (length(failure_messages)) {
    sentences <- c(
      sentences,
      paste(
        "Selection ladder notes:",
        paste(utils::head(failure_messages, 2L), collapse = " | "),
        "."
      )
    )
  }

  if (!is.na(explanation_error) && nzchar(trimws(explanation_error))) {
    sentences <- c(
      sentences,
      paste(
        "The explanation pass itself did not return usable plain text after one retry,",
        "so this fallback explanation was assembled programmatically.",
        "Technical note:",
        explanation_error,
        "."
      )
    )
  }

  paste(sentences[nzchar(sentences)], collapse = " ")
}

.cluster_label_programmatic_fallback_label_summary <- function(
    decision_output,
    draft_analysis_text,
    failure_messages,
    summary_error = NULL
) {
  decision_output <- decision_output %||% list()
  display_label <- .as_scalar_character(decision_output$display_label)
  summary_error <- .as_scalar_character(summary_error)

  main_signal_lines <- .cluster_label_draft_section_lines(
    draft_analysis_text,
    "main signal"
  )
  overclaim_lines <- .cluster_label_draft_section_lines(
    draft_analysis_text,
    "what not to overclaim"
  )

  label_text <- if (.is_non_empty_scalar_character(display_label)) {
    display_label
  } else {
    "the selected short label"
  }

  sentences <- c(
    paste(
      label_text,
      "is retained as a broad evidence-safe short label rather than a narrower habitat claim."
    )
  )

  if (length(main_signal_lines)) {
    sentences <- c(
      sentences,
      paste(
        "The brainstorm pointed mainly toward",
        paste(utils::head(main_signal_lines, 2L), collapse = "; "),
        "."
      )
    )
  }

  if (length(overclaim_lines)) {
    sentences <- c(
      sentences,
      paste(
        "The workflow also avoids overclaiming",
        paste(utils::head(overclaim_lines, 2L), collapse = "; "),
        "."
      )
    )
  }

  if (!is.na(summary_error) && nzchar(trimws(summary_error))) {
    sentences <- c(
      sentences,
      paste(
        "The label-summary step did not return usable plain text after one retry,",
        "so this fallback summary was assembled programmatically.",
        "Technical note:",
        summary_error,
        "."
      )
    )
  }

  paste(sentences[nzchar(sentences)], collapse = " ")
}

.cluster_label_programmatic_fallback_abstain_reason <- function(
    decision_output,
    draft_analysis_text,
    failure_messages,
    abstain_reason_error = NULL
) {
  decision_output <- decision_output %||% list()
  decision_text <- .as_scalar_character(decision_output$label_decision_text)
  abstain_reason_error <- .as_scalar_character(abstain_reason_error)
  failure_messages <- unique(.as_character_vector(failure_messages))

  conflict_lines <- .cluster_label_draft_section_lines(
    draft_analysis_text,
    "noise or conflicts"
  )
  overclaim_lines <- .cluster_label_draft_section_lines(
    draft_analysis_text,
    "what not to overclaim"
  )

  inline_reason <- trimws(sub(
    "^ABSTAIN\\b[[:punct:][:space:]]*",
    "",
    .null_default(decision_text, ""),
    ignore.case = TRUE,
    perl = TRUE
  ))

  sentences <- c()
  if (nzchar(inline_reason)) {
    sentences <- c(sentences, inline_reason)
  } else {
    sentences <- c(
      sentences,
      "The available signal remains too mixed or weak for a stable short label."
    )
  }

  if (length(conflict_lines)) {
    sentences <- c(
      sentences,
      paste(
        "Important unresolved conflicts include",
        paste(utils::head(conflict_lines, 2L), collapse = "; "),
        "."
      )
    )
  }

  if (length(overclaim_lines)) {
    sentences <- c(
      sentences,
      paste(
        "The workflow also avoids overclaiming",
        paste(utils::head(overclaim_lines, 2L), collapse = "; "),
        "."
      )
    )
  }

  if (length(failure_messages)) {
    sentences <- c(
      sentences,
      paste(
        "Label-decision ladder notes:",
        paste(utils::head(failure_messages, 2L), collapse = " | "),
        "."
      )
    )
  }

  if (!is.na(abstain_reason_error) && nzchar(trimws(abstain_reason_error))) {
    sentences <- c(
      sentences,
      paste(
        "The abstain-reason step did not return usable plain text after one retry,",
        "so this fallback reason was assembled programmatically.",
        "Technical note:",
        abstain_reason_error,
        "."
      )
    )
  }

  paste(sentences[nzchar(sentences)], collapse = " ")
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
  last_payload_attempt <- NULL
  last_parsed_outer <- NULL
  last_content <- NULL
  last_candidate_output <- NULL
  repair_history <- list()

  # Keep stage-level retries local to one prompt bundle. Structural repair
  # happens by appending the invalid reply and a constrained correction
  # instruction to the existing message stack.
  for (attempt in seq_len(max_retries + 1L)) {
    payload_attempt <- request_payload
    payload_attempt$messages <- messages_current
    last_payload_attempt <- payload_attempt

    if (!is.null(log_paths$request_prefix)) {
      .write_text_file(
        paste0(log_paths$request_prefix, "_attempt", attempt, ".json"),
        jsonlite::toJSON(
          payload_attempt,
          auto_unbox = TRUE,
          null = "null",
          pretty = TRUE
        )
      )
    }

    resp <- request_fn(endpoint, payload_attempt, timeout_sec)
    parsed_outer <- .ensure_ollama_envelope(resp)
    last_parsed_outer <- parsed_outer

    if (!is.null(log_paths$response_prefix)) {
      .write_text_file(
        paste0(log_paths$response_prefix, "_attempt", attempt, "_envelope.json"),
        parsed_outer$body_text
      )
    }

    content <- .extract_ollama_message_content(parsed_outer$parsed)
    last_content <- content
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
    candidate_output <- .cluster_label_error_candidate_output(parsed_output)
    if (is.list(candidate_output)) {
      last_candidate_output <- candidate_output
    }

    .write_stage_attempt_diagnostics(
      log_paths = log_paths,
      attempt = attempt,
      evidence = evidence,
      provider = provider,
      model = model,
      variant = variant,
      stage_name = stage_name,
      prompt_bundle = prompt_bundle,
      endpoint = endpoint,
      payload_attempt = payload_attempt,
      content = content,
      parsed_output = parsed_output
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

      if (!is.null(log_paths$parsed_text_fields)) {
        .write_text_file(
          log_paths$parsed_text_fields,
          jsonlite::prettify(
            jsonlite::toJSON(
              .cluster_label_stage_text_artifact(parsed_output, stage_name),
              auto_unbox = TRUE,
              null = "null"
            )
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
        logs = log_paths,
        repair_history = repair_history
      ))
    }

    last_error <- parsed_output
    repair_message <- conditionMessage(parsed_output)
    repair_instruction_text <- if (attempt <= max_retries) {
      if (is.function(repair_instruction)) {
        repair_instruction(
          error_message = repair_message,
          attempt = attempt,
          prompt_bundle = prompt_bundle,
          variant = variant,
          stage_name = stage_name,
          content = content,
          parsed_output = parsed_output
        )
      } else {
        repair_instruction
      }
    } else {
      NULL
    }
    repair_instruction_text <- .as_scalar_character(repair_instruction_text)
    repair_history[[length(repair_history) + 1L]] <- list(
      attempt = attempt,
      error = repair_message,
      repair_instruction = repair_instruction_text,
      request = payload_attempt,
      response_raw = parsed_outer$body_text,
      response_content = content,
      parsed_output_candidate = candidate_output
    )

    if (attempt > max_retries) {
      break
    }

    messages_current <- c(
      messages_current,
      list(
        list(role = "assistant", content = content),
        list(role = "user", content = repair_instruction_text)
      )
    )
  }

  if (!is.null(log_paths$error)) {
    .write_text_file(
      log_paths$error,
      paste(
        "Failed to obtain a valid LLM stage result.",
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
    .new_cluster_label_stage_failure(
      message = paste0(
        "Failed to obtain a valid LLM stage result after ",
        max_retries + 1L,
        " attempt(s): ",
        conditionMessage(last_error)
      ),
      prompt_bundle = prompt_bundle,
      request = last_payload_attempt,
      response = if (!is.null(last_parsed_outer)) {
        list(
          status_code = last_parsed_outer$status_code,
          envelope = last_parsed_outer$parsed,
          raw = last_parsed_outer$body_text,
          content = last_content
        )
      } else {
        NULL
      },
      output = last_candidate_output,
      attempts = max_retries + 1L,
      logs = log_paths,
      repair_history = repair_history
    )
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
  decision_summary <- gate_output$decision_summary %||%
    gate_output$abstain_reason %||%
    "The workflow abstained before final labeling."
  list(
    schema_version = "0.1.0",
    cluster_id = evidence$meta$cluster_id,
    status = "abstain",
    canonical_label = NULL,
    display_label = NULL,
    interpretation_summary = decision_summary,
    basis_in_data = list(),
    key_species = gate_output$key_species %||% list(),
    external_knowledge = list(),
    not_confirmed_by_data = gate_output$not_confirmed_by_data %||% list(),
    confidence = gate_output$confidence,
    checks_to_run = gate_output$checks_to_run %||% list(),
    abstain_reason = gate_output$abstain_reason %||% decision_summary,
    ontology_slots = gate_output$ontology_slots %||% NULL,
    contrastive_notes = NULL,
    label_summary = NULL,
    explanation = decision_summary,
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

  if (identical(status, "abstain")) {
    if (!is.null(output$canonical_label) || !is.null(output$display_label)) {
      return(list(
        ok = FALSE,
        message = "abstaining selection outputs must set canonical_label and display_label to null."
      ))
    }
    if (!is.null(output$label_summary)) {
      return(list(
        ok = FALSE,
        message = "abstaining selection outputs must set label_summary to null."
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

  if (!.is_non_empty_scalar_character(label_summary)) {
    return(list(ok = FALSE, message = "labeled selection outputs must provide non-empty label_summary."))
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

.cluster_label_label_decision_output_is_valid <- function(output) {
  status <- .as_scalar_character(output$status)
  canonical_label <- .as_scalar_character(output$canonical_label)
  display_label <- .as_scalar_character(output$display_label)
  label_decision_text <- .as_scalar_character(output$label_decision_text)

  if (identical(status, "abstain")) {
    if (!is.null(output$canonical_label) || !is.null(output$display_label)) {
      return(list(
        ok = FALSE,
        message = "abstaining label-decision outputs must set canonical_label and display_label to null."
      ))
    }
    if (!.is_non_empty_scalar_character(label_decision_text)) {
      return(list(
        ok = FALSE,
        message = "abstaining label-decision outputs must provide a non-empty short abstain answer."
      ))
    }
    return(list(ok = TRUE, message = NULL))
  }

  if (!identical(status, "labeled")) {
    return(list(
      ok = FALSE,
      message = "label-decision status must be either labeled or abstain."
    ))
  }

  if (!.is_non_empty_scalar_character(label_decision_text)) {
    return(list(
      ok = FALSE,
      message = "labeled label-decision outputs must provide label_decision_text."
    ))
  }
  if (!.is_non_empty_scalar_character(canonical_label)) {
    return(list(
      ok = FALSE,
      message = "labeled label-decision outputs must provide canonical_label."
    ))
  }
  if (!grepl("^[a-z0-9_]+$", canonical_label)) {
    return(list(
      ok = FALSE,
      message = "canonical_label must use lowercase snake_case."
    ))
  }
  if (nchar(canonical_label, type = "chars") > .cluster_label_max_canonical_length()) {
    return(list(
      ok = FALSE,
      message = "canonical_label is too long for the final contract."
    ))
  }
  if (!.is_non_empty_scalar_character(display_label)) {
    return(list(
      ok = FALSE,
      message = "labeled label-decision outputs must provide display_label."
    ))
  }

  display_label_trimmed <- trimws(display_label)
  if (grepl(
    .cluster_label_forbidden_display_punctuation_pattern(),
    display_label_trimmed,
    perl = TRUE
  )) {
    return(list(
      ok = FALSE,
      message = "display_label contains forbidden punctuation."
    ))
  }
  if (grepl("\\.$", display_label_trimmed, perl = TRUE)) {
    return(list(
      ok = FALSE,
      message = "display_label must not end with a period."
    ))
  }

  list(ok = TRUE, message = NULL)
}

.cluster_label_prepare_display_label_for_contract <- function(display_label) {
  display_label <- .as_scalar_character(display_label)

  if (is.na(display_label) || !nzchar(trimws(display_label))) {
    return(NA_character_)
  }

  text <- trimws(display_label)

  # Remove wrappers and sentence-like residue.
  text <- gsub("`", "", text, fixed = TRUE)
  text <- gsub("^[\"']+|[\"']+$", "", text, perl = TRUE)
  text <- gsub("\\s+", " ", text, perl = TRUE)
  text <- trimws(text)

  # Common LLM pattern:
  # "Base-rich dry grassland forbs with Salvia nemorosa and Falcaria vulgaris core"
  # -> "Base-rich dry grassland forbs"
  text <- sub(
    "\\s+with\\s+[A-Z][a-zA-Z-]+\\s+[a-z][a-zA-Z-]+.*$",
    "",
    text,
    perl = TRUE
  )

  text <- sub(
    "\\s+including\\s+[A-Z][a-zA-Z-]+\\s+[a-z][a-zA-Z-]+.*$",
    "",
    text,
    perl = TRUE
  )

  text <- sub(
    "\\s+dominated\\s+by\\s+[A-Z][a-zA-Z-]+\\s+[a-z][a-zA-Z-]+.*$",
    "",
    text,
    perl = TRUE
  )

  # Remove trailing "core" after model-generated species-core phrases.
  text <- sub("\\s+core\\s*$", "", text, perl = TRUE)

  # Remove trailing punctuation.
  text <- gsub("[.]+$", "", text, perl = TRUE)

  text <- gsub("\\s+", " ", text, perl = TRUE)
  text <- trimws(text)

  if (!nzchar(text)) {
    return(NA_character_)
  }

  text
}

.cluster_label_shorten_display_label_for_contract <- function(display_label) {
  text <- .cluster_label_prepare_display_label_for_contract(display_label)

  if (is.na(text) || !nzchar(text)) {
    return(NA_character_)
  }

  max_words <- getOption(
    "cocktailr.label_final_target_max_words",
    .cluster_label_max_display_words()
  )

  max_chars <- getOption(
    "cocktailr.label_final_target_max_chars",
    .cluster_label_max_display_length()
  )

  words <- strsplit(text, "\\s+", perl = TRUE)[[1L]]

  if (length(words) > max_words) {
    text <- paste(words[seq_len(max_words)], collapse = " ")
  }

  if (nchar(text, type = "chars") > max_chars) {
    text <- substr(text, 1L, max_chars)
    text <- sub("\\s+\\S*$", "", text, perl = TRUE)
    text <- trimws(text)
  }

  if (nzchar(text)) text else NA_character_
}

.cluster_label_project_canonical_label_for_contract <- function(display_label) {
  text <- .cluster_label_prepare_display_label_for_contract(display_label)

  if (is.na(text) || !nzchar(text)) {
    return(NA_character_)
  }

  max_words <- getOption(
    "cocktailr.label_final_target_canonical_max_words",
    .cluster_label_max_canonical_words()
  )

  words <- strsplit(text, "\\s+", perl = TRUE)[[1L]]
  truncated <- length(words) > max_words

  if (truncated) {
    text <- paste(words[seq_len(max_words)], collapse = " ")
  }

  canonical <- .cluster_label_canonical_for_contract(text)
  if (is.na(canonical) || !nzchar(canonical)) {
    return(NA_character_)
  }

  if (truncated) {
    max_chars <- getOption(
      "cocktailr.label_final_target_canonical_max_chars",
      .cluster_label_max_canonical_length()
    )
    canonical <- substr(canonical, 1L, max(1L, max_chars - 1L))
    canonical <- gsub("_+$", "", canonical, perl = TRUE)
    canonical <- trimws(canonical)
    if (!nzchar(canonical)) {
      return(NA_character_)
    }
    canonical <- paste0(canonical, "_")
  }

  canonical
}

.cluster_label_canonical_for_contract <- function(display_label) {
  display_label <- .as_scalar_character(display_label)

  if (is.na(display_label) || !nzchar(trimws(display_label))) {
    return(NA_character_)
  }

  canonical <- iconv(display_label, from = "", to = "ASCII//TRANSLIT")
  canonical <- tolower(canonical)
  canonical <- gsub("[^a-z0-9]+", "_", canonical, perl = TRUE)
  canonical <- gsub("_+", "_", canonical, perl = TRUE)
  canonical <- gsub("^_+|_+$", "", canonical, perl = TRUE)

  max_chars <- getOption(
    "cocktailr.label_final_target_canonical_max_chars",
    .cluster_label_max_canonical_length()
  )

  if (nchar(canonical, type = "chars") > max_chars) {
    canonical <- substr(canonical, 1L, max_chars)
    canonical <- sub("_[^_]*$", "", canonical, perl = TRUE)
    canonical <- gsub("_+$", "", canonical, perl = TRUE)
  }

  if (nzchar(canonical)) canonical else NA_character_
}

.cluster_label_coerce_output_to_final_contract <- function(
    output,
    prompt_bundle = NULL
) {
  if (!is.list(output)) {
    return(output)
  }

  status <- .as_scalar_character(output$status)

  if (!identical(status, "labeled")) {
    return(output)
  }

  label_mode <- if (is.list(prompt_bundle)) {
    .as_scalar_character(prompt_bundle$label_mode_effective %||% "open")
  } else {
    "open"
  }

  display_label <- .as_scalar_character(output$display_label)

  if (!.is_non_empty_scalar_character(display_label)) {
    display_label <- .as_scalar_character(output$label_decision_text)
  }

  if (!.is_non_empty_scalar_character(display_label)) {
    return(output)
  }

  display_label <- trimws(display_label)
  output$display_label <- display_label

  projected_label <- .cluster_label_shorten_display_label_for_contract(
    display_label
  )

  if (identical(label_mode, "open")) {
    output$canonical_label <- .cluster_label_project_canonical_label_for_contract(
      display_label
    )
  }

  if (!is.null(output$label_decision_text)) {
    output$label_decision_text <- display_label
  }

  output
}

.cluster_label_selection_validation_error_message <- function(message) {
  paste0("Selection output failed text-field validation: ", message)
}

.assert_cluster_label_selection_stage_output <- function(
    output,
    prompt_bundle,
    stage_name = "label_selection"
) {
  output <- .harmonize_cluster_label_prompt_contract_output(
    output = output,
    prompt_bundle = prompt_bundle
  )

  output <- .cluster_label_coerce_output_to_final_contract(
    output = output,
    prompt_bundle = prompt_bundle
  )

  selection_contract <- .cluster_label_selection_output_is_valid(output)
  if (!isTRUE(selection_contract$ok)) {
    stop(.cluster_label_selection_validation_error_message(selection_contract$message))
  }

  .assert_cluster_label_prompt_contract(
    output = output,
    prompt_bundle = prompt_bundle,
    stage_name = stage_name
  )
}

.assert_cluster_label_label_decision_stage_output <- function(
    output,
    prompt_bundle,
    stage_name = "label_decision"
) {
  output <- .harmonize_cluster_label_prompt_contract_output(
    output = output,
    prompt_bundle = prompt_bundle
  )

  output <- .cluster_label_coerce_output_to_final_contract(
    output = output,
    prompt_bundle = prompt_bundle
  )

  decision_contract <- .cluster_label_label_decision_output_is_valid(output)
  if (!isTRUE(decision_contract$ok)) {
    stop(
      "Label-decision output failed text validation: ",
      decision_contract$message
    )
  }

  .assert_cluster_label_prompt_contract(
    output = output,
    prompt_bundle = prompt_bundle,
    stage_name = stage_name
  )
}

.harmonize_cluster_label_prompt_contract_output <- function(output, prompt_bundle) {
  if (!is.list(output) || !is.list(prompt_bundle)) {
    return(output)
  }

  status <- .as_scalar_character(output$status)
  if (!identical(status, "labeled")) {
    return(output)
  }

  label_mode <- .as_scalar_character(prompt_bundle$label_mode_effective %||% "open")
  canonical_label <- .as_scalar_character(output$canonical_label)
  if (!.is_non_empty_scalar_character(canonical_label)) {
    return(output)
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
    if (!is.na(match_idx) &&
        .is_non_empty_scalar_character(allowed_display[[match_idx]])) {
      output$display_label <- allowed_display[[match_idx]]
      output$label_decision_text <- allowed_display[[match_idx]]
    }
  }

  output
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

  if (identical(label_mode, "open")) {
    return(list(ok = TRUE, message = NULL))
  }

  if (identical(label_mode, "constrained")) {
    labels <- prompt_bundle$vocabulary_object$labels %||% list()
    allowed_canonical <- vapply(labels, function(x) {
      .as_scalar_character(x$canonical_label)
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
    label_summary = paste(
      "The public label-selection cascade exhausted all rungs, so the workflow",
      "fell back to a broad manual-review placeholder."
    ),
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
    explanation = paste(
      "The workflow used a broad chaotic-cluster placeholder because",
      fallback_reason
    ),
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
  selection_output <- list(
    schema_version = "0.1.0",
    cluster_id = "c_0",
    status = "labeled",
    canonical_label = "placeholder_label",
    display_label = "placeholder label",
    label_summary = "Dry-run placeholder selection output.",
    abstain_reason = NULL
  )

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
    selection_text = .render_cluster_label_selection_text_block(selection_output)
  )
}

.render_cluster_label_selection_text_block <- function(selection_output) {
  selection_output <- selection_output %||% list()
  status <- .as_scalar_character(selection_output$status)
  if (!status %in% c("labeled", "abstain")) {
    inferred <- .infer_cluster_label_selection_status(selection_output)
    status <- if (isTRUE(inferred$ok)) inferred$status else ""
  }

  canonical_label <- .as_scalar_character(selection_output$canonical_label)
  display_label <- .as_scalar_character(selection_output$display_label)
  category_label <- .as_scalar_character(selection_output$category_label)
  subcategory_labels <- .cluster_label_subcategory_labels_text(
    selection_output$subcategory_labels
  )
  label_summary <- .as_scalar_character(selection_output$label_summary)
  abstain_reason <- .as_scalar_character(selection_output$abstain_reason)

  fields <- c(
    SELECTION_STATUS = if (status %in% c("labeled", "abstain")) status else "",
    CANONICAL_LABEL = if (.is_non_empty_scalar_character(canonical_label)) canonical_label else "",
    DISPLAY_LABEL = if (.is_non_empty_scalar_character(display_label)) display_label else "",
    CATEGORY_LABEL = if (.is_non_empty_scalar_character(category_label)) category_label else "",
    SUBCATEGORY_LABELS = if (.is_non_empty_scalar_character(subcategory_labels)) subcategory_labels else "",
    LABEL_SUMMARY = if (.is_non_empty_scalar_character(label_summary)) label_summary else "",
    ABSTAIN_REASON = if (.is_non_empty_scalar_character(abstain_reason)) abstain_reason else ""
  )

  paste(sprintf("%s: %s", names(fields), unname(fields)), collapse = "\n")
}

.cluster_label_v2_placeholders <- function(cluster_id = "c_0") {
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
    labeled_decision = list(
      schema_version = "0.1.0",
      cluster_id = cluster_id,
      status = "labeled",
      label_decision_text = "placeholder label",
      canonical_label = "placeholder_label",
      display_label = "placeholder label",
      category_label = "mixed meadow",
      subcategory_labels = c("dry", "transition")
    ),
    abstain_decision = list(
      schema_version = "0.1.0",
      cluster_id = cluster_id,
      status = "abstain",
      label_decision_text = "ABSTAIN",
      canonical_label = NULL,
      display_label = NULL,
      category_label = NULL,
      subcategory_labels = character(0)
    ),
    label_summary = "Placeholder short label summary for dry-run assembly.",
    abstain_reason = "Placeholder abstain reason for dry-run assembly."
  )
}

.render_cluster_label_decision_text <- function(decision_output) {
  decision_output <- decision_output %||% list()
  label_decision_text <- .as_scalar_character(decision_output$label_decision_text)
  if (.is_non_empty_scalar_character(label_decision_text)) {
    return(trimws(label_decision_text))
  }

  status <- .as_scalar_character(decision_output$status)
  if (identical(status, "abstain")) {
    return("ABSTAIN")
  }

  display_label <- .as_scalar_character(decision_output$display_label)
  if (.is_non_empty_scalar_character(display_label)) {
    return(trimws(display_label))
  }

  ""
}

.assemble_cluster_label_final_output <- function(
    evidence,
    selection_output,
    label_summary_text = NULL,
    abstain_reason_text = NULL,
    explanation_text = NULL
) {
  selection_output <- selection_output %||% list()
  status <- .as_scalar_character(selection_output$status)
  if (!status %in% c("labeled", "abstain")) {
    stop("Selection output must carry a programmatic status before final assembly.")
  }

  canonical_label <- .as_scalar_character(selection_output$canonical_label)
  display_label <- .as_scalar_character(selection_output$display_label)
  category_label <- .as_scalar_character(selection_output$category_label)
  subcategory_labels <- .cluster_label_subcategory_labels(
    selection_output$subcategory_labels
  )
  label_summary <- .as_scalar_character(selection_output$label_summary)
  abstain_reason <- .as_scalar_character(selection_output$abstain_reason)

  if (!.is_non_empty_scalar_character(canonical_label)) {
    canonical_label <- NULL
  }
  if (!.is_non_empty_scalar_character(display_label)) {
    display_label <- NULL
  }
  if (!.is_non_empty_scalar_character(category_label)) {
    category_label <- NULL
  }
  if (!.is_non_empty_scalar_character(label_summary)) {
    label_summary <- NULL
  }
  if (!.is_non_empty_scalar_character(abstain_reason)) {
    abstain_reason <- NULL
  }

  label_summary_text <- .as_scalar_character(label_summary_text)
  if (is.na(label_summary_text) || !nzchar(trimws(label_summary_text))) {
    label_summary_text <- NULL
  }
  abstain_reason_text <- .as_scalar_character(abstain_reason_text)
  if (is.na(abstain_reason_text) || !nzchar(trimws(abstain_reason_text))) {
    abstain_reason_text <- NULL
  }
  explanation_text <- .as_scalar_character(explanation_text)
  if (is.na(explanation_text) || !nzchar(trimws(explanation_text))) {
    explanation_text <- NULL
  }

  evidence_ids <- utils::head(
    .cluster_label_selection_fallback_evidence_ids(evidence),
    3L
  )
  key_species <- .cluster_label_selection_fallback_key_species(evidence)

  if (identical(status, "labeled")) {
    label_summary <- label_summary_text %||% label_summary
    if (!.is_non_empty_scalar_character(label_summary)) {
      stop("Labeled final output assembly requires non-empty label_summary_text.")
    }
    explanation_text <- explanation_text %||% label_summary
    interpretation_summary <- label_summary
    basis_statement <- if (.is_non_empty_scalar_character(label_summary)) {
      paste(
        "The selected short label is supported by the recurring compositional",
        "signal summarized in the evidence bundle."
      )
    } else {
      "The selected short label is supported by a recurring compositional signal in the evidence bundle."
    }
    not_confirmed_by_data <- list(
      list(
        statement = paste(
          "A more specific",
          display_label %||% "habitat-level",
          "interpretation is not directly confirmed by the evidence bundle."
        ),
        reason = "The staged workflow now optimizes for safe label selection plus compact explanation rather than a large structured ecological expansion."
      )
    )
    confidence <- list(
      score = 0.55,
      rationale = "This concise label passed the staged decision ladder; the explanation field carries the human-readable justification."
    )
    checks_to_run <- list(
      list(
        check = "Compare sibling clusters if finer ecological interpretation is required.",
        priority = "medium",
        reason = "The active workflow intentionally prioritizes a safe short label over a heavier downstream ecological expansion."
      )
    )
    abstain_reason <- NULL
  } else {
    canonical_label <- NULL
    display_label <- NULL
    label_summary <- NULL
    abstain_reason <- abstain_reason_text %||% abstain_reason
    explanation_text <- explanation_text %||% abstain_reason
    if (!.is_non_empty_scalar_character(abstain_reason)) {
      abstain_reason <- "The staged decision ladder abstained."
    }
    if (!.is_non_empty_scalar_character(explanation_text)) {
      explanation_text <- abstain_reason
    }
    interpretation_summary <- paste(
      "No stable short label was selected.",
      abstain_reason
    )
    basis_statement <- "The cluster shows some recurring compositional signal, but the available evidence is still too mixed or weak for a stable short label."
    not_confirmed_by_data <- list(
      list(
        statement = "A stable short ecological label is not confirmed.",
        reason = abstain_reason
      )
    )
    confidence <- list(
      score = 0,
      rationale = "The staged decision ladder abstained, so the workflow records no stable short label."
    )
    checks_to_run <- list(
      list(
        check = "Review the cluster manually or compare it with neighboring clusters.",
        priority = "high",
        reason = abstain_reason
      )
    )
  }

  basis_in_data <- if (length(evidence_ids)) {
    list(
      list(
        claim_id = "C1",
        statement = basis_statement,
        evidence_ids = evidence_ids,
        support_strength = "weak"
      )
    )
  } else {
    list()
  }

  list(
    schema_version = "0.1.0",
    cluster_id = evidence$meta$cluster_id,
    status = status,
    canonical_label = canonical_label,
    display_label = display_label,
    category_label = category_label,
    subcategory_labels = subcategory_labels,
    interpretation_summary = interpretation_summary,
    basis_in_data = basis_in_data,
    key_species = key_species,
    external_knowledge = list(),
    not_confirmed_by_data = not_confirmed_by_data,
    confidence = confidence,
    checks_to_run = checks_to_run,
    abstain_reason = abstain_reason,
    label_summary = label_summary,
    explanation = trimws(explanation_text)
  )
}

.cluster_label_subcategory_labels <- function(x) {
  if (is.null(x)) {
    return(character(0))
  }

  x <- .as_character_vector(x)
  x <- trimws(x)
  unique(x[nzchar(x)])
}

.cluster_label_subcategory_labels_text <- function(x) {
  x <- .cluster_label_subcategory_labels(x)
  if (!length(x)) {
    return(NA_character_)
  }
  paste(x, collapse = "; ")
}

.build_cluster_label_decision_prompt <- function(
    evidence,
    decision_variant,
    draft_analysis_text,
    label_mode,
    dynamic_candidates,
    temperature,
    top_p,
    seed,
    num_predict,
    prompt_budget_chars,
    internal_prompt_version
) {
  .build_cluster_label_prompt(
    evidence = evidence,
    variant = decision_variant,
    schema_path = NULL,
    temperature = temperature,
    top_p = top_p,
    seed = seed,
    num_predict = num_predict,
    prompt_budget_chars = prompt_budget_chars,
    include_schema = FALSE,
    label_mode = label_mode,
    dynamic_candidates = dynamic_candidates,
    internal_prompt_version = internal_prompt_version,
    extra_template_values = list(
      "{{DRAFT_ANALYSIS_TEXT}}" = draft_analysis_text
    )
  )
}

.default_cluster_label_category_variants <- function() {
  c(
    "category_decision_primary_v1",
    "category_decision_soft_v1",
    "category_decision_broad_v1"
  )
}

.default_cluster_label_subcategory_variants <- function() {
  c(
    "subcategory_decision_primary_v1",
    "subcategory_decision_soft_v1",
    "subcategory_decision_broad_v1"
  )
}

.is_cluster_label_decomposed_internal_prompt_version <- function(internal_prompt_version) {
  identical(
    .normalize_cluster_label_internal_prompt_version(internal_prompt_version),
    "v4"
  )
}

.build_cluster_label_category_prompt <- function(
    evidence,
    category_variant,
    draft_analysis_text,
    label_mode,
    dynamic_candidates,
    temperature,
    top_p,
    seed,
    num_predict,
    prompt_budget_chars,
    internal_prompt_version
) {
  .build_cluster_label_prompt(
    evidence = evidence,
    variant = category_variant,
    schema_path = NULL,
    temperature = temperature,
    top_p = top_p,
    seed = seed,
    num_predict = num_predict,
    prompt_budget_chars = prompt_budget_chars,
    include_schema = FALSE,
    label_mode = label_mode,
    dynamic_candidates = dynamic_candidates,
    internal_prompt_version = internal_prompt_version,
    extra_template_values = list(
      "{{DRAFT_ANALYSIS_TEXT}}" = draft_analysis_text
    )
  )
}

.build_cluster_label_subcategory_prompt <- function(
    evidence,
    subcategory_variant,
    draft_analysis_text,
    category_label_text,
    label_mode,
    dynamic_candidates,
    temperature,
    top_p,
    seed,
    num_predict,
    prompt_budget_chars,
    internal_prompt_version
) {
  .build_cluster_label_prompt(
    evidence = evidence,
    variant = subcategory_variant,
    schema_path = NULL,
    temperature = temperature,
    top_p = top_p,
    seed = seed,
    num_predict = num_predict,
    prompt_budget_chars = prompt_budget_chars,
    include_schema = FALSE,
    label_mode = label_mode,
    dynamic_candidates = dynamic_candidates,
    internal_prompt_version = internal_prompt_version,
    extra_template_values = list(
      "{{DRAFT_ANALYSIS_TEXT}}" = draft_analysis_text,
      "{{CATEGORY_LABEL_TEXT}}" = category_label_text
    )
  )
}

.build_cluster_label_summary_prompt <- function(
    evidence,
    summary_variant,
    draft_analysis_text,
    selected_label_text,
    temperature,
    top_p,
    seed,
    num_predict,
    prompt_budget_chars,
    internal_prompt_version,
    category_label_text = NULL,
    subcategory_labels_text = NULL
) {
  .build_cluster_label_prompt(
    evidence = evidence,
    variant = summary_variant,
    schema_path = NULL,
    temperature = temperature,
    top_p = top_p,
    seed = seed,
    num_predict = num_predict,
    prompt_budget_chars = prompt_budget_chars,
    include_schema = FALSE,
    internal_prompt_version = internal_prompt_version,
    extra_template_values = list(
      "{{DRAFT_ANALYSIS_TEXT}}" = draft_analysis_text,
      "{{SELECTED_LABEL_TEXT}}" = selected_label_text,
      "{{CATEGORY_LABEL_TEXT}}" = category_label_text %||% "",
      "{{SUBCATEGORY_LABELS_TEXT}}" = subcategory_labels_text %||% ""
    )
  )
}

.build_cluster_label_abstain_reason_prompt <- function(
    evidence,
    abstain_reason_variant,
    draft_analysis_text,
    label_decision_text,
    temperature,
    top_p,
    seed,
    num_predict,
    prompt_budget_chars,
    internal_prompt_version
) {
  .build_cluster_label_prompt(
    evidence = evidence,
    variant = abstain_reason_variant,
    schema_path = NULL,
    temperature = temperature,
    top_p = top_p,
    seed = seed,
    num_predict = num_predict,
    prompt_budget_chars = prompt_budget_chars,
    include_schema = FALSE,
    internal_prompt_version = internal_prompt_version,
    extra_template_values = list(
      "{{DRAFT_ANALYSIS_TEXT}}" = draft_analysis_text,
      "{{LABEL_DECISION_TEXT}}" = label_decision_text
    )
  )
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
    prompt_budget_chars,
    internal_prompt_version
) {
  .build_cluster_label_prompt(
    evidence = evidence,
    variant = selection_variant,
    schema_path = NULL,
    temperature = temperature,
    top_p = top_p,
    seed = seed,
    num_predict = num_predict,
    prompt_budget_chars = prompt_budget_chars,
    include_schema = FALSE,
    label_mode = label_mode,
    dynamic_candidates = dynamic_candidates,
    internal_prompt_version = internal_prompt_version,
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
    prompt_budget_chars,
    internal_prompt_version
) {
  selection_text <- .render_cluster_label_selection_text_block(selection_output)

  .build_cluster_label_prompt(
    evidence = evidence,
    variant = explanation_variant,
    schema_path = NULL,
    temperature = temperature,
    top_p = top_p,
    seed = seed,
    num_predict = num_predict,
    prompt_budget_chars = prompt_budget_chars,
    include_schema = FALSE,
    internal_prompt_version = internal_prompt_version,
    extra_template_values = list(
      "{{DRAFT_ANALYSIS_TEXT}}" = draft_analysis_text,
      "{{LABEL_SELECTION_TEXT}}" = selection_text
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

.run_cluster_label_decision_ladder <- function(
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
    label_stage_log_paths,
    short_label_with_llm,
    internal_prompt_version
) {
  cascade <- .cluster_label_decision_cascade_variants(variant)
  attempts <- list()
  failure_messages <- character(0)
  abstain_reasons <- character(0)
  decision_retry_budget <- .cluster_label_single_retry_budget(max_retries)

  for (i in seq_along(cascade$public_variants)) {
    public_variant <- cascade$public_variants[[i]]
    decision_variant <- cascade$internal_variants[[i]]
    shortening_repair_prompt <- if (isTRUE(short_label_with_llm)) {
      .cluster_label_decision_shortening_repair_prompt(
        variant = public_variant,
        internal_prompt_version = internal_prompt_version
      )
    } else {
      list(
        public_variant = public_variant,
        variant = NULL,
        text = NULL,
        path = NULL,
        available = FALSE
      )
    }
    prompt_bundle <- .build_cluster_label_decision_prompt(
      evidence = evidence,
      decision_variant = decision_variant,
      draft_analysis_text = draft_analysis_text,
      label_mode = label_mode,
      dynamic_candidates = dynamic_candidates,
      temperature = temperature,
      top_p = top_p,
      seed = seed,
      num_predict = num_predict,
      prompt_budget_chars = prompt_budget_chars,
      internal_prompt_version = internal_prompt_version
    )
    .expect_prompt_task_type(prompt_bundle, "label_decision", decision_variant)

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
        variant = decision_variant,
        prompt_bundle = prompt_bundle,
        keep_alive = keep_alive,
        ollama_options = ollama_options,
        endpoint = endpoint,
        timeout_sec = timeout_sec,
        max_retries = decision_retry_budget,
        request_fn = request_fn,
        log_paths = stage_log_paths,
        parse_output_fn = function(content) {
          output <- .parse_cluster_label_label_decision_text(
            content = content,
            cluster_id = evidence$meta$cluster_id
          )
          output <- tryCatch(
            .assert_cluster_label_label_decision_stage_output(
              output = output,
              prompt_bundle = prompt_bundle,
              stage_name = "label_decision"
            ),
            error = function(e) e
          )
          if (inherits(output, "error")) {
            output$parsed_output_candidate <- .parse_cluster_label_label_decision_text(
              content = content,
              cluster_id = evidence$meta$cluster_id
            )
            stop(output)
          }
          output
        },
        stage_name = "label_decision",
        repair_instruction = function(error_message, content = NULL, parsed_output = NULL, ...) {
          if (isTRUE(shortening_repair_prompt$available) &&
              .is_cluster_label_decision_overlength_error(error_message)) {
            dynamic_shortening_prompt <- .cluster_label_decision_shortening_repair_prompt(
              variant = public_variant,
              internal_prompt_version = internal_prompt_version,
              long_label = .cluster_label_decision_failed_label_text(
                content = content,
                parsed_output_candidate = parsed_output
              ),
              label_description = .cluster_label_shortening_description_text(
                draft_analysis_text
              )
            )
            if (isTRUE(dynamic_shortening_prompt$available)) {
              return(dynamic_shortening_prompt$text)
            }
          }
          .default_label_decision_stage_repair_instruction()
        }
      ),
      error = function(e) e
    )

    if (inherits(attempt, "error")) {
      compact_error <- .cluster_label_compact_stage_error_message(
        conditionMessage(attempt)
      )
      failure_messages <- c(
        failure_messages,
        paste0(
          public_variant,
          ": invalid label-decision output after ",
          decision_retry_budget + 1L,
          " attempt(s): ",
          compact_error %||% conditionMessage(attempt)
        )
      )
      attempts[[length(attempts) + 1L]] <- list(
        variant = public_variant,
        selection_variant = decision_variant,
        result = "failed_after_retry",
        error = conditionMessage(attempt),
        attempts = decision_retry_budget + 1L,
        retry_exhausted = TRUE,
        prompt = attempt$prompt %||% prompt_bundle,
        request = attempt$request %||% NULL,
        response = attempt$response %||% NULL,
        output = attempt$output %||% .cluster_label_error_candidate_output(attempt),
        repair_history = attempt$repair_history %||% list(),
        repair_source = .cluster_label_decision_repair_source_from_history(
          repair_history = attempt$repair_history %||% list(),
          prompt_bundle = prompt_bundle
        ),
        repair_variant = if (
          isTRUE(shortening_repair_prompt$available) &&
            identical(
              .cluster_label_decision_repair_source_from_history(
                repair_history = attempt$repair_history %||% list(),
                prompt_bundle = prompt_bundle
              ),
              "shortening_branch"
            )
        ) {
          shortening_repair_prompt$variant
        } else {
          NULL
        },
        logs = attempt$logs %||% stage_log_paths
      )
      next
    }

    attempt$public_variant <- public_variant
    attempt$selection_variant <- decision_variant
    attempt$result <- .as_scalar_character(attempt$output$status)
    attempt$repair_source <- .cluster_label_decision_repair_source(
      attempt = attempt,
      prompt_bundle = prompt_bundle
    )
    attempt$repair_variant <- if (identical(attempt$repair_source, "shortening_branch") &&
                                  isTRUE(shortening_repair_prompt$available)) {
      shortening_repair_prompt$variant
    } else {
      NULL
    }
    attempt$retry_exhausted <- FALSE
    attempts[[length(attempts) + 1L]] <- attempt

    if (identical(attempt$output$status, "abstain")) {
      abstain_reasons <- c(
        abstain_reasons,
        .as_scalar_character(attempt$output$label_decision_text)
      )
      failure_messages <- c(
        failure_messages,
        paste0(
          public_variant,
          ": abstained",
          if (.is_non_empty_scalar_character(attempt$output$label_decision_text)) {
            paste0(" (", attempt$output$label_decision_text, ")")
          } else {
            ""
          }
        )
      )
      next
    }

    return(list(
      attempts = attempts,
      selection_output = attempt$output,
      decision_output = attempt$output,
      selected_public_variant = public_variant,
      selected_selection_variant = decision_variant,
      selected_stage = attempt,
      exhausted = FALSE,
      failure_messages = failure_messages,
      repair_source = attempt$repair_source %||% NULL,
      repair_variant = attempt$repair_variant %||% NULL,
      logs = label_stage_log_paths
    ))
  }

  abstain_output <- .cluster_label_selection_all_abstain_output(
    evidence = evidence,
    abstain_reasons = abstain_reasons,
    failure_messages = failure_messages
  )

  list(
    attempts = attempts,
    selection_output = abstain_output,
    decision_output = abstain_output,
    selected_public_variant = "selection_all_abstain",
    selected_selection_variant = NULL,
    selected_stage = NULL,
    exhausted = TRUE,
    failure_messages = failure_messages,
    repair_source = NULL,
    repair_variant = NULL,
    logs = label_stage_log_paths
  )
}

.run_cluster_label_clean_ladder <- function(
    evidence,
    provider,
    model,
    stage_name,
    task_type,
    variants,
    build_prompt_fn,
    parse_output_fn,
    abstain_is_terminal,
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
    request_fn,
    stage_log_paths
) {
  attempts <- list()
  failure_messages <- character(0)
  clean_retry_budget <- 3L

  for (i in seq_along(variants)) {
    stage_variant <- variants[[i]]
    prompt_bundle <- build_prompt_fn(stage_variant)
    .expect_prompt_task_type(prompt_bundle, task_type, stage_variant)

    attempt_log_paths <- .label_selection_attempt_log_paths(
      label_stage_log_paths = stage_log_paths,
      index = i,
      public_variant = stage_variant
    )

    attempt <- tryCatch(
      .run_structured_llm_stage(
        evidence = evidence,
        provider = provider,
        model = model,
        variant = stage_variant,
        prompt_bundle = prompt_bundle,
        keep_alive = keep_alive,
        ollama_options = ollama_options,
        endpoint = endpoint,
        timeout_sec = timeout_sec,
        max_retries = clean_retry_budget,
        request_fn = request_fn,
        log_paths = attempt_log_paths,
        parse_output_fn = parse_output_fn,
        stage_name = stage_name,
        repair_instruction = .default_clean_name_stage_repair_instruction(stage_name)
      ),
      error = function(e) e
    )

    if (inherits(attempt, "error")) {
      compact_error <- .cluster_label_compact_stage_error_message(
        conditionMessage(attempt)
      )
      failure_messages <- c(
        failure_messages,
        paste0(
          stage_variant,
          ": invalid clean answer after ",
          clean_retry_budget + 1L,
          " attempt(s): ",
          compact_error %||% conditionMessage(attempt)
        )
      )
      attempts[[length(attempts) + 1L]] <- list(
        variant = stage_variant,
        result = "failed_after_retry",
        error = conditionMessage(attempt),
        attempts = clean_retry_budget + 1L,
        retry_exhausted = TRUE,
        prompt = attempt$prompt %||% prompt_bundle,
        request = attempt$request %||% NULL,
        response = attempt$response %||% NULL,
        output = attempt$output %||% .cluster_label_error_candidate_output(attempt),
        repair_history = attempt$repair_history %||% list(),
        logs = attempt$logs %||% attempt_log_paths
      )
      next
    }

    attempt$result <- .as_scalar_character(attempt$output$status)
    attempt$retry_exhausted <- FALSE
    attempts[[length(attempts) + 1L]] <- attempt

    if (identical(attempt$output$status, "abstain") && isTRUE(abstain_is_terminal)) {
      failure_messages <- c(failure_messages, paste0(stage_variant, ": abstained"))
      next
    }

    return(list(
      attempts = attempts,
      output = attempt$output,
      selected_variant = stage_variant,
      selected_stage = attempt,
      exhausted = FALSE,
      failure_messages = failure_messages,
      logs = stage_log_paths
    ))
  }

  list(
    attempts = attempts,
    output = list(
      schema_version = "0.1.0",
      cluster_id = evidence$meta$cluster_id,
      status = "abstain",
      label_decision_text = "ABSTAIN",
      category_label = NULL,
      subcategory_labels = character(0)
    ),
    selected_variant = NULL,
    selected_stage = NULL,
    exhausted = TRUE,
    failure_messages = failure_messages,
    logs = stage_log_paths
  )
}

.default_clean_name_stage_repair_instruction <- function(stage_name) {
  function(error_message, ...) {
    paste(
      "Your previous answer was not a clean name.",
      paste0("Parser error: ", error_message),
      "Reply again with only the requested clean name.",
      "Do not include prefixes such as LABEL:, CATEGORY:, CATEGORY_LABEL:, SUBCATEGORY:, or SUBCATEGORY_LABELS:.",
      "Do not include quotes, bullets, explanations, commas, brackets, or a final period.",
      if (identical(stage_name, "subcategory_decision")) {
        "For no safe subcategory, reply only: none"
      } else {
        "For no safe category, reply only: ABSTAIN"
      },
      sep = "\n"
    )
  }
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
    label_stage_log_paths,
    internal_prompt_version
) {
  cascade <- .cluster_label_selection_cascade_variants(variant)
  attempts <- list()
  failure_messages <- character(0)
  abstain_reasons <- character(0)
  selection_retry_budget <- .cluster_label_single_retry_budget(max_retries)

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
      prompt_budget_chars = prompt_budget_chars,
      internal_prompt_version = internal_prompt_version
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
        max_retries = selection_retry_budget,
        request_fn = request_fn,
        log_paths = stage_log_paths,
        parse_output_fn = function(content) {
          output <- .parse_cluster_label_selection_text(
            content = content,
            cluster_id = evidence$meta$cluster_id
          )
          .assert_cluster_label_selection_stage_output(
            output = output,
            prompt_bundle = prompt_bundle,
            stage_name = "label_selection"
          )
        },
        stage_name = "label_selection",
        repair_instruction = .default_selection_stage_repair_instruction()
      ),
      error = function(e) e
    )

    if (inherits(attempt, "error")) {
      compact_error <- .cluster_label_compact_stage_error_message(
        conditionMessage(attempt)
      )
      failure_messages <- c(
        failure_messages,
        paste0(
          public_variant,
          ": invalid text-field output after ",
          selection_retry_budget + 1L,
          " attempt(s): ",
          compact_error %||% conditionMessage(attempt)
        )
      )
      attempts[[length(attempts) + 1L]] <- list(
        variant = public_variant,
        selection_variant = selection_variant,
        result = "failed_after_retry",
        error = conditionMessage(attempt),
        attempts = selection_retry_budget + 1L,
        retry_exhausted = TRUE,
        logs = stage_log_paths
      )
      next
    }

    attempt$public_variant <- public_variant
    attempt$selection_variant <- selection_variant
    attempt$result <- .as_scalar_character(attempt$output$status)
    attempt$retry_exhausted <- FALSE
    attempts[[length(attempts) + 1L]] <- attempt

    if (identical(attempt$output$status, "abstain")) {
      abstain_reasons <- c(
        abstain_reasons,
        .as_scalar_character(attempt$output$abstain_reason)
      )
      failure_messages <- c(
        failure_messages,
        paste0(
          public_variant,
          ": abstained",
          if (.is_non_empty_scalar_character(attempt$output$abstain_reason)) {
            paste0(" (", attempt$output$abstain_reason, ")")
          } else {
            ""
          }
        )
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
      failure_messages = failure_messages,
      logs = label_stage_log_paths
    ))
  }

  abstain_output <- .cluster_label_selection_all_abstain_output(
    evidence = evidence,
    abstain_reasons = abstain_reasons,
    failure_messages = failure_messages
  )

  list(
    attempts = attempts,
    selection_output = abstain_output,
    selected_public_variant = "selection_all_abstain",
    selected_selection_variant = NULL,
    selected_stage = NULL,
    exhausted = TRUE,
    failure_messages = failure_messages,
    logs = label_stage_log_paths
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
    internal_prompt_version,
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
    label_mode = label_mode,
    internal_prompt_version = internal_prompt_version
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
    internal_prompt_version,
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
    prompt_budget_chars = prompt_budget_chars,
    internal_prompt_version = internal_prompt_version
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
    label_mode = label_mode,
    internal_prompt_version = internal_prompt_version
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

.cluster_label_reused_draft_stage <- function(
    cluster_id,
    draft_variant,
    workflow_stage_logs,
    draft_analysis_text,
    use_brainstorm
) {
  draft_analysis_text <- .as_scalar_character(draft_analysis_text)
  if (is.na(draft_analysis_text) || !nzchar(trimws(draft_analysis_text))) {
    draft_analysis_text <- if (isTRUE(use_brainstorm)) {
      paste(
        "Brainstorm was enabled earlier, but no reusable draft analysis text was available.",
        "Proceed from the cluster evidence directly."
      )
    } else {
      .cluster_label_brainstorm_disabled_text()
    }
  }

  list(
    variant = draft_variant,
    prompt = NULL,
    request = NULL,
    response = NULL,
    output = list(
      cluster_id = cluster_id,
      status = "draft_reused",
      draft_analysis = draft_analysis_text
    ),
    attempts = 0L,
    logs = workflow_stage_logs,
    skipped = TRUE,
    skip_reason = "draft_analysis_reused"
  )
}

.cluster_label_decomposed_workflow_logs <- function(workflow_logs) {
  if (is.null(workflow_logs$run_dir) || !nzchar(workflow_logs$run_dir)) {
    workflow_logs$stages <- stats::setNames(
      lapply(
        c("draft", "category", "subcategory", "summary", "abstain_reason"),
        function(...) .null_stage_log_paths()
      ),
      c("draft", "category", "subcategory", "summary", "abstain_reason")
    )
    return(workflow_logs)
  }

  stage_names <- c("draft", "category", "subcategory", "summary", "abstain_reason")
  stage_dirs <- c(
    "stage1_draft",
    "stage2_category",
    "stage3_subcategory",
    "stage4_label_summary",
    "stage5_abstain_reason"
  )
  workflow_logs$stages <- stats::setNames(
    lapply(seq_along(stage_names), function(i) {
      .stage_log_paths(
        file.path(workflow_logs$run_dir, stage_dirs[[i]]),
        started_at = workflow_logs$started_at
      )
    }),
    stage_names
  )
  workflow_logs
}

.cluster_label_decomposed_display_label <- function(category_label, subcategory_labels) {
  category_label <- .as_scalar_character(category_label)
  if (is.na(category_label) || !nzchar(trimws(category_label))) {
    return(NA_character_)
  }
  gsub("\\s+", " ", trimws(category_label), perl = TRUE)
}

.cluster_label_decomposed_selection_output <- function(
    evidence,
    category_output,
    subcategory_output
) {
  category_label <- .as_scalar_character(category_output$category_label)
  subcategory_labels <- .cluster_label_subcategory_labels(
    subcategory_output$subcategory_labels
  )
  display_label <- .cluster_label_decomposed_display_label(
    category_label,
    subcategory_labels
  )
  canonical_label <- .cluster_label_candidate_to_canonical(display_label)

  list(
    schema_version = "0.1.0",
    cluster_id = evidence$meta$cluster_id,
    status = "labeled",
    label_decision_text = display_label,
    canonical_label = canonical_label,
    display_label = display_label,
    category_label = category_label,
    subcategory_labels = subcategory_labels
  )
}

.llm_label_cluster_decomposed_pipeline <- function(
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
    workflow_steps,
    use_brainstorm,
    short_label_with_llm,
    internal_prompt_version,
    dry_run,
    log_dir,
    request_fn,
    draft_analysis_text_override = NULL,
    draft_candidates_override = NULL,
    selection_context_extra_text = NULL,
    explanation_context_extra_text = NULL,
    workflow_variant_suffix = NULL
) {
  draft_variant <- .default_cluster_label_draft_variant()
  category_variants <- .default_cluster_label_category_variants()
  subcategory_variants <- .default_cluster_label_subcategory_variants()
  summary_variant <- .default_cluster_label_v2_label_summary_variant()
  abstain_reason_variant <- .default_cluster_label_v2_abstain_reason_variant()
  workflow_variant_suffix <- .as_scalar_character(workflow_variant_suffix)
  if (is.na(workflow_variant_suffix) || !nzchar(workflow_variant_suffix)) {
    workflow_variant_suffix <- "_decomposed"
  }
  workflow_variant <- paste0(variant, workflow_variant_suffix)
  has_draft_override <- !is.na(.as_scalar_character(draft_analysis_text_override)) &&
    nzchar(trimws(.as_scalar_character(draft_analysis_text_override)))

  draft_prompt_bundle <- NULL
  if (isTRUE(use_brainstorm) && !isTRUE(has_draft_override)) {
    draft_prompt_bundle <- .build_cluster_label_prompt(
      evidence = evidence,
      variant = draft_variant,
      schema_path = NULL,
      temperature = temperature,
      top_p = top_p,
      seed = seed,
      num_predict = num_predict,
      prompt_budget_chars = prompt_budget_chars,
      include_schema = FALSE,
      internal_prompt_version = internal_prompt_version
    )
    .expect_prompt_task_type(draft_prompt_bundle, "draft", draft_variant)
  }

  dry_placeholders <- .cluster_label_v2_placeholders(
    cluster_id = evidence$meta$cluster_id
  )
  dry_draft_analysis_text <- if (isTRUE(has_draft_override)) {
    .as_scalar_character(draft_analysis_text_override)
  } else if (isTRUE(use_brainstorm)) {
    dry_placeholders$draft_analysis
  } else {
    .cluster_label_brainstorm_disabled_text()
  }
  dry_candidates <- if (is.list(draft_candidates_override)) {
    draft_candidates_override
  } else if (isTRUE(use_brainstorm)) {
    .extract_cluster_label_candidates_from_draft(dry_draft_analysis_text)
  } else {
    list()
  }
  dry_selection_context_text <- .compose_cluster_label_selection_context_text(
    draft_analysis_text = dry_draft_analysis_text,
    candidates = dry_candidates,
    use_brainstorm = use_brainstorm,
    extra_guidance_text = selection_context_extra_text
  )
  dry_explanation_context_text <- .compose_cluster_label_explanation_context_text(
    draft_analysis_text = dry_draft_analysis_text,
    use_brainstorm = use_brainstorm,
    extra_guidance_text = explanation_context_extra_text
  )

  category_prompt_bundle <- .build_cluster_label_category_prompt(
    evidence = evidence,
    category_variant = category_variants[[1L]],
    draft_analysis_text = dry_selection_context_text,
    label_mode = label_mode,
    dynamic_candidates = dry_candidates,
    temperature = temperature,
    top_p = top_p,
    seed = seed,
    num_predict = num_predict,
    prompt_budget_chars = prompt_budget_chars,
    internal_prompt_version = internal_prompt_version
  )
  .expect_prompt_task_type(category_prompt_bundle, "category_decision", category_variants[[1L]])

  subcategory_prompt_bundle <- .build_cluster_label_subcategory_prompt(
    evidence = evidence,
    subcategory_variant = subcategory_variants[[1L]],
    draft_analysis_text = dry_selection_context_text,
    category_label_text = dry_placeholders$labeled_decision$category_label,
    label_mode = label_mode,
    dynamic_candidates = dry_candidates,
    temperature = temperature,
    top_p = top_p,
    seed = seed,
    num_predict = num_predict,
    prompt_budget_chars = prompt_budget_chars,
    internal_prompt_version = internal_prompt_version
  )
  .expect_prompt_task_type(subcategory_prompt_bundle, "subcategory_decision", subcategory_variants[[1L]])

  dry_selection_output <- .cluster_label_decomposed_selection_output(
    evidence = evidence,
    category_output = list(category_label = dry_placeholders$labeled_decision$category_label),
    subcategory_output = list(subcategory_labels = dry_placeholders$labeled_decision$subcategory_labels)
  )
  summary_prompt_bundle <- .build_cluster_label_summary_prompt(
    evidence = evidence,
    summary_variant = summary_variant,
    draft_analysis_text = dry_explanation_context_text,
    selected_label_text = dry_selection_output$display_label,
    category_label_text = dry_selection_output$category_label,
    subcategory_labels_text = .cluster_label_subcategory_labels_text(
      dry_selection_output$subcategory_labels
    ),
    temperature = temperature,
    top_p = top_p,
    seed = seed,
    num_predict = num_predict,
    prompt_budget_chars = prompt_budget_chars,
    internal_prompt_version = internal_prompt_version
  )
  .expect_prompt_task_type(summary_prompt_bundle, "label_summary", summary_variant)

  abstain_reason_prompt_bundle <- .build_cluster_label_abstain_reason_prompt(
    evidence = evidence,
    abstain_reason_variant = abstain_reason_variant,
    draft_analysis_text = dry_explanation_context_text,
    label_decision_text = "ABSTAIN",
    temperature = temperature,
    top_p = top_p,
    seed = seed,
    num_predict = num_predict,
    prompt_budget_chars = prompt_budget_chars,
    internal_prompt_version = internal_prompt_version
  )
  .expect_prompt_task_type(abstain_reason_prompt_bundle, "abstain_reason", abstain_reason_variant)

  dry_run_out <- list(
    cluster_id = evidence$meta$cluster_id,
    provider = provider,
    model = model,
    variant = workflow_variant,
    workflow_steps = workflow_steps,
    prompt = summary_prompt_bundle,
    request = .build_ollama_label_request(
      model = model,
      prompt_bundle = summary_prompt_bundle,
      keep_alive = keep_alive,
      ollama_options = ollama_options
    ),
    schema_path = summary_prompt_bundle$schema_path,
    workflow = list(
      draft_variant = draft_variant,
      draft = list(prompt = draft_prompt_bundle),
      category = list(variants = category_variants, prompt = category_prompt_bundle),
      subcategory = list(variants = subcategory_variants, prompt = subcategory_prompt_bundle),
      summary_variant = summary_variant,
      summary = list(prompt = summary_prompt_bundle),
      abstain_reason_variant = abstain_reason_variant,
      abstain_reason = list(prompt = abstain_reason_prompt_bundle)
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
    variant = workflow_variant,
    workflow_steps = workflow_steps
  )
  workflow_logs <- .cluster_label_decomposed_workflow_logs(workflow_logs)

  .write_workflow_metadata(
    workflow_logs,
    list(
      started_at = workflow_logs$started_at,
      cluster_id = evidence$meta$cluster_id,
      provider = provider,
      model = model,
      variant = workflow_variant,
      workflow_steps = workflow_steps,
      use_brainstorm = isTRUE(use_brainstorm),
      internal_prompt_version = internal_prompt_version,
      draft_override_used = isTRUE(has_draft_override),
      draft_variant = draft_variant,
      category_variants = unname(category_variants),
      subcategory_variants = unname(subcategory_variants),
      summary_variant = summary_variant,
      abstain_reason_variant = abstain_reason_variant,
      final_schema_path = NULL,
      base_url = endpoint,
      status = "started",
      run_dir = workflow_logs$run_dir
    )
  )

  result <- tryCatch({
    draft_stage <- if (isTRUE(has_draft_override)) {
      .cluster_label_reused_draft_stage(
        cluster_id = evidence$meta$cluster_id,
        draft_variant = draft_variant,
        workflow_stage_logs = workflow_logs$stages$draft,
        draft_analysis_text = draft_analysis_text_override,
        use_brainstorm = use_brainstorm
      )
    } else if (isTRUE(use_brainstorm)) {
      .run_structured_llm_stage(
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
    } else {
      list(
        variant = draft_variant,
        prompt = NULL,
        request = NULL,
        response = NULL,
        output = list(
          cluster_id = evidence$meta$cluster_id,
          status = "draft_skipped",
          draft_analysis = .cluster_label_brainstorm_disabled_text()
        ),
        attempts = 0L,
        logs = workflow_logs$stages$draft,
        skipped = TRUE,
        skip_reason = "brainstorm_disabled"
      )
    }

    draft_analysis_text <- draft_stage$output$draft_analysis %||%
      .cluster_label_brainstorm_disabled_text()
    draft_candidates <- if (is.list(draft_candidates_override)) {
      draft_candidates_override
    } else if (isTRUE(use_brainstorm)) {
      .extract_cluster_label_candidates_from_draft(draft_analysis_text)
    } else {
      list()
    }
    selection_context_text <- .compose_cluster_label_selection_context_text(
      draft_analysis_text = draft_analysis_text,
      candidates = draft_candidates,
      use_brainstorm = use_brainstorm,
      extra_guidance_text = selection_context_extra_text
    )
    explanation_context_text <- .compose_cluster_label_explanation_context_text(
      draft_analysis_text = draft_analysis_text,
      use_brainstorm = use_brainstorm,
      extra_guidance_text = explanation_context_extra_text
    )

    category_stage <- .run_cluster_label_clean_ladder(
      evidence = evidence,
      provider = provider,
      model = model,
      stage_name = "category_decision",
      task_type = "category_decision",
      variants = category_variants,
      build_prompt_fn = function(category_variant) {
        .build_cluster_label_category_prompt(
          evidence = evidence,
          category_variant = category_variant,
          draft_analysis_text = selection_context_text,
          label_mode = label_mode,
          dynamic_candidates = draft_candidates,
          temperature = temperature,
          top_p = top_p,
          seed = seed,
          num_predict = num_predict,
          prompt_budget_chars = prompt_budget_chars,
          internal_prompt_version = internal_prompt_version
        )
      },
      parse_output_fn = function(content) {
        .parse_cluster_label_category_decision_text(
          content = content,
          cluster_id = evidence$meta$cluster_id
        )
      },
      abstain_is_terminal = TRUE,
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
      request_fn = request_fn,
      stage_log_paths = workflow_logs$stages$category
    )

    decision_output <- category_stage$output
    subcategory_stage <- list(
      attempts = list(),
      output = list(
        schema_version = "0.1.0",
        cluster_id = evidence$meta$cluster_id,
        status = "subcategory_skipped",
        subcategory_labels = character(0)
      ),
      selected_variant = NULL,
      selected_stage = NULL,
      exhausted = FALSE,
      skipped = TRUE,
      skip_reason = "category_abstained",
      failure_messages = character(0),
      logs = workflow_logs$stages$subcategory
    )

    if (!identical(decision_output$status, "abstain")) {
      subcategory_stage <- .run_cluster_label_clean_ladder(
        evidence = evidence,
        provider = provider,
        model = model,
        stage_name = "subcategory_decision",
        task_type = "subcategory_decision",
        variants = subcategory_variants,
        build_prompt_fn = function(subcategory_variant) {
          .build_cluster_label_subcategory_prompt(
            evidence = evidence,
            subcategory_variant = subcategory_variant,
            draft_analysis_text = selection_context_text,
            category_label_text = decision_output$category_label,
            label_mode = label_mode,
            dynamic_candidates = draft_candidates,
            temperature = temperature,
            top_p = top_p,
            seed = seed,
            num_predict = num_predict,
            prompt_budget_chars = prompt_budget_chars,
            internal_prompt_version = internal_prompt_version
          )
        },
        parse_output_fn = function(content) {
          .parse_cluster_label_subcategory_decision_text(
            content = content,
            cluster_id = evidence$meta$cluster_id
          )
        },
        abstain_is_terminal = FALSE,
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
        request_fn = request_fn,
        stage_log_paths = workflow_logs$stages$subcategory
      )

      if (isTRUE(subcategory_stage$exhausted) || identical(subcategory_stage$output$status, "abstain")) {
        subcategory_stage$output <- list(
          schema_version = "0.1.0",
          cluster_id = evidence$meta$cluster_id,
          status = "subcategory_ready",
          subcategory_labels = character(0)
        )
        subcategory_stage$fallback_used <- TRUE
        subcategory_stage$fallback_reason <- "No clean subcategory answer was selected; keeping category only."
      }

      decision_output <- .cluster_label_decomposed_selection_output(
        evidence = evidence,
        category_output = category_stage$output,
        subcategory_output = subcategory_stage$output
      )
    }

    summary_stage <- list(
      variant = summary_variant,
      prompt = NULL,
      request = NULL,
      response = NULL,
      output = list(
        cluster_id = evidence$meta$cluster_id,
        status = "summary_skipped",
        label_summary = NULL
      ),
      attempts = 0L,
      logs = workflow_logs$stages$summary,
      skipped = TRUE,
      skip_reason = "category_not_selected"
    )
    abstain_reason_stage <- list(
      variant = abstain_reason_variant,
      prompt = NULL,
      request = NULL,
      response = NULL,
      output = list(
        cluster_id = evidence$meta$cluster_id,
        status = "abstain_reason_skipped",
        abstain_reason = NULL
      ),
      attempts = 0L,
      logs = workflow_logs$stages$abstain_reason,
      skipped = TRUE,
      skip_reason = "category_selected"
    )

    if (!identical(decision_output$status, "abstain")) {
      summary_prompt_bundle <- .build_cluster_label_summary_prompt(
        evidence = evidence,
        summary_variant = summary_variant,
        draft_analysis_text = explanation_context_text,
        selected_label_text = decision_output$display_label,
        category_label_text = decision_output$category_label,
        subcategory_labels_text = .cluster_label_subcategory_labels_text(
          decision_output$subcategory_labels
        ),
        temperature = temperature,
        top_p = top_p,
        seed = seed,
        num_predict = num_predict,
        prompt_budget_chars = prompt_budget_chars,
        internal_prompt_version = internal_prompt_version
      )
      .expect_prompt_task_type(summary_prompt_bundle, "label_summary", summary_variant)

      summary_stage <- .run_structured_llm_stage(
        evidence = evidence,
        provider = provider,
        model = model,
        variant = summary_variant,
        prompt_bundle = summary_prompt_bundle,
        keep_alive = keep_alive,
        ollama_options = ollama_options,
        endpoint = endpoint,
        timeout_sec = timeout_sec,
        max_retries = .cluster_label_single_retry_budget(max_retries),
        request_fn = request_fn,
        log_paths = workflow_logs$stages$summary,
        parse_output_fn = function(content) {
          .parse_cluster_label_summary_text(
            content = content,
            cluster_id = evidence$meta$cluster_id
          )
        },
        stage_name = "label_summary",
        repair_instruction = .default_label_summary_stage_repair_instruction()
      )

      final_output <- .assemble_cluster_label_final_output(
        evidence = evidence,
        selection_output = decision_output,
        label_summary_text = summary_stage$output$label_summary,
        explanation_text = summary_stage$output$label_summary
      )
      terminal_stage <- summary_stage
      terminal_prompt_bundle <- summary_prompt_bundle
    } else {
      abstain_reason_prompt_bundle <- .build_cluster_label_abstain_reason_prompt(
        evidence = evidence,
        abstain_reason_variant = abstain_reason_variant,
        draft_analysis_text = explanation_context_text,
        label_decision_text = .render_cluster_label_decision_text(decision_output),
        temperature = temperature,
        top_p = top_p,
        seed = seed,
        num_predict = num_predict,
        prompt_budget_chars = prompt_budget_chars,
        internal_prompt_version = internal_prompt_version
      )
      .expect_prompt_task_type(abstain_reason_prompt_bundle, "abstain_reason", abstain_reason_variant)

      abstain_reason_stage <- .run_structured_llm_stage(
        evidence = evidence,
        provider = provider,
        model = model,
        variant = abstain_reason_variant,
        prompt_bundle = abstain_reason_prompt_bundle,
        keep_alive = keep_alive,
        ollama_options = ollama_options,
        endpoint = endpoint,
        timeout_sec = timeout_sec,
        max_retries = .cluster_label_single_retry_budget(max_retries),
        request_fn = request_fn,
        log_paths = workflow_logs$stages$abstain_reason,
        parse_output_fn = function(content) {
          .parse_cluster_label_abstain_reason_text(
            content = content,
            cluster_id = evidence$meta$cluster_id
          )
        },
        stage_name = "abstain_reason",
        repair_instruction = .default_abstain_reason_stage_repair_instruction()
      )

      final_output <- .assemble_cluster_label_final_output(
        evidence = evidence,
        selection_output = decision_output,
        abstain_reason_text = abstain_reason_stage$output$abstain_reason,
        explanation_text = abstain_reason_stage$output$abstain_reason
      )
      terminal_stage <- abstain_reason_stage
      terminal_prompt_bundle <- abstain_reason_prompt_bundle
    }

    explanation_stage <- list(
      variant = NULL,
      prompt = NULL,
      request = NULL,
      response = NULL,
      output = list(
        cluster_id = evidence$meta$cluster_id,
        status = "explanation_ready",
        explanation = final_output$explanation
      ),
      attempts = 0L,
      skipped = TRUE,
      skip_reason = "programmatic_v4_passthrough",
      fallback_used = isTRUE(terminal_stage$fallback_used),
      source_stage = if (identical(final_output$status, "labeled")) {
        "label_summary"
      } else {
        "abstain_reason"
      }
    )
    explanation_stage$assembled_output <- final_output
    .write_workflow_final_output(workflow_logs, final_output)

    category_attempt_total <- sum(vapply(category_stage$attempts, function(x) {
      as.integer(x$attempts %||% 0L)
    }, integer(1)))
    subcategory_attempt_total <- sum(vapply(subcategory_stage$attempts, function(x) {
      as.integer(x$attempts %||% 0L)
    }, integer(1)))
    total_attempts <- as.integer(
      (draft_stage$attempts %||% 0L) +
        category_attempt_total +
        subcategory_attempt_total +
        (terminal_stage$attempts %||% 0L)
    )

    .write_workflow_metadata(
      workflow_logs,
      list(
        started_at = workflow_logs$started_at,
        finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
        cluster_id = evidence$meta$cluster_id,
        provider = provider,
        model = model,
        variant = workflow_variant,
        workflow_steps = workflow_steps,
        use_brainstorm = isTRUE(use_brainstorm),
        draft_override_used = isTRUE(has_draft_override),
        draft_variant = draft_variant,
        category_variants = unname(category_variants),
        subcategory_variants = unname(subcategory_variants),
        selected_label_variant = category_stage$selected_variant,
        label_stage_exhausted = isTRUE(category_stage$exhausted),
        label_stage_failure_reason = .as_scalar_character(
          paste(category_stage$failure_messages %||% character(0), collapse = " | ")
        ),
        terminal_stage_name = if (identical(final_output$status, "labeled")) {
          "label_summary"
        } else {
          "abstain_reason"
        },
        final_schema_path = terminal_prompt_bundle$schema_path,
        base_url = endpoint,
        status = "success",
        executed_stages = if (identical(final_output$status, "labeled")) 4L else 3L,
        attempts = total_attempts,
        final_output_status = final_output$status,
        run_dir = workflow_logs$run_dir
      )
    )

    out <- list(
      cluster_id = evidence$meta$cluster_id,
      provider = provider,
      model = model,
      variant = workflow_variant,
      workflow_steps = workflow_steps,
      prompt = terminal_stage$prompt,
      request = terminal_stage$request,
      response = terminal_stage$response,
      output = final_output,
      attempts = total_attempts,
      schema_path = terminal_prompt_bundle$schema_path,
      logs = workflow_logs,
      workflow = list(
        draft_variant = draft_variant,
        draft = draft_stage,
        label = category_stage,
        category = category_stage,
        subcategory = subcategory_stage,
        summary_variant = summary_variant,
        summary = summary_stage,
        abstain_reason_variant = abstain_reason_variant,
        abstain_reason = abstain_reason_stage,
        explanation_variant = NULL,
        explanation = explanation_stage
      )
    )
    class(out) <- c("cluster_label_result", "list")
    out
  }, error = function(e) {
    if (!is.null(workflow_logs$error)) {
      .write_text_file(workflow_logs$error, conditionMessage(e))
    }
    stop(e)
  })

  result
}

.llm_label_cluster_fixed_pipeline <- function(
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
    workflow_steps,
    use_brainstorm,
    short_label_with_llm,
    internal_prompt_version,
    dry_run,
    log_dir,
    request_fn,
    draft_analysis_text_override = NULL,
    draft_candidates_override = NULL,
    selection_context_extra_text = NULL,
    explanation_context_extra_text = NULL,
    workflow_variant_suffix = NULL
) {
  internal_prompt_version <- .normalize_cluster_label_internal_prompt_version(
    internal_prompt_version
  )
  if (.is_cluster_label_decomposed_internal_prompt_version(internal_prompt_version)) {
    return(.llm_label_cluster_decomposed_pipeline(
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
      workflow_steps = workflow_steps,
      use_brainstorm = use_brainstorm,
      short_label_with_llm = short_label_with_llm,
      internal_prompt_version = internal_prompt_version,
      dry_run = dry_run,
      log_dir = log_dir,
      request_fn = request_fn,
      draft_analysis_text_override = draft_analysis_text_override,
      draft_candidates_override = draft_candidates_override,
      selection_context_extra_text = selection_context_extra_text,
      explanation_context_extra_text = explanation_context_extra_text,
      workflow_variant_suffix = workflow_variant_suffix
    ))
  }
  draft_variant <- .default_cluster_label_draft_variant()
  summary_variant <- .default_cluster_label_v2_label_summary_variant()
  abstain_reason_variant <- .default_cluster_label_v2_abstain_reason_variant()
  cascade <- .cluster_label_decision_cascade_variants(variant)
  workflow_variant_suffix <- .as_scalar_character(workflow_variant_suffix)
  if (is.na(workflow_variant_suffix) || !nzchar(workflow_variant_suffix)) {
    workflow_variant_suffix <- ""
  }
  workflow_variant <- paste0(
    variant,
    workflow_variant_suffix
  )
  has_draft_override <- !is.na(.as_scalar_character(draft_analysis_text_override)) &&
    nzchar(trimws(.as_scalar_character(draft_analysis_text_override)))

  draft_prompt_bundle <- NULL
  draft_request_payload <- NULL
  if (isTRUE(use_brainstorm) && !isTRUE(has_draft_override)) {
    draft_prompt_bundle <- .build_cluster_label_prompt(
      evidence = evidence,
      variant = draft_variant,
      schema_path = NULL,
      temperature = temperature,
      top_p = top_p,
      seed = seed,
      num_predict = num_predict,
      prompt_budget_chars = prompt_budget_chars,
      include_schema = FALSE,
      internal_prompt_version = internal_prompt_version
    )
    .expect_prompt_task_type(draft_prompt_bundle, "draft", draft_variant)
  }

  dry_placeholders <- .cluster_label_v2_placeholders(
    cluster_id = evidence$meta$cluster_id
  )
  dry_draft_analysis_text <- if (isTRUE(has_draft_override)) {
    .as_scalar_character(draft_analysis_text_override)
  } else if (isTRUE(use_brainstorm)) {
    dry_placeholders$draft_analysis
  } else {
    .cluster_label_brainstorm_disabled_text()
  }
  dry_candidates <- if (is.list(draft_candidates_override)) {
    draft_candidates_override
  } else if (isTRUE(has_draft_override) && isTRUE(use_brainstorm)) {
    .extract_cluster_label_candidates_from_draft(dry_draft_analysis_text)
  } else if (isTRUE(use_brainstorm)) {
    .extract_cluster_label_candidates_from_draft(dry_placeholders$draft_analysis)
  } else {
    list()
  }
  dry_selection_context_text <- .compose_cluster_label_selection_context_text(
    draft_analysis_text = dry_draft_analysis_text,
    candidates = dry_candidates,
    use_brainstorm = use_brainstorm,
    extra_guidance_text = selection_context_extra_text
  )
  dry_explanation_context_text <- .compose_cluster_label_explanation_context_text(
    draft_analysis_text = dry_draft_analysis_text,
    use_brainstorm = use_brainstorm,
    extra_guidance_text = explanation_context_extra_text
  )

  label_stage_dry <- lapply(seq_along(cascade$public_variants), function(i) {
    public_variant <- cascade$public_variants[[i]]
    decision_variant <- cascade$internal_variants[[i]]
    prompt_bundle <- .build_cluster_label_decision_prompt(
      evidence = evidence,
      decision_variant = decision_variant,
      draft_analysis_text = dry_selection_context_text,
      label_mode = label_mode,
      dynamic_candidates = dry_candidates,
      temperature = temperature,
      top_p = top_p,
      seed = seed,
      num_predict = num_predict,
      prompt_budget_chars = prompt_budget_chars,
      internal_prompt_version = internal_prompt_version
    )
    .expect_prompt_task_type(prompt_bundle, "label_decision", decision_variant)

    list(
      variant = public_variant,
      selection_variant = decision_variant,
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

  summary_prompt_bundle <- .build_cluster_label_summary_prompt(
    evidence = evidence,
    summary_variant = summary_variant,
    draft_analysis_text = dry_explanation_context_text,
    selected_label_text = dry_placeholders$labeled_decision$display_label,
    category_label_text = dry_placeholders$labeled_decision$category_label,
    subcategory_labels_text = .cluster_label_subcategory_labels_text(
      dry_placeholders$labeled_decision$subcategory_labels
    ),
    temperature = temperature,
    top_p = top_p,
    seed = seed,
    num_predict = num_predict,
    prompt_budget_chars = prompt_budget_chars,
    internal_prompt_version = internal_prompt_version
  )
  .expect_prompt_task_type(
    summary_prompt_bundle,
    "label_summary",
    summary_variant
  )

  abstain_reason_prompt_bundle <- .build_cluster_label_abstain_reason_prompt(
    evidence = evidence,
    abstain_reason_variant = abstain_reason_variant,
    draft_analysis_text = dry_explanation_context_text,
    label_decision_text = dry_placeholders$abstain_decision$label_decision_text,
    temperature = temperature,
    top_p = top_p,
    seed = seed,
    num_predict = num_predict,
    prompt_budget_chars = prompt_budget_chars,
    internal_prompt_version = internal_prompt_version
  )
  .expect_prompt_task_type(
    abstain_reason_prompt_bundle,
    "abstain_reason",
    abstain_reason_variant
  )

  if (isTRUE(use_brainstorm)) {
    draft_request_payload <- .build_ollama_label_request(
      model = model,
      prompt_bundle = draft_prompt_bundle,
      keep_alive = keep_alive,
      ollama_options = ollama_options
    )
  }
  summary_request_payload <- .build_ollama_label_request(
    model = model,
    prompt_bundle = summary_prompt_bundle,
    keep_alive = keep_alive,
    ollama_options = ollama_options
  )
  abstain_reason_request_payload <- .build_ollama_label_request(
    model = model,
    prompt_bundle = abstain_reason_prompt_bundle,
    keep_alive = keep_alive,
    ollama_options = ollama_options
  )

  dry_run_out <- list(
    cluster_id = evidence$meta$cluster_id,
    provider = provider,
    model = model,
    variant = variant,
    workflow_steps = workflow_steps,
    prompt = summary_prompt_bundle,
    request = summary_request_payload,
    schema_path = summary_prompt_bundle$schema_path,
    workflow = list(
      draft_variant = draft_variant,
      draft = if (isTRUE(has_draft_override)) {
        list(
          variant = draft_variant,
          prompt = NULL,
          request = NULL,
          schema_path = NULL,
          skipped = TRUE,
          skip_reason = "draft_analysis_reused"
        )
      } else if (isTRUE(use_brainstorm)) {
        list(
          variant = draft_variant,
          prompt = draft_prompt_bundle,
          request = draft_request_payload,
          schema_path = NULL
        )
      } else {
        list(
          variant = draft_variant,
          prompt = NULL,
          request = NULL,
          schema_path = NULL,
          skipped = TRUE,
          skip_reason = "brainstorm_disabled"
        )
      },
      label = list(
        variants = label_stage_dry
      ),
      summary_variant = summary_variant,
      summary = list(
        variant = summary_variant,
        prompt = summary_prompt_bundle,
        request = summary_request_payload,
        schema_path = summary_prompt_bundle$schema_path
      ),
      abstain_reason_variant = abstain_reason_variant,
      abstain_reason = list(
        variant = abstain_reason_variant,
        prompt = abstain_reason_prompt_bundle,
        request = abstain_reason_request_payload,
        schema_path = abstain_reason_prompt_bundle$schema_path
      ),
      explanation_variant = NULL,
      explanation = list(
        variant = NULL,
        prompt = NULL,
        request = NULL,
        schema_path = NULL,
        skipped = TRUE,
        skip_reason = "programmatic_v2_passthrough"
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
    variant = workflow_variant,
    workflow_steps = workflow_steps
  )

  .write_workflow_metadata(
    workflow_logs,
    list(
      started_at = workflow_logs$started_at,
      cluster_id = evidence$meta$cluster_id,
      provider = provider,
      model = model,
      variant = workflow_variant,
      workflow_steps = workflow_steps,
      use_brainstorm = isTRUE(use_brainstorm),
      internal_prompt_version = internal_prompt_version,
      draft_override_used = isTRUE(has_draft_override),
      draft_variant = draft_variant,
      summary_variant = summary_variant,
      abstain_reason_variant = abstain_reason_variant,
      label_variants = unname(cascade$public_variants),
      final_schema_path = NULL,
      base_url = endpoint,
      status = "started",
      run_dir = workflow_logs$run_dir
    )
  )

  result <- tryCatch({
    draft_stage <- if (isTRUE(has_draft_override)) {
      .cluster_label_reused_draft_stage(
        cluster_id = evidence$meta$cluster_id,
        draft_variant = draft_variant,
        workflow_stage_logs = workflow_logs$stages$draft,
        draft_analysis_text = draft_analysis_text_override,
        use_brainstorm = use_brainstorm
      )
    } else if (isTRUE(use_brainstorm)) {
      .run_structured_llm_stage(
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
    } else {
      list(
        variant = draft_variant,
        prompt = NULL,
        request = NULL,
        response = NULL,
        output = list(
          cluster_id = evidence$meta$cluster_id,
          status = "draft_skipped",
          draft_analysis = .cluster_label_brainstorm_disabled_text()
        ),
        attempts = 0L,
        logs = workflow_logs$stages$draft,
        skipped = TRUE,
        skip_reason = "brainstorm_disabled"
      )
    }

    draft_analysis_text <- draft_stage$output$draft_analysis %||%
      .cluster_label_brainstorm_disabled_text()
    draft_candidates <- if (is.list(draft_candidates_override)) {
      draft_candidates_override
    } else if (isTRUE(use_brainstorm)) {
      .extract_cluster_label_candidates_from_draft(draft_analysis_text)
    } else {
      list()
    }
    selection_context_text <- .compose_cluster_label_selection_context_text(
      draft_analysis_text = draft_analysis_text,
      candidates = draft_candidates,
      use_brainstorm = use_brainstorm,
      extra_guidance_text = selection_context_extra_text
    )
    explanation_context_text <- .compose_cluster_label_explanation_context_text(
      draft_analysis_text = draft_analysis_text,
      use_brainstorm = use_brainstorm,
      extra_guidance_text = explanation_context_extra_text
    )

    label_stage <- .run_cluster_label_decision_ladder(
      evidence = evidence,
      provider = provider,
      model = model,
      variant = variant,
      label_mode = label_mode,
      draft_analysis_text = selection_context_text,
      dynamic_candidates = draft_candidates,
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
      label_stage_log_paths = workflow_logs$stages$label,
      short_label_with_llm = short_label_with_llm,
      internal_prompt_version = internal_prompt_version
    )

    decision_output <- label_stage$selection_output
    terminal_stage <- NULL
    terminal_prompt_bundle <- NULL
    summary_stage <- list(
      variant = summary_variant,
      prompt = NULL,
      request = NULL,
      response = NULL,
      output = list(
        cluster_id = evidence$meta$cluster_id,
        status = "summary_skipped",
        label_summary = NULL
      ),
      attempts = 0L,
      logs = workflow_logs$stages$summary,
      skipped = TRUE,
      skip_reason = "label_not_selected"
    )
    abstain_reason_stage <- list(
      variant = abstain_reason_variant,
      prompt = NULL,
      request = NULL,
      response = NULL,
      output = list(
        cluster_id = evidence$meta$cluster_id,
        status = "abstain_reason_skipped",
        abstain_reason = NULL
      ),
      attempts = 0L,
      logs = workflow_logs$stages$abstain_reason,
      skipped = TRUE,
      skip_reason = "label_selected"
    )

    if (identical(decision_output$status, "labeled")) {
      summary_prompt_bundle <- .build_cluster_label_summary_prompt(
        evidence = evidence,
        summary_variant = summary_variant,
        draft_analysis_text = explanation_context_text,
        selected_label_text = decision_output$display_label,
        category_label_text = decision_output$category_label,
        subcategory_labels_text = .cluster_label_subcategory_labels_text(
          decision_output$subcategory_labels
        ),
        temperature = temperature,
        top_p = top_p,
        seed = seed,
        num_predict = num_predict,
        prompt_budget_chars = prompt_budget_chars,
        internal_prompt_version = internal_prompt_version
      )
      .expect_prompt_task_type(
        summary_prompt_bundle,
        "label_summary",
        summary_variant
      )

      summary_retry_budget <- .cluster_label_single_retry_budget(max_retries)
      summary_stage <- tryCatch(
        .run_structured_llm_stage(
          evidence = evidence,
          provider = provider,
          model = model,
          variant = summary_variant,
          prompt_bundle = summary_prompt_bundle,
          keep_alive = keep_alive,
          ollama_options = ollama_options,
          endpoint = endpoint,
          timeout_sec = timeout_sec,
          max_retries = summary_retry_budget,
          request_fn = request_fn,
          log_paths = workflow_logs$stages$summary,
          parse_output_fn = function(content) {
            .parse_cluster_label_summary_text(
              content = content,
              cluster_id = evidence$meta$cluster_id
            )
          },
          stage_name = "label_summary",
          repair_instruction = .default_label_summary_stage_repair_instruction()
        ),
        error = function(e) e
      )

      if (inherits(summary_stage, "error")) {
        fallback_summary <- .cluster_label_programmatic_fallback_label_summary(
          decision_output = decision_output,
          draft_analysis_text = draft_analysis_text,
          failure_messages = label_stage$failure_messages,
          summary_error = conditionMessage(summary_stage)
        )
        summary_stage <- list(
          variant = summary_variant,
          prompt = summary_prompt_bundle,
          request = NULL,
          response = NULL,
          output = list(
            cluster_id = evidence$meta$cluster_id,
            status = "summary_ready",
            label_summary = fallback_summary
          ),
          attempts = summary_retry_budget + 1L,
          logs = workflow_logs$stages$summary,
          fallback_used = TRUE,
          fallback_reason = conditionMessage(summary_stage)
        )
      }

      final_output <- .assemble_cluster_label_final_output(
        evidence = evidence,
        selection_output = decision_output,
        label_summary_text = summary_stage$output$label_summary,
        explanation_text = summary_stage$output$label_summary
      )
      terminal_stage <- summary_stage
      terminal_prompt_bundle <- summary_prompt_bundle
    } else {
      abstain_reason_prompt_bundle <- .build_cluster_label_abstain_reason_prompt(
        evidence = evidence,
        abstain_reason_variant = abstain_reason_variant,
        draft_analysis_text = explanation_context_text,
        label_decision_text = .render_cluster_label_decision_text(decision_output),
        temperature = temperature,
        top_p = top_p,
        seed = seed,
        num_predict = num_predict,
        prompt_budget_chars = prompt_budget_chars,
        internal_prompt_version = internal_prompt_version
      )
      .expect_prompt_task_type(
        abstain_reason_prompt_bundle,
        "abstain_reason",
        abstain_reason_variant
      )

      abstain_reason_retry_budget <- .cluster_label_single_retry_budget(max_retries)
      abstain_reason_stage <- tryCatch(
        .run_structured_llm_stage(
          evidence = evidence,
          provider = provider,
          model = model,
          variant = abstain_reason_variant,
          prompt_bundle = abstain_reason_prompt_bundle,
          keep_alive = keep_alive,
          ollama_options = ollama_options,
          endpoint = endpoint,
          timeout_sec = timeout_sec,
          max_retries = abstain_reason_retry_budget,
          request_fn = request_fn,
          log_paths = workflow_logs$stages$abstain_reason,
          parse_output_fn = function(content) {
            .parse_cluster_label_abstain_reason_text(
              content = content,
              cluster_id = evidence$meta$cluster_id
            )
          },
          stage_name = "abstain_reason",
          repair_instruction = .default_abstain_reason_stage_repair_instruction()
        ),
        error = function(e) e
      )

      if (inherits(abstain_reason_stage, "error")) {
        fallback_abstain_reason <- .cluster_label_programmatic_fallback_abstain_reason(
          decision_output = decision_output,
          draft_analysis_text = draft_analysis_text,
          failure_messages = label_stage$failure_messages,
          abstain_reason_error = conditionMessage(abstain_reason_stage)
        )
        abstain_reason_stage <- list(
          variant = abstain_reason_variant,
          prompt = abstain_reason_prompt_bundle,
          request = NULL,
          response = NULL,
          output = list(
            cluster_id = evidence$meta$cluster_id,
            status = "abstain_reason_ready",
            abstain_reason = fallback_abstain_reason
          ),
          attempts = abstain_reason_retry_budget + 1L,
          logs = workflow_logs$stages$abstain_reason,
          fallback_used = TRUE,
          fallback_reason = conditionMessage(abstain_reason_stage)
        )
      }

      final_output <- .assemble_cluster_label_final_output(
        evidence = evidence,
        selection_output = decision_output,
        abstain_reason_text = abstain_reason_stage$output$abstain_reason,
        explanation_text = abstain_reason_stage$output$abstain_reason
      )
      terminal_stage <- abstain_reason_stage
      terminal_prompt_bundle <- abstain_reason_prompt_bundle
    }

    explanation_stage <- list(
      variant = NULL,
      prompt = NULL,
      request = NULL,
      response = NULL,
      output = list(
        cluster_id = evidence$meta$cluster_id,
        status = "explanation_ready",
        explanation = final_output$explanation
      ),
      attempts = 0L,
      skipped = TRUE,
      skip_reason = "programmatic_v2_passthrough",
      fallback_used = isTRUE(terminal_stage$fallback_used),
      source_stage = if (identical(decision_output$status, "labeled")) {
        "label_summary"
      } else {
        "abstain_reason"
      }
    )
    explanation_stage$assembled_output <- final_output
    .write_workflow_final_output(workflow_logs, final_output)

    label_attempt_total <- sum(vapply(label_stage$attempts, function(x) {
      as.integer(x$attempts %||% 0L)
    }, integer(1)))
    total_attempts <- as.integer(
      (draft_stage$attempts %||% 0L) +
        label_attempt_total +
        (terminal_stage$attempts %||% 0L)
    )

    .write_workflow_metadata(
      workflow_logs,
      list(
        started_at = workflow_logs$started_at,
        finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
        cluster_id = evidence$meta$cluster_id,
        provider = provider,
        model = model,
        variant = workflow_variant,
        workflow_steps = workflow_steps,
        use_brainstorm = isTRUE(use_brainstorm),
        draft_override_used = isTRUE(has_draft_override),
        draft_variant = draft_variant,
        summary_variant = summary_variant,
        abstain_reason_variant = abstain_reason_variant,
        label_variants = unname(cascade$public_variants),
        selected_label_variant = label_stage$selected_public_variant,
        label_stage_exhausted = isTRUE(label_stage$exhausted),
        label_stage_failure_reason = .as_scalar_character(
          label_stage$selection_output$fallback_reason %||%
            label_stage$selection_output$failure_reason %||%
            paste(label_stage$failure_messages %||% character(0), collapse = " | ")
        ),
        terminal_stage_name = if (identical(decision_output$status, "labeled")) {
          "label_summary"
        } else {
          "abstain_reason"
        },
        final_schema_path = terminal_prompt_bundle$schema_path,
        base_url = endpoint,
        status = "success",
        executed_stages = if (isTRUE(use_brainstorm)) 3L else 2L,
        attempts = total_attempts,
        final_output_status = final_output$status,
        run_dir = workflow_logs$run_dir
      )
    )

    out <- list(
      cluster_id = evidence$meta$cluster_id,
      provider = provider,
      model = model,
      variant = workflow_variant,
      workflow_steps = workflow_steps,
      prompt = terminal_stage$prompt,
      request = terminal_stage$request,
      response = terminal_stage$response,
      output = final_output,
      attempts = total_attempts,
      schema_path = terminal_prompt_bundle$schema_path,
      logs = workflow_logs,
      workflow = list(
        draft_variant = draft_variant,
        draft = draft_stage,
        label = label_stage,
        summary_variant = summary_variant,
        summary = summary_stage,
        abstain_reason_variant = abstain_reason_variant,
        abstain_reason = abstain_reason_stage,
        explanation_variant = NULL,
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
          "Failed to obtain a valid fixed-pipeline cluster label result.",
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
        variant = workflow_variant,
        workflow_steps = workflow_steps,
        use_brainstorm = isTRUE(use_brainstorm),
        draft_override_used = isTRUE(has_draft_override),
        draft_variant = draft_variant,
        summary_variant = summary_variant,
        abstain_reason_variant = abstain_reason_variant,
        label_variants = unname(cascade$public_variants),
        final_schema_path = NULL,
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
