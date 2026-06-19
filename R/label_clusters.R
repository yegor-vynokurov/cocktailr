#' Run an end-to-end cluster labeling workflow for one or more Cocktail clusters
#'
#' @description
#' This is a high-level orchestration helper around
#' \code{\link{cluster_evidence}}, \code{\link{llm_label_cluster}},
#' \code{\link{validate_cluster_label}}, and
#' \code{\link{render_cluster_review}}.
#'
#' For each selected cluster, the function:
#' \enumerate{
#'   \item builds a deterministic evidence bundle,
#'   \item requests one structured label / abstention from the local LLM,
#'   \item validates the structured output against the evidence,
#'   \item if validation fails, performs one validator-guided repair pass,
#'   \item saves a markdown review card to disk.
#' }
#'
#' If no \code{clusters} are supplied, the function selects up to the first
#' \code{top_n} clusters ranked by the Cocktail selection score from
#' \code{\link{select_clusters}(return = "table")}.
#'
#' The workflow is intentionally bounded: at most two top-level iterations are
#' attempted per cluster. If the first attempt fails with an EOF-like structured
#' output error, the second attempt automatically doubles \code{num_predict}. If
#' no valid structured result is available after the bounded retry budget, the
#' function writes a compact placeholder review card that records the cluster,
#' model, prompt provenance, and the failure reason.
#'
#' @param x A \code{"cocktail"} object produced by
#'   \code{\link{cocktail_cluster}}.
#' @param clusters Optional cluster identifiers to process. Accepts numeric IDs
#'   such as \code{c(12, 27)} or labels such as \code{c("c_12", "c_27")}. If
#'   \code{NULL} (default), clusters are selected automatically from the top of
#'   the score-ranked selection table.
#' @param top_n Integer. Used only when \code{clusters = NULL}. Default
#'   \code{10}.
#' @param select_min_phi,select_min_k,select_min_score,select_mode Cluster
#'   selection controls forwarded to \code{\link{select_clusters}} when
#'   \code{clusters = NULL}.
#' @param top_n_phi,n_prototype_plots,n_borderline_plots,include_cover Evidence
#'   extraction controls forwarded to \code{\link{cluster_evidence}}.
#' @param provider,model,variant,base_url,schema_path,temperature,top_p,seed,
#'   num_predict,keep_alive,timeout_sec,max_retries,workflow_steps,log_dir,
#'   request_fn LLM controls forwarded to \code{\link{llm_label_cluster}}.
#' @param max_iterations Integer workflow budget. Currently allowed values are
#'   \code{1} and \code{2}. Default \code{2}.
#' @param review_dir Directory where markdown review cards are written. Default
#'   \code{file.path("temp", "reports", "cluster_reviews")}. When a relative
#'   path is used and a local \code{cocktailr} source checkout can be detected,
#'   it is resolved against that package root.
#' @param verbose Logical. If \code{TRUE} (default), print short progress
#'   messages for cluster-level workflow stages, retries, and saved review
#'   artifacts.
#' @param full Logical. Forwarded to \code{\link{render_cluster_review}}.
#' @param include_front_matter,write_metadata Forwarded to
#'   \code{\link{render_cluster_review}}.
#'
#' @return A list of class \code{"cluster_label_batch_result"} with:
#' \itemize{
#'   \item \code{summary}: one-row-per-cluster workflow summary
#'   \item \code{results}: named list with evidence, llm result, validation, and
#'     review artifact for each cluster
#'   \item \code{selection}: the resolved cluster table used for the run
#' }
#'
#' @examples
#' syn <- generate_synthetic_vegetation_data(
#'   n_plots_per_community = 4,
#'   n_transition_plots = 2,
#'   seed = 42
#' )
#'
#' res <- cocktail_cluster(
#'   vegmatrix = syn$wide_matrix,
#'   progress = FALSE,
#'   plot_values = "rel_cover",
#'   species_cluster_phi = TRUE,
#'   save_vegmatrix = TRUE
#' )
#'
#' req <- label_clusters(
#'   x = res,
#'   clusters = "c_1",
#'   model = "gemma4:12b",
#'   variant = "strict_abstention_gate_v1",
#'   review_dir = file.path("temp", "reports", "cluster_reviews")
#' )
#'
#' req$summary
#'
#' @export
label_clusters <- function(
    x,
    clusters = NULL,
    top_n = 10L,
    select_min_phi = 0.2,
    select_min_k = 1L,
    select_min_score = 1,
    select_mode = c("strict", "top"),
    top_n_phi = 10L,
    n_prototype_plots = 5L,
    n_borderline_plots = 5L,
    include_cover = TRUE,
    provider = "ollama",
    model,
    variant = "strict_abstention_gate_v1",
    base_url = getOption("cocktailr.ollama_base_url", "http://localhost:11434"),
    schema_path = NULL,
    temperature = NULL,
    top_p = NULL,
    seed = NULL,
    num_predict = NULL,
    keep_alive = NULL,
    timeout_sec = 600,
    max_retries = 1L,
    workflow_steps = 1L,
    max_iterations = 2L,
    review_dir = file.path("temp", "reports", "cluster_reviews"),
    verbose = TRUE,
    full = FALSE,
    include_front_matter = full,
    write_metadata = full,
    log_dir = NULL,
    request_fn = NULL
) {
  provider <- .arg_scalar_character(provider, "provider")
  model <- .arg_scalar_character(model, "model")
  variant <- .arg_scalar_character(variant, "variant")
  workflow_steps <- .arg_workflow_steps(workflow_steps, "workflow_steps")
  max_retries <- .arg_non_negative_integer(max_retries, "max_retries")
  max_iterations <- .arg_cluster_label_max_iterations(max_iterations)
  verbose <- .arg_single_flag(verbose, "verbose")
  select_mode <- match.arg(select_mode)

  selection <- .resolve_label_clusters_selection(
    x = x,
    clusters = clusters,
    top_n = top_n,
    min_phi = select_min_phi,
    min_k = select_min_k,
    min_score = select_min_score,
    mode = select_mode
  )

  .label_clusters_log(
    verbose,
    if (is.null(clusters)) {
      paste0(
        "Auto-selected ",
        nrow(selection),
        " cluster(s) for labeling."
      )
    } else {
      paste0(
        "Processing ",
        nrow(selection),
        " requested cluster(s)."
      )
    }
  )

  results <- vector("list", length = nrow(selection))
  names(results) <- selection$cluster
  summary_rows <- vector("list", length = nrow(selection))

  for (i in seq_len(nrow(selection))) {
    cluster_id <- selection$cluster[[i]]
    cluster_tag <- .label_clusters_cluster_tag(
      cluster_id = cluster_id,
      cluster_index = i,
      total_clusters = nrow(selection)
    )

    .label_clusters_log(verbose, cluster_tag, ": building evidence.")

    evidence <- cluster_evidence(
      x = x,
      cluster = cluster_id,
      top_n_phi = top_n_phi,
      n_prototype_plots = n_prototype_plots,
      n_borderline_plots = n_borderline_plots,
      include_cover = include_cover
    )

    template <- llm_label_cluster(
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
      workflow_steps = workflow_steps,
      dry_run = TRUE,
      request_fn = request_fn
    )

    cluster_run <- .run_label_cluster_for_one_evidence(
      evidence = evidence,
      template = template,
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
      workflow_steps = workflow_steps,
      max_iterations = max_iterations,
      review_dir = review_dir,
      verbose = verbose,
      cluster_tag = cluster_tag,
      full = full,
      include_front_matter = include_front_matter,
      write_metadata = write_metadata,
      log_dir = log_dir,
      request_fn = request_fn
    )

    results[[i]] <- cluster_run
    summary_rows[[i]] <- data.frame(
      cluster = cluster_id,
      h = selection$h[[i]],
      k = selection$k[[i]],
      m = selection$m[[i]],
      score = selection$score[[i]],
      run_status = cluster_run$run_status,
      output_status = cluster_run$validation$output_status %||% NA_character_,
      validation_status = cluster_run$validation$validation_status %||% NA_character_,
      needs_human_review = isTRUE(cluster_run$validation$needs_human_review),
      used_placeholder = isTRUE(cluster_run$used_placeholder),
      repair_used = isTRUE(cluster_run$repair_used),
      iterations_used = as.integer(cluster_run$iterations_used %||% NA_integer_),
      num_predict_used = as.integer(cluster_run$num_predict_used %||% NA_integer_),
      review_file = cluster_run$review$file %||% NA_character_,
      failure_reason = cluster_run$failure_reason %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }

  out <- list(
    summary = do.call(rbind, summary_rows),
    results = results,
    selection = selection
  )
  class(out) <- c("cluster_label_batch_result", "list")
  out
}

.arg_cluster_label_max_iterations <- function(x) {
  x <- .arg_non_negative_integer(x, "max_iterations")
  if (!x %in% c(1L, 2L)) {
    stop("`max_iterations` must be either 1 or 2.")
  }
  x
}

.label_clusters_log <- function(verbose, ...) {
  if (!isTRUE(verbose)) {
    return(invisible(NULL))
  }
  message("[label_clusters] ", paste0(..., collapse = ""))
  invisible(NULL)
}

.label_clusters_cluster_tag <- function(cluster_id, cluster_index, total_clusters) {
  paste0(
    "Cluster ",
    cluster_id,
    " (",
    cluster_index,
    "/",
    total_clusters,
    ")"
  )
}

.resolve_label_clusters_selection <- function(
    x,
    clusters,
    top_n,
    min_phi,
    min_k,
    min_score,
    mode
) {
  top_n <- as.integer(top_n)
  if (!is.finite(top_n) || top_n < 1L) {
    stop("`top_n` must be a single integer >= 1.")
  }

  if (is.null(clusters)) {
    sel <- select_clusters(
      x = x,
      min_phi = min_phi,
      min_k = min_k,
      min_score = min_score,
      mode = mode,
      return = "table"
    )
    sel <- utils::head(sel, top_n)
    rownames(sel) <- NULL
    return(sel)
  }

  cluster_labels <- .normalize_cluster_labels(
    clusters = clusters,
    n_nodes = nrow(x$Cluster.species)
  )
  score_table <- .cluster_score_table(x, cluster_labels)
  score_table[match(cluster_labels, score_table$cluster), , drop = FALSE]
}

.normalize_cluster_labels <- function(clusters, n_nodes) {
  if (is.list(clusters)) {
    clusters <- unlist(clusters, use.names = FALSE)
  }

  if (is.character(clusters)) {
    ids <- as.integer(sub("^c_", "", clusters))
  } else if (is.numeric(clusters) || is.integer(clusters)) {
    ids <- as.integer(clusters)
  } else {
    stop("`clusters` must be NULL, numeric IDs, or character labels like 'c_12'.")
  }

  keep <- is.finite(ids) & !is.na(ids) & ids >= 1L & ids <= n_nodes
  ids <- ids[keep]
  if (!length(ids)) {
    stop("No valid cluster IDs remain after filtering to 1..", n_nodes, ".")
  }

  paste0("c_", unique(ids))
}

.cluster_score_table <- function(x, cluster_labels) {
  ids <- as.integer(sub("^c_", "", cluster_labels))
  h <- as.numeric(x$Cluster.height[ids])
  k <- as.numeric(x$Cluster.info[ids, "k"])
  m <- as.numeric(x$Cluster.info[ids, "m"])
  score <- h * log(k) * log(m)

  data.frame(
    cluster = paste0("c_", ids),
    h = h,
    k = k,
    m = m,
    score = score,
    stringsAsFactors = FALSE
  )
}

.run_label_cluster_for_one_evidence <- function(
    evidence,
    template,
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
    workflow_steps,
    max_iterations,
    review_dir,
    verbose,
    cluster_tag,
    full,
    include_front_matter,
    write_metadata,
    log_dir,
    request_fn
) {
  request_fn <- request_fn %||% .ollama_chat_request
  effective_num_predict <- .cluster_label_effective_num_predict(
    template = template,
    workflow_steps = workflow_steps
  )
  if (!is.null(num_predict)) {
    effective_num_predict <- as.integer(num_predict)
  }

  llm_result <- NULL
  validation <- NULL
  failure_reason <- NULL
  repair_used <- FALSE
  used_placeholder <- FALSE
  iteration_log <- vector("list", max_iterations)
  next_mode <- "initial"
  next_num_predict <- effective_num_predict

  # This outer loop is intentionally bounded. We allow at most one follow-up
  # iteration, either as a validator-guided repair pass or as a fresh retry
  # after transport / truncation failure.
  for (iter in seq_len(max_iterations)) {
    iter_mode <- next_mode

    .label_clusters_log(
      verbose,
      cluster_tag,
      if (identical(iter_mode, "validator_repair")) {
        paste0(
          ": validator requested repair; LLM started ",
          "(iteration ",
          iter,
          "/",
          max_iterations,
          ", num_predict=",
          next_num_predict,
          ")."
        )
      } else if (identical(iter_mode, "retry")) {
        paste0(
          ": retrying LLM ",
          "(iteration ",
          iter,
          "/",
          max_iterations,
          ", num_predict=",
          next_num_predict,
          ")."
        )
      } else {
        paste0(
          ": LLM started ",
          "(iteration ",
          iter,
          "/",
          max_iterations,
          ", num_predict=",
          next_num_predict,
          ")."
        )
      }
    )

    attempt <- tryCatch(
      {
        if (identical(iter_mode, "validator_repair") &&
            !is.null(llm_result) &&
            !is.null(validation) &&
            !isTRUE(validation$is_valid)) {
          repair_used <- TRUE
          .repair_cluster_label_result(
            evidence = evidence,
            previous_result = llm_result,
            validation = validation,
            template = template,
            provider = provider,
            model = model,
            variant = variant,
            base_url = base_url,
            keep_alive = keep_alive,
            timeout_sec = timeout_sec,
            max_retries = max_retries,
            num_predict = next_num_predict,
            log_dir = log_dir,
            request_fn = request_fn,
            workflow_steps = workflow_steps
          )
        } else {
          llm_label_cluster(
            evidence = evidence,
            provider = provider,
            model = model,
            variant = variant,
            base_url = base_url,
            schema_path = schema_path,
            temperature = temperature,
            top_p = top_p,
            seed = seed,
            num_predict = next_num_predict,
            keep_alive = keep_alive,
            timeout_sec = timeout_sec,
            max_retries = max_retries,
            workflow_steps = workflow_steps,
            dry_run = FALSE,
            log_dir = log_dir,
            request_fn = request_fn
          )
        }
      },
      error = function(e) e
    )

    if (inherits(attempt, "error")) {
      failure_reason <- conditionMessage(attempt)
      iteration_log[[iter]] <- list(
        iteration = iter,
        mode = iter_mode,
        result = "error",
        num_predict = next_num_predict,
        failure_reason = failure_reason
      )

      if (iter < max_iterations) {
        next_mode <- "retry"
        if (.is_cluster_label_eof_error(failure_reason)) {
          previous_num_predict <- next_num_predict
          next_num_predict <- .double_num_predict(next_num_predict, effective_num_predict)
          .label_clusters_log(
            verbose,
            cluster_tag,
            ": structured output was incomplete or hit EOF; increased num_predict from ",
            previous_num_predict,
            " to ",
            next_num_predict,
            ". Cluster will continue."
          )
        } else {
          .label_clusters_log(
            verbose,
            cluster_tag,
            ": LLM call failed; cluster will continue to the next iteration. Reason: ",
            failure_reason
          )
        }
      } else {
        .label_clusters_log(
          verbose,
          cluster_tag,
          ": LLM call failed and retry budget is exhausted. Reason: ",
          failure_reason
        )
      }
      next
    }

    llm_result <- attempt
    validation <- validate_cluster_label(llm_result, evidence)
    iteration_log[[iter]] <- list(
      iteration = iter,
      mode = iter_mode,
      result = "ok",
      num_predict = next_num_predict,
      validation_status = validation$validation_status,
      is_valid = validation$is_valid
    )

    if (isTRUE(validation$is_valid)) {
      review <- render_cluster_review(
        x = llm_result,
        evidence = evidence,
        validation = validation,
        review_dir = review_dir,
        full = full,
        include_front_matter = include_front_matter,
        write_metadata = write_metadata
      )

      .label_clusters_log(
        verbose,
        cluster_tag,
        ": results saved to ",
        review$file %||% "<unsaved>"
      )

      return(list(
        evidence = evidence,
        llm_result = llm_result,
        validation = validation,
        review = review,
        run_status = "success",
        used_placeholder = FALSE,
        repair_used = repair_used,
        iterations_used = iter,
        num_predict_used = next_num_predict,
        failure_reason = NULL,
        iteration_log = iteration_log[seq_len(iter)]
      ))
    }

    failure_reason <- .validation_failure_summary(validation)
    if (iter < max_iterations) {
      .label_clusters_log(
        verbose,
        cluster_tag,
        ": validator rejected the output (",
        validation$validation_status %||% "unknown",
        "); cluster will continue with a repair pass."
      )
      next_mode <- "validator_repair"
    } else {
      .label_clusters_log(
        verbose,
        cluster_tag,
        ": validator rejected the output and retry budget is exhausted."
      )
    }
  }

  used_placeholder <- TRUE
  # Never fail silently at the batch level: if we cannot obtain a valid
  # structured result, we still emit a minimal abstaining artifact so the
  # review workflow remains auditable.
  placeholder_result <- .cluster_label_placeholder_result(
    evidence = evidence,
    template = .cluster_label_placeholder_template(llm_result, template),
    provider = provider,
    model = model,
    variant = variant,
    workflow_steps = workflow_steps,
    failure_reason = failure_reason
  )
  validation <- validate_cluster_label(placeholder_result, evidence)
  review <- render_cluster_review(
    x = placeholder_result,
    evidence = evidence,
    validation = validation,
    review_dir = review_dir,
    full = full,
    include_front_matter = include_front_matter,
    write_metadata = write_metadata
  )

  .label_clusters_log(
    verbose,
    cluster_tag,
    ": no valid structured result was obtained; placeholder review saved to ",
    review$file %||% "<unsaved>"
  )

  list(
    evidence = evidence,
    llm_result = placeholder_result,
    validation = validation,
    review = review,
    run_status = "placeholder",
    used_placeholder = used_placeholder,
    repair_used = repair_used,
    iterations_used = max_iterations,
    num_predict_used = next_num_predict,
    failure_reason = failure_reason,
    iteration_log = iteration_log[seq_len(max_iterations)]
  )
}

.cluster_label_effective_num_predict <- function(template, workflow_steps) {
  request <- if (identical(workflow_steps, 2L)) {
    template$workflow$label$request
  } else {
    template$request
  }

  num_predict <- request$options$num_predict %||% NULL
  num_predict <- suppressWarnings(as.integer(num_predict))
  if (!is.finite(num_predict) || is.na(num_predict) || num_predict < 1L) {
    return(1200L)
  }
  num_predict
}

.double_num_predict <- function(current, fallback) {
  current <- suppressWarnings(as.integer(current))
  fallback <- suppressWarnings(as.integer(fallback))

  if (!is.finite(current) || is.na(current) || current < 1L) {
    current <- if (is.finite(fallback) && !is.na(fallback) && fallback >= 1L) {
      fallback
    } else {
      1200L
    }
  }

  as.integer(current * 2L)
}

.is_cluster_label_eof_error <- function(message) {
  if (is.null(message) || !length(message)) {
    return(FALSE)
  }

  grepl(
    "premature\\s+EOF|unexpected\\s+end|EOF",
    message,
    ignore.case = TRUE,
    perl = TRUE
  )
}

.validation_failure_summary <- function(validation) {
  issues <- validation$issues %||% .new_cluster_label_issue_table()
  if (!nrow(issues)) {
    return(
      paste0(
        "Validation failed with status `",
        validation$validation_status %||% "unknown",
        "`."
      )
    )
  }

  issue_lines <- apply(issues, 1L, function(row) {
    location <- if (!is.na(row[["location"]]) && nzchar(row[["location"]])) {
      paste0(" @ ", row[["location"]])
    } else {
      ""
    }
    paste0("[", row[["code"]], "] ", row[["message"]], location)
  })

  paste(
    c(
      paste0("Validation failed with status `", validation$validation_status, "`."),
      issue_lines
    ),
    collapse = " "
  )
}

.cluster_label_validation_repair_message <- function(validation, previous_output) {
  issues <- validation$issues %||% .new_cluster_label_issue_table()
  issue_lines <- if (!nrow(issues)) {
    "- No structured validator issue table was recorded, but the previous output must still be corrected."
  } else {
    vapply(seq_len(nrow(issues)), function(i) {
      row <- issues[i, , drop = FALSE]
      location <- row$location[[1]]
      location_text <- if (!is.na(location) && nzchar(location)) {
        paste0(" Location: ", location, ".")
      } else {
        ""
      }
      paste0(
        "- [", row$severity[[1]], "][", row$category[[1]], "][", row$code[[1]], "] ",
        row$message[[1]],
        location_text
      )
    }, character(1))
  }

  previous_json <- jsonlite::toJSON(
    previous_output,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )

  paste(
    c(
      "The previous JSON output parsed, but it failed downstream validation.",
      "Return one complete corrected JSON object only.",
      "Keep fields that are already valid unless they depend on a corrected field.",
      "Fix only the problematic parts listed below, but return the full JSON object.",
      "Do not add markdown, commentary, or code fences.",
      "Do not invent new evidence IDs, species, habitats, or external facts.",
      "If the evidence is insufficient for a safe label, switch to `status = \"abstain\"` and make all dependent fields consistent.",
      "",
      "Validator issues:",
      issue_lines,
      "",
      "Previous JSON output:",
      previous_json
    ),
    collapse = "\n"
  )
}

.repair_cluster_label_result <- function(
    evidence,
    previous_result,
    validation,
    template,
    provider,
    model,
    variant,
    base_url,
    keep_alive,
    timeout_sec,
    max_retries,
    num_predict,
    log_dir,
    request_fn,
    workflow_steps
) {
  prompt_bundle <- if (identical(workflow_steps, 2L)) {
    template$workflow$label$prompt
  } else {
    template$prompt
  }

  prompt_bundle$generation$num_predict <- as.integer(num_predict)
  prompt_bundle$messages <- c(
    prompt_bundle$messages,
    list(
      list(
        role = "assistant",
        content = jsonlite::toJSON(
          previous_result$output,
          auto_unbox = TRUE,
          null = "null",
          pretty = TRUE
        )
      ),
      list(
        role = "user",
        content = .cluster_label_validation_repair_message(
          validation = validation,
          previous_output = previous_result$output
        )
      )
    )
  )

  endpoint <- paste0(sub("/+$", "", base_url), "/api/chat")
  log_paths <- .init_cluster_label_logs(
    log_dir = log_dir,
    cluster_id = evidence$meta$cluster_id,
    model = model,
    variant = paste0(variant, "_validator_repair")
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
    stage_name = "validator_repair"
  )

  out$workflow_steps <- workflow_steps
  out$workflow <- previous_result$workflow %||% NULL
  out$attempts <- as.integer((previous_result$attempts %||% 0L) + (out$attempts %||% 0L))
  out$repair <- list(
    source = "validator",
    previous_validation_status = validation$validation_status,
    issue_count = nrow(validation$issues %||% .new_cluster_label_issue_table())
  )
  class(out) <- c("cluster_label_result", "list")
  out
}

.cluster_label_placeholder_template <- function(llm_result, template) {
  if (inherits(llm_result, "cluster_label_result")) {
    return(llm_result)
  }
  template
}

.cluster_label_placeholder_output <- function(evidence, failure_reason) {
  reason <- .null_default(
    .as_scalar_character(failure_reason),
    "No valid structured LLM output was obtained after the bounded retry budget."
  )

  list(
    schema_version = "0.1.0",
    cluster_id = evidence$meta$cluster_id,
    status = "abstain",
    canonical_label = NULL,
    display_label = NULL,
    interpretation_summary = paste(
      "No data: no valid structured cluster label could be obtained",
      "after the bounded retry budget."
    ),
    basis_in_data = list(),
    key_species = list(),
    external_knowledge = list(),
    not_confirmed_by_data = list(
      list(
        statement = "No final cluster label is available.",
        reason = reason
      )
    ),
    confidence = list(
      score = 0,
      rationale = "No usable structured LLM output was available for this cluster."
    ),
    checks_to_run = list(
      list(
        check = "Retry the labeling workflow.",
        priority = "high",
        reason = reason
      )
    ),
    abstain_reason = "No data: no valid structured cluster label could be obtained."
  )
}

.cluster_label_placeholder_result <- function(
    evidence,
    template,
    provider,
    model,
    variant,
    workflow_steps,
    failure_reason
) {
  output <- .cluster_label_placeholder_output(
    evidence = evidence,
    failure_reason = failure_reason
  )

  prompt <- if (inherits(template, "cluster_label_result")) {
    template$prompt
  } else if (identical(workflow_steps, 2L)) {
    template$workflow$label$prompt
  } else {
    template$prompt
  }

  request <- if (inherits(template, "cluster_label_result")) {
    template$request
  } else if (identical(workflow_steps, 2L)) {
    template$workflow$label$request
  } else {
    template$request
  }

  workflow <- if (inherits(template, "cluster_label_result")) {
    template$workflow %||% NULL
  } else {
    template$workflow %||% NULL
  }

  logs <- if (inherits(template, "cluster_label_result")) {
    template$logs %||% list(run_dir = NULL)
  } else {
    list(run_dir = NULL)
  }

  out <- list(
    cluster_id = evidence$meta$cluster_id,
    provider = provider,
    model = model,
    variant = variant,
    prompt = prompt,
    request = request,
    response = NULL,
    output = output,
    attempts = 0L,
    workflow_steps = workflow_steps,
    schema_path = prompt$schema_path %||% NULL,
    logs = logs,
    workflow = workflow,
    failure_reason = failure_reason
  )
  class(out) <- c("cluster_label_result", "list")
  out
}
