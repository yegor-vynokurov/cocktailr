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
#' The workflow is intentionally bounded: at most three top-level iterations are
#' attempted per cluster, while still allowing at most one validator-guided
#' repair pass. If an iteration fails with an EOF-like structured output error,
#' the next iteration automatically increases \code{num_predict} along the
#' default retry ladder \code{2400 -> 4800 -> 9600}. If no valid structured
#' result is available after the bounded retry budget, the function writes a
#' compact placeholder review card that records the cluster, model, prompt
#' provenance, and the failure reason.
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
#' @param semantic_layer Logical. If \code{TRUE}, try to enrich each cluster's
#'   evidence bundle with indicator-derived ecological axes from the optional
#'   semantic layer built from files under \code{data-raw/external/}. Failures
#'   in this auxiliary step are recorded in the batch summary and the workflow
#'   continues with the plain evidence bundle. Default \code{FALSE}.
#' @param semantic_min_phi Optional numeric scalar forwarded to
#'   \code{score_cluster_semantics()}.
#' @param semantic_bootstrap Integer bootstrap count for semantic axis
#'   summaries. Default \code{200}.
#' @param semantic_root Optional project root override for the semantic layer.
#'   By default the semantic helpers use \code{cocktailr_project_root()}.
#' @param semantic_force_species,semantic_force_reference Logical refresh flags
#'   forwarded to \code{score_cluster_semantics()}.
#' @param provider,model,variant,base_url,schema_path,temperature,top_p,seed,
#'   num_predict,prompt_budget_chars,keep_alive,ollama_options,timeout_sec,max_retries,
#'   workflow_steps,label_mode,log_dir,request_fn LLM controls forwarded to
#'   \code{\link{llm_label_cluster}}. In particular, \code{workflow_steps = 3}
#'   enables the staged draft-analysis -> label-selection -> explanation mode,
#'   and \code{label_mode} selects between open, constrained, and dynamic
#'   label-space behavior.
#' @param max_iterations Integer workflow budget. Currently allowed values are
#'   \code{1}, \code{2}, and \code{3}. Default \code{3}. The default supports
#'   the full EOF retry ladder \code{2400 -> 4800 -> 9600}.
#' @param review_dir Directory where markdown review cards are written. Default
#'   \code{file.path("temp", "reports", "cluster_reviews")}. When a relative
#'   path is used and a local \code{cocktailr} source checkout can be detected,
#'   it is resolved against that package root.
#' @param verbose Logical. If \code{TRUE} (default), print short progress
#'   messages for cluster-level workflow stages, retries, and saved review
#'   artifacts.
#' @param labels_for_imgs Logical. If \code{TRUE}, build a plotting-oriented
#'   registry table with \code{\link{cluster_label_registry}} after all review
#'   cards are generated, save it automatically as
#'   \code{"cluster_label_registry.csv"} next to the review cards, and return
#'   it as \code{$label_registry}. Default \code{FALSE}.
#' @param speculative_fallback_mode Character scalar controlling whether a
#'   softer speculative fallback ladder may run after the strict workflow fails
#'   to produce an accepted structured label. Default \code{"off"}. Supported
#'   non-default values are \code{"after_rejection"} for the narrow placeholder
#'   branch only, and \code{"after_nonaccepted"} to also continue after a valid
#'   strict abstention.
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
#'   \item \code{label_registry}: optional flat registry table for downstream
#'     plotting, present when \code{labels_for_imgs = TRUE}
#'   \item \code{label_registry_file}: optional path to the saved registry CSV,
#'     present when \code{labels_for_imgs = TRUE}
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
#'   variant = "label_primary_v1",
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
    semantic_layer = FALSE,
    semantic_min_phi = NULL,
    semantic_bootstrap = 200L,
    semantic_root = NULL,
    semantic_force_species = FALSE,
    semantic_force_reference = FALSE,
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
    timeout_sec = 600,
    max_retries = 1L,
    workflow_steps = 1L,
    max_iterations = 3L,
    review_dir = file.path("temp", "reports", "cluster_reviews"),
    verbose = TRUE,
    labels_for_imgs = FALSE,
    speculative_fallback_mode = c("off", "after_rejection", "after_nonaccepted"),
    full = FALSE,
    include_front_matter = full,
    write_metadata = full,
    log_dir = NULL,
    request_fn = NULL
) {
  provider <- .arg_scalar_character(provider, "provider")
  model <- .arg_scalar_character(model, "model")
  variant <- .arg_scalar_character(variant, "variant")
  label_mode <- .arg_cluster_label_mode(label_mode, "label_mode")
  workflow_steps <- .arg_workflow_steps(workflow_steps, "workflow_steps")
  max_retries <- .arg_non_negative_integer(max_retries, "max_retries")
  prompt_budget_chars <- .arg_nullable_positive_integer(
    prompt_budget_chars,
    "prompt_budget_chars"
  )
  max_iterations <- .arg_cluster_label_max_iterations(max_iterations)
  ollama_options <- .arg_named_list_or_null(ollama_options, "ollama_options")
  verbose <- .arg_single_flag(verbose, "verbose")
  labels_for_imgs <- .arg_single_flag(labels_for_imgs, "labels_for_imgs")
  semantic_layer <- .arg_single_flag(semantic_layer, "semantic_layer")
  semantic_bootstrap <- .arg_positive_integer(
    semantic_bootstrap,
    "semantic_bootstrap"
  )
  semantic_force_species <- .arg_single_flag(
    semantic_force_species,
    "semantic_force_species"
  )
  semantic_force_reference <- .arg_single_flag(
    semantic_force_reference,
    "semantic_force_reference"
  )

  if (identical(label_mode, "dynamic") && !identical(workflow_steps, 3L)) {
    stop("`label_mode = \"dynamic\"` currently requires `workflow_steps = 3`.")
  }
  speculative_fallback_mode <- match.arg(speculative_fallback_mode)
  select_mode <- match.arg(select_mode)

  if (!is.null(semantic_min_phi)) {
    semantic_min_phi <- suppressWarnings(as.numeric(semantic_min_phi))
    if (length(semantic_min_phi) != 1L || is.na(semantic_min_phi)) {
      stop("`semantic_min_phi` must be NULL or a single numeric value.")
    }
  }

  if (!is.null(semantic_root)) {
    semantic_root <- .arg_scalar_character(semantic_root, "semantic_root")
  }

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

    semantic_step <- .label_clusters_apply_semantic_layer(
      evidence = evidence,
      x = x,
      semantic_layer = semantic_layer,
      semantic_min_phi = semantic_min_phi,
      semantic_bootstrap = semantic_bootstrap,
      semantic_root = semantic_root,
      semantic_force_species = semantic_force_species,
      semantic_force_reference = semantic_force_reference,
      verbose = verbose,
      cluster_tag = cluster_tag
    )
    evidence <- semantic_step$evidence

    template <- llm_label_cluster(
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
      dry_run = TRUE,
      request_fn = request_fn
    )

    cluster_run <- .run_label_cluster_for_one_evidence(
      evidence = evidence,
      cluster_score = selection$score[[i]],
      template = template,
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
      max_iterations = max_iterations,
      review_dir = review_dir,
      verbose = verbose,
      cluster_tag = cluster_tag,
      full = full,
      include_front_matter = include_front_matter,
      write_metadata = write_metadata,
      speculative_fallback_mode = speculative_fallback_mode,
      log_dir = log_dir,
      request_fn = request_fn
    )

    cluster_run$semantic_layer_used <- isTRUE(semantic_step$used)
    cluster_run$semantic_layer_status <- semantic_step$status %||% "off"
    cluster_run$semantic_layer_error <- semantic_step$error %||% NA_character_

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
      label_tier = cluster_run$label_tier %||% NA_character_,
      is_speculative = isTRUE(cluster_run$is_speculative),
      speculative_fallback_used = isTRUE(cluster_run$speculative_fallback_used),
      strict_outcome = cluster_run$strict_outcome %||% NA_character_,
      strict_validation_status = cluster_run$strict_validation_status %||% NA_character_,
      semantic_layer_used = isTRUE(cluster_run$semantic_layer_used),
      semantic_layer_status = cluster_run$semantic_layer_status %||% NA_character_,
      semantic_layer_error = cluster_run$semantic_layer_error %||% NA_character_,
      label_origin = cluster_run$label_origin %||% NA_character_,
      species_entropy_band = cluster_run$species_entropy_band %||% NA_character_,
      species_entropy_text = cluster_run$species_entropy_text %||% NA_character_,
      chaoticity_score = as.integer(cluster_run$chaoticity_score %||% NA_integer_),
      chaoticity_label = cluster_run$chaoticity_label %||% NA_character_,
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
    selection = selection,
    label_registry = NULL,
    label_registry_file = NULL
  )
  class(out) <- c("cluster_label_batch_result", "list")

  if (isTRUE(labels_for_imgs)) {
    out$label_registry <- cluster_label_registry(out)
    out$label_registry_file <- tryCatch(
      {
        .write_cluster_label_registry_file(
          label_registry = out$label_registry,
          results = out$results,
          review_dir = review_dir
        )
      },
      error = function(e) {
        warning(
          "Could not save `cluster_label_registry.csv`: ",
          conditionMessage(e),
          call. = FALSE
        )
        NULL
      }
    )
    out$label_registry <- .attach_cluster_label_registry_file(
      out$label_registry,
      out$label_registry_file
    )
    .label_clusters_log(
      verbose,
      "Built label registry for downstream plotting with ",
      nrow(out$label_registry),
      " row(s)."
    )
    if (!is.null(out$label_registry_file)) {
      .label_clusters_log(
        verbose,
        "Saved label registry to ",
        out$label_registry_file
      )
    } else {
      .label_clusters_log(
        verbose,
        "Label registry could not be saved automatically; plotting can still use `run$label_registry` explicitly."
      )
    }
  }

  out
}

.arg_cluster_label_max_iterations <- function(x) {
  x <- .arg_non_negative_integer(x, "max_iterations")
  if (!x %in% c(1L, 2L, 3L)) {
    stop("`max_iterations` must be one of 1, 2, or 3.")
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

.label_clusters_apply_semantic_layer <- function(
    evidence,
    x,
    semantic_layer,
    semantic_min_phi,
    semantic_bootstrap,
    semantic_root,
    semantic_force_species,
    semantic_force_reference,
    verbose,
    cluster_tag
) {
  if (!isTRUE(semantic_layer)) {
    return(list(
      evidence = evidence,
      used = FALSE,
      status = "off",
      error = NA_character_
    ))
  }

  .label_clusters_log(verbose, cluster_tag, ": semantic enrichment started.")

  attempt <- tryCatch(
    {
      root <- semantic_root
      if (is.null(root)) {
        root <- cocktailr_project_root()
      }

      semantic_result <- score_cluster_semantics(
        x = x,
        clusters = evidence$meta$cluster_id,
        min_phi = semantic_min_phi,
        bootstrap = semantic_bootstrap,
        root = root,
        force_species = semantic_force_species,
        force_reference = semantic_force_reference
      )

      enriched <- .augment_cluster_evidence_with_semantic_layer(
        evidence = evidence,
        semantic_result = semantic_result
      )

      list(
        evidence = enriched,
        used = TRUE,
        status = "enriched",
        error = NA_character_
      )
    },
    error = function(e) {
      list(
        evidence = evidence,
        used = FALSE,
        status = "failed",
        error = conditionMessage(e)
      )
    }
  )

  if (identical(attempt$status, "enriched")) {
    .label_clusters_log(verbose, cluster_tag, ": semantic enrichment completed.")
  } else if (identical(attempt$status, "failed")) {
    .label_clusters_log(
      verbose,
      cluster_tag,
      ": semantic enrichment failed; continuing without it. ",
      attempt$error
    )
  }

  attempt
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
    cluster_score,
    template,
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
    max_iterations,
    review_dir,
    verbose,
    cluster_tag,
    full,
    include_front_matter,
    write_metadata,
    speculative_fallback_mode,
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
  strict_terminal_state <- NULL
  strict_terminal_iteration <- NULL

  # This outer loop is intentionally bounded. We allow up to three top-level
  # iterations, while still keeping validator-guided repair to a single pass.
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
            workflow_steps = workflow_steps,
            ollama_options = ollama_options
          )
        } else {
          llm_label_cluster(
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
            num_predict = next_num_predict,
            prompt_budget_chars = prompt_budget_chars,
            keep_alive = keep_alive,
            ollama_options = ollama_options,
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
          next_num_predict <- .next_cluster_label_num_predict(
            current = next_num_predict,
            fallback = effective_num_predict
          )
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
      if (identical(validation$output_status, "labeled")) {
        validation <- .attach_cluster_label_difficulty_profile(
          validation = validation,
          cluster_score = cluster_score,
          label_origin = "strict_label"
        )

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

        return(.cluster_label_completed_run(
          evidence = evidence,
          llm_result = llm_result,
          validation = validation,
          review = review,
          run_status = "success",
          used_placeholder = FALSE,
          speculative_fallback_used = FALSE,
          strict_outcome = "accepted",
          strict_validation_status = validation$validation_status %||% NA_character_,
          strict_failure_reason = NULL,
          repair_used = repair_used,
          iterations_used = iter,
          num_predict_used = next_num_predict,
          failure_reason = NULL,
          iteration_log = iteration_log[seq_len(iter)]
        ))
      }

      if (identical(validation$output_status, "abstain") &&
          identical(speculative_fallback_mode, "after_nonaccepted")) {
        strict_terminal_state <- "abstained"
        strict_terminal_iteration <- iter
        failure_reason <- .null_default(
          .as_scalar_character(validation$output$abstain_reason),
          "Strict workflow abstained before the speculative fallback ladder."
        )
        .label_clusters_log(
          verbose,
          cluster_tag,
          ": strict workflow abstained; speculative fallback ladder started."
        )
        break
      }

      validation <- .attach_cluster_label_difficulty_profile(
        validation = validation,
        cluster_score = cluster_score,
        label_origin = "strict_abstain"
      )

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

      return(.cluster_label_completed_run(
        evidence = evidence,
        llm_result = llm_result,
        validation = validation,
        review = review,
        run_status = "success",
        used_placeholder = FALSE,
        speculative_fallback_used = FALSE,
        strict_outcome = "abstained",
        strict_validation_status = validation$validation_status %||% NA_character_,
        strict_failure_reason = NULL,
        repair_used = repair_used,
        iterations_used = iter,
        num_predict_used = next_num_predict,
        failure_reason = NULL,
        iteration_log = iteration_log[seq_len(iter)]
      ))
    }

    failure_reason <- .validation_failure_summary(validation)
    if (iter < max_iterations && !isTRUE(repair_used)) {
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
        if (isTRUE(repair_used)) {
          ": validator rejected the repaired output and no additional repair pass is allowed."
        } else {
          ": validator rejected the output and retry budget is exhausted."
        }
      )
    }
  }

  strict_llm_result <- llm_result
  strict_validation <- validation
  strict_failure_reason <- failure_reason
  strict_iteration_count <- strict_terminal_iteration %||% max_iterations
  strict_iteration_log <- iteration_log[seq_len(strict_iteration_count)]
  speculative_iteration_record <- NULL
  speculative_fallback_used <- FALSE
  speculative_needed <- identical(speculative_fallback_mode, "after_rejection") ||
    (identical(speculative_fallback_mode, "after_nonaccepted") &&
      identical(strict_terminal_state, "abstained"))

  if (identical(speculative_fallback_mode, "after_nonaccepted") &&
      !identical(strict_terminal_state, "abstained") &&
      !isTRUE(strict_validation$is_valid)) {
    speculative_needed <- TRUE
  }

  if (isTRUE(speculative_needed)) {
    speculative_fallback_used <- TRUE
    .label_clusters_log(
      verbose,
      cluster_tag,
      if (identical(strict_terminal_state, "abstained")) {
        ": soft-label ladder continues after strict abstention."
      } else {
        ": strict labeling exhausted its retry budget; soft-label ladder started."
      }
    )

    speculative_attempt <- .run_speculative_fallback_cluster_label(
      evidence = evidence,
      provider = provider,
      model = .cluster_label_speculative_model(provider, model),
      base_url = base_url,
      schema_path = schema_path,
      temperature = temperature,
      top_p = top_p,
      seed = seed,
      num_predict = .cluster_label_speculative_num_predict(provider, next_num_predict),
      prompt_budget_chars = prompt_budget_chars,
      keep_alive = keep_alive,
      ollama_options = .cluster_label_speculative_ollama_options(
        provider = provider,
        ollama_options = ollama_options
      ),
      timeout_sec = timeout_sec,
      max_retries = max_retries,
      strict_variant = variant,
      strict_label_mode = label_mode,
      strict_workflow_steps = workflow_steps,
      strict_result = strict_llm_result,
      strict_validation = strict_validation,
      strict_failure_reason = strict_failure_reason,
      cluster_score = cluster_score,
      strict_outcome = if (identical(strict_terminal_state, "abstained")) {
        "abstained"
      } else {
        "placeholder"
      },
      verbose = verbose,
      cluster_tag = cluster_tag,
      log_dir = log_dir,
      request_fn = request_fn
    )

    if (isTRUE(speculative_attempt$success)) {
      review <- render_cluster_review(
        x = speculative_attempt$llm_result,
        evidence = evidence,
        validation = speculative_attempt$validation,
        review_dir = review_dir,
        full = full,
        include_front_matter = include_front_matter,
        write_metadata = write_metadata
      )

      .label_clusters_log(
        verbose,
        cluster_tag,
        ": soft-label ladder produced a tentative label; results saved to ",
        review$file %||% "<unsaved>"
      )

      return(.cluster_label_completed_run(
        evidence = evidence,
        llm_result = speculative_attempt$llm_result,
        validation = speculative_attempt$validation,
        review = review,
        run_status = "speculative",
        used_placeholder = FALSE,
        speculative_fallback_used = speculative_fallback_used,
        strict_outcome = if (identical(strict_terminal_state, "abstained")) {
          "abstained"
        } else {
          "placeholder"
        },
        strict_validation_status = strict_validation$validation_status %||% NA_character_,
        strict_failure_reason = strict_failure_reason,
        repair_used = repair_used,
        iterations_used = strict_iteration_count,
        num_predict_used = speculative_attempt$num_predict_used %||% next_num_predict,
        failure_reason = NULL,
        iteration_log = c(
          strict_iteration_log,
          speculative_attempt$iteration_log %||% list()
        )
      ))
    }

    speculative_iteration_record <- speculative_attempt$iteration_log %||% list()
    failure_reason <- paste(
      c(
        strict_failure_reason,
        paste0(
          "Soft-label ladder failed: ",
          speculative_attempt$failure_reason %||% "unknown reason."
        )
      ),
      collapse = " "
    )

    .label_clusters_log(
      verbose,
      cluster_tag,
      ": soft-label ladder did not produce an acceptable tentative label. Reason: ",
      speculative_attempt$failure_reason %||% "unknown reason."
    )
  }

  if (identical(strict_terminal_state, "abstained") &&
      inherits(strict_llm_result, "cluster_label_result") &&
      inherits(strict_validation, "cluster_label_validation")) {
    strict_validation <- .attach_cluster_label_difficulty_profile(
      validation = strict_validation,
      cluster_score = cluster_score,
      label_origin = "fallback_abstain"
    )
    review <- render_cluster_review(
      x = strict_llm_result,
      evidence = evidence,
      validation = strict_validation,
      review_dir = review_dir,
      full = full,
      include_front_matter = include_front_matter,
      write_metadata = write_metadata
    )

    .label_clusters_log(
      verbose,
      cluster_tag,
      ": no tentative label passed the soft ladder; strict abstention saved to ",
      review$file %||% "<unsaved>"
    )

    return(.cluster_label_completed_run(
      evidence = evidence,
      llm_result = strict_llm_result,
      validation = strict_validation,
      review = review,
      run_status = "success",
      used_placeholder = FALSE,
      speculative_fallback_used = speculative_fallback_used,
      strict_outcome = "abstained",
      strict_validation_status = strict_validation$validation_status %||% NA_character_,
      strict_failure_reason = strict_failure_reason,
      repair_used = repair_used,
      iterations_used = strict_iteration_count,
      num_predict_used = next_num_predict,
      failure_reason = failure_reason,
      iteration_log = c(
        strict_iteration_log,
        speculative_iteration_record
      )
    ))
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
  validation <- .attach_cluster_label_difficulty_profile(
    validation = validation,
    cluster_score = cluster_score,
    label_origin = "placeholder"
  )
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

  .cluster_label_completed_run(
    evidence = evidence,
    llm_result = placeholder_result,
    validation = validation,
    review = review,
    run_status = "placeholder",
    used_placeholder = used_placeholder,
    speculative_fallback_used = speculative_fallback_used,
    strict_outcome = "placeholder",
    strict_validation_status = strict_validation$validation_status %||% NA_character_,
    strict_failure_reason = strict_failure_reason,
    repair_used = repair_used,
    iterations_used = strict_iteration_count,
    num_predict_used = next_num_predict,
    failure_reason = failure_reason,
    iteration_log = c(
      strict_iteration_log,
      speculative_iteration_record
    )
  )
}

.cluster_label_completed_run <- function(
    evidence,
    llm_result,
    validation,
    review,
    run_status,
    used_placeholder,
    speculative_fallback_used,
    strict_outcome,
    strict_validation_status,
    strict_failure_reason,
    repair_used,
    iterations_used,
    num_predict_used,
    failure_reason,
    iteration_log
) {
  list(
    evidence = evidence,
    llm_result = llm_result,
    validation = validation,
    review = review,
    run_status = run_status,
    used_placeholder = isTRUE(used_placeholder),
    label_tier = .cluster_label_validation_label_tier(validation),
    is_speculative = isTRUE(validation$is_speculative),
    speculative_fallback_used = isTRUE(speculative_fallback_used),
    strict_outcome = strict_outcome %||% validation$strict_outcome %||% NA_character_,
    strict_validation_status = strict_validation_status %||%
      validation$strict_validation_status %||% NA_character_,
    strict_failure_reason = strict_failure_reason,
    label_origin = validation$label_origin %||% NA_character_,
    species_entropy_band = validation$species_entropy_band %||% NA_character_,
    species_entropy_text = validation$species_entropy_text %||% NA_character_,
    chaoticity_score = validation$chaoticity_score %||% NA_integer_,
    chaoticity_label = validation$chaoticity_label %||% NA_character_,
    repair_used = isTRUE(repair_used),
    iterations_used = as.integer(iterations_used %||% NA_integer_),
    num_predict_used = as.integer(num_predict_used %||% NA_integer_),
    failure_reason = failure_reason,
    iteration_log = iteration_log
  )
}

.cluster_label_entropy_profile <- function(cluster_score, label_origin) {
  label_origin <- .as_scalar_character(label_origin)
  cluster_score <- suppressWarnings(as.numeric(cluster_score))

  if (identical(label_origin, "strict_label")) {
    if (is.finite(cluster_score) && cluster_score > 50) {
      return(list(
        label_origin = "strict_label",
        species_entropy_band = "minimal",
        species_entropy_text = "minimal entropy of species composition",
        chaoticity_score = 10L,
        chaoticity_label = "low"
      ))
    }

    return(list(
      label_origin = "strict_label",
      species_entropy_band = "moderate",
      species_entropy_text = "moderate entropy of species composition",
      chaoticity_score = 40L,
      chaoticity_label = "moderate"
    ))
  }

  if (identical(label_origin, "speculative_v3")) {
    return(list(
      label_origin = "speculative_v3",
      species_entropy_band = "high",
      species_entropy_text = "high entropy of species composition",
      chaoticity_score = 70L,
      chaoticity_label = "high"
    ))
  }

  if (identical(label_origin, "speculative_v4")) {
    return(list(
      label_origin = "speculative_v4",
      species_entropy_band = "very_high",
      species_entropy_text = "very high entropy of species composition",
      chaoticity_score = 90L,
      chaoticity_label = "very_high"
    ))
  }

  if (identical(label_origin, "fallback_abstain")) {
    return(list(
      label_origin = "fallback_abstain",
      species_entropy_band = "very_high",
      species_entropy_text = "very high entropy of species composition",
      chaoticity_score = 95L,
      chaoticity_label = "very_high"
    ))
  }

  if (identical(label_origin, "placeholder")) {
    return(list(
      label_origin = "placeholder",
      species_entropy_band = "extreme",
      species_entropy_text = "extreme entropy of species composition",
      chaoticity_score = 100L,
      chaoticity_label = "extreme"
    ))
  }

  if (identical(label_origin, "strict_abstain")) {
    return(list(
      label_origin = "strict_abstain",
      species_entropy_band = NA_character_,
      species_entropy_text = NA_character_,
      chaoticity_score = NA_integer_,
      chaoticity_label = NA_character_
    ))
  }

  list(
    label_origin = label_origin %||% NA_character_,
    species_entropy_band = NA_character_,
    species_entropy_text = NA_character_,
    chaoticity_score = NA_integer_,
    chaoticity_label = NA_character_
  )
}

.attach_cluster_label_difficulty_profile <- function(validation, cluster_score, label_origin) {
  if (!inherits(validation, "cluster_label_validation")) {
    return(validation)
  }

  profile <- .cluster_label_entropy_profile(
    cluster_score = cluster_score,
    label_origin = label_origin
  )

  validation$label_origin <- profile$label_origin
  validation$species_entropy_band <- profile$species_entropy_band
  validation$species_entropy_text <- profile$species_entropy_text
  validation$chaoticity_score <- profile$chaoticity_score
  validation$chaoticity_label <- profile$chaoticity_label
  class(validation) <- "cluster_label_validation"
  validation
}

.cluster_label_speculative_model <- function(provider, model) {
  provider <- .as_scalar_character(provider)
  model <- .as_scalar_character(model)

  if (!identical(provider, "ollama")) {
    return(model)
  }

  .default_cluster_label_speculative_model()
}

.cluster_label_speculative_num_predict <- function(provider, fallback_num_predict) {
  provider <- .as_scalar_character(provider)
  fallback_num_predict <- suppressWarnings(as.integer(fallback_num_predict))

  if (!identical(provider, "ollama")) {
    return(fallback_num_predict)
  }

  .default_cluster_label_speculative_num_predict()
}

.cluster_label_speculative_ollama_options <- function(provider, ollama_options) {
  provider <- .as_scalar_character(provider)
  ollama_options <- .arg_named_list_or_null(ollama_options, "ollama_options")

  if (!identical(provider, "ollama")) {
    return(ollama_options)
  }

  .merge_named_lists(
    .default_cluster_label_speculative_ollama_options(),
    ollama_options
  )
}

.default_cluster_label_num_predict <- function() {
  2400L
}

.default_cluster_label_max_num_predict <- function() {
  9600L
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
    return(.default_cluster_label_num_predict())
  }
  num_predict
}

.next_cluster_label_num_predict <- function(current, fallback) {
  current <- suppressWarnings(as.integer(current))
  fallback <- suppressWarnings(as.integer(fallback))
  max_num_predict <- .default_cluster_label_max_num_predict()

  if (!is.finite(current) || is.na(current) || current < 1L) {
    current <- if (is.finite(fallback) && !is.na(fallback) && fallback >= 1L) {
      fallback
    } else {
      .default_cluster_label_num_predict()
    }
  }

  if (current >= max_num_predict) {
    return(as.integer(current))
  }

  as.integer(min(current * 2L, max_num_predict))
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

.cluster_label_format_issue_codes <- function() {
  c(
    "canonical_label_too_long",
    "display_label_too_long",
    "display_label_too_many_words",
    "display_label_forbidden_punctuation",
    "display_label_trailing_period"
  )
}

.cluster_label_has_format_issues <- function(validation) {
  issues <- validation$issues %||% .new_cluster_label_issue_table()
  if (!nrow(issues)) {
    return(FALSE)
  }

  any(issues$code %in% .cluster_label_format_issue_codes())
}

.cluster_label_lightweight_repair_issue_codes <- function() {
  c(
    "invalid_canonical_label_format",
    "missing_canonical_label",
    "canonical_label_too_long",
    "missing_display_label",
    "display_label_too_long",
    "display_label_too_many_words",
    "display_label_forbidden_punctuation",
    "display_label_trailing_period"
  )
}

.cluster_label_validation_issue_lines <- function(
    issues,
    empty_message = "- No structured validator issue table was recorded."
) {
  if (!nrow(issues)) {
    return(empty_message)
  }

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

.cluster_label_can_use_lightweight_repair <- function(validation) {
  issues <- validation$issues %||% .new_cluster_label_issue_table()

  if (!nrow(issues) || !identical(validation$output_status, "labeled")) {
    return(FALSE)
  }

  all(issues$code %in% .cluster_label_lightweight_repair_issue_codes())
}

.cluster_label_validation_repair_guidance <- function(validation) {
  if (!.cluster_label_has_format_issues(validation)) {
    return(character(0))
  }

  c(
    "",
    "Label-format repair reminder:",
    "- display_label must be <= 80 characters and <= 6 words.",
    "- display_label must not contain commas or brackets, and must not end with a period.",
    "- canonical_label must stay lowercase snake_case and must be <= 64 characters.",
    "- If the evidence-backed content is already valid, prefer changing only canonical_label and display_label, plus the minimum dependent wording needed for consistency."
  )
}

.cluster_label_validation_repair_message <- function(validation, previous_output) {
  issues <- validation$issues %||% .new_cluster_label_issue_table()
  issue_lines <- .cluster_label_validation_issue_lines(
    issues,
    empty_message = paste(
      "- No structured validator issue table was recorded,",
      "but the previous output must still be corrected."
    )
  )

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
      .cluster_label_validation_repair_guidance(validation),
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

.cluster_label_lightweight_repair_message <- function(validation) {
  issues <- validation$issues %||% .new_cluster_label_issue_table()
  issue_lines <- .cluster_label_validation_issue_lines(
    issues,
    empty_message = paste(
      "- No structured validator issue table was recorded,",
      "but the label fields must still be corrected."
    )
  )

  paste(
    c(
      "Repair the previously parsed JSON object shown in the assistant message above.",
      "Return one complete corrected JSON object only.",
      "Keep fields that are already valid unless they depend on a corrected label field.",
      "This is a lightweight label-repair pass: do not reconsider the full evidence bundle.",
      "Do not add markdown, commentary, or code fences.",
      "Do not invent new evidence IDs, species, habitats, or external facts.",
      "Keep schema_version, cluster_id, basis_in_data, key_species, external_knowledge, not_confirmed_by_data, confidence, and checks_to_run unchanged unless a listed validator issue explicitly requires a dependent fix.",
      .cluster_label_validation_repair_guidance(validation),
      "",
      "Validator issues:",
      issue_lines
    ),
    collapse = "\n"
  )
}

.cluster_label_lightweight_repair_prompt_bundle <- function(
    prompt_bundle,
    previous_output,
    validation,
    num_predict
) {
  previous_json <- jsonlite::toJSON(
    previous_output,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )
  system_content <- paste(
    prompt_bundle$system,
    "",
    "Repair mode: fix a previously parsed JSON object using validator feedback.",
    "Prefer the smallest defensible edit and keep already-valid evidence-backed fields unchanged.",
    sep = "\n"
  )
  user_content <- .cluster_label_lightweight_repair_message(validation)
  system_chars <- .cluster_evidence_prompt_char_count(system_content)
  user_chars <- .cluster_evidence_prompt_char_count(user_content)
  assistant_chars <- .cluster_evidence_prompt_char_count(previous_json)
  prompt_budget_chars <- prompt_bundle$evidence_budget$prompt_budget_chars %||% NULL

  prompt_bundle$system <- system_content
  prompt_bundle$user <- user_content
  prompt_bundle$messages <- list(
    list(role = "system", content = system_content),
    list(role = "assistant", content = previous_json),
    list(role = "user", content = user_content)
  )
  prompt_bundle$generation$num_predict <- as.integer(num_predict)
  prompt_bundle$evidence_text <- ""
  prompt_bundle$evidence_budget <- list(
    prompt_budget_chars = prompt_budget_chars,
    fixed_overhead_chars = system_chars + user_chars,
    schema_prompt_chars = .cluster_evidence_prompt_char_count(
      prompt_bundle$schema_prompt_text %||% ""
    ),
    schema_text_chars = .cluster_evidence_prompt_char_count(
      prompt_bundle$schema_text %||% ""
    ),
    evidence_budget_chars = 0L,
    evidence_chars_full = 0L,
    evidence_chars_used = 0L,
    repair_previous_json_chars = assistant_chars,
    total_prompt_chars = system_chars + user_chars + assistant_chars,
    fits_within_budget = if (is.null(prompt_budget_chars)) {
      TRUE
    } else {
      (system_chars + user_chars + assistant_chars) <= prompt_budget_chars
    },
    fixed_overhead_exceeds_budget = if (is.null(prompt_budget_chars)) {
      FALSE
    } else {
      (system_chars + user_chars) > prompt_budget_chars
    },
    trimmed = FALSE,
    kept_block_ids = character(0),
    dropped_block_ids = character(0),
    truncated_block_ids = character(0),
    blocks = NULL,
    repair_mode = "lightweight_label_format"
  )
  prompt_bundle$repair_prompt_type <- "lightweight_label_format"
  prompt_bundle
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
    workflow_steps,
    ollama_options
) {
  prompt_bundle <- if (inherits(previous_result, "cluster_label_result") &&
    is.list(previous_result$prompt)) {
    previous_result$prompt
  } else if (identical(workflow_steps, 2L)) {
    template$workflow$label$prompt
  } else {
    template$prompt
  }

  use_lightweight_repair <- .cluster_label_can_use_lightweight_repair(validation)

  if (isTRUE(use_lightweight_repair)) {
    prompt_bundle <- .cluster_label_lightweight_repair_prompt_bundle(
      prompt_bundle = prompt_bundle,
      previous_output = previous_result$output,
      validation = validation,
      num_predict = num_predict
    )
  } else {
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
    prompt_bundle$repair_prompt_type <- "validator_full"
  }

  endpoint <- paste0(sub("/+$", "", base_url), "/api/chat")
  log_paths <- .init_cluster_label_logs(
    log_dir = log_dir,
    cluster_id = evidence$meta$cluster_id,
    model = model,
    variant = paste0(
      variant,
      if (isTRUE(use_lightweight_repair)) {
        "_label_format_repair"
      } else {
        "_validator_repair"
      }
    )
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
        stage_name = if (isTRUE(use_lightweight_repair)) {
          "label_format_repair"
        } else {
          "validator_repair"
        }
      )
    },
    stage_name = if (isTRUE(use_lightweight_repair)) {
      "label_format_repair"
    } else {
      "validator_repair"
    }
  )

  out$workflow_steps <- workflow_steps
  out$workflow <- previous_result$workflow %||% NULL
  out$attempts <- as.integer((previous_result$attempts %||% 0L) + (out$attempts %||% 0L))
  out$repair <- list(
    source = if (isTRUE(use_lightweight_repair)) {
      "validator_lightweight_label_format"
    } else {
      "validator"
    },
    prompt_type = prompt_bundle$repair_prompt_type %||% "validator_full",
    previous_validation_status = validation$validation_status,
    issue_count = nrow(validation$issues %||% .new_cluster_label_issue_table())
  )
  class(out) <- c("cluster_label_result", "list")
  out
}

.cluster_label_speculative_followup_message <- function(
    strict_result,
    strict_validation,
    strict_failure_reason
) {
  issues <- strict_validation$issues %||% .new_cluster_label_issue_table()
  issue_lines <- if (!nrow(issues)) {
    "- No structured validator issue table was recorded for the strict pass."
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

  previous_json <- if (inherits(strict_result, "cluster_label_result") &&
    is.list(strict_result$output)) {
    jsonlite::toJSON(
      strict_result$output,
      auto_unbox = TRUE,
      null = "null",
      pretty = TRUE
    )
  } else {
    "No structured strict-pass JSON is available."
  }

  failure_reason <- .null_default(
    .as_scalar_character(strict_failure_reason),
    "No structured accepted label was obtained in the strict workflow."
  )

  paste(
    c(
      "The strict cluster-labeling workflow did not produce an accepted stable label.",
      "Run a speculative fallback pass using the same evidence only.",
      "If there is any directional signal, return one cautious tentative label.",
      "If you return `status = \"labeled\"`, explicitly list what is missing in `not_confirmed_by_data`.",
      "Prefer compositional or structural wording over strong habitat naming.",
      "Do not add markdown, commentary, or code fences.",
      "If there is no directional signal, return `status = \"abstain\"`.",
      "",
      "Strict-pass failure summary:",
      failure_reason,
      "",
      "Strict-pass validator issues:",
      issue_lines,
      "",
      "Previous strict-pass JSON output:",
      previous_json
    ),
    collapse = "\n"
  )
}

.speculative_missing_for_confidence_text <- function(output) {
  .cluster_label_missing_for_confidence_from_output(output)
}

.normalize_speculative_fallback_output <- function(output) {
  if (!is.list(output)) {
    return(output)
  }

  if (!identical(.as_scalar_character(output$status), "labeled")) {
    return(output)
  }

  confidence <- output$confidence %||% list()
  confidence$score <- 0
  if (!.is_non_empty_scalar_character(confidence$rationale)) {
    confidence$rationale <- paste(
      "This is a tentative speculative label produced after the strict",
      "workflow failed to produce an accepted stable label."
    )
  }
  output$confidence <- confidence
  output
}

.validate_speculative_fallback_contract <- function(validation) {
  if (!inherits(validation, "cluster_label_validation")) {
    return(list(ok = FALSE, message = "Speculative fallback did not return a cluster_label_validation object."))
  }

  output <- validation$output %||% list()
  not_confirmed <- output$not_confirmed_by_data %||% list()
  checks_to_run <- output$checks_to_run %||% list()

  problems <- character(0)

  if (!isTRUE(validation$is_valid)) {
    problems <- c(
      problems,
      paste0(
        "validator returned non-valid status `",
        validation$validation_status %||% "unknown",
        "`"
      )
    )
  }

  if (!identical(validation$output_status, "labeled")) {
    problems <- c(problems, "output status is not `labeled`")
  }

  if (!is.list(not_confirmed) || !length(not_confirmed)) {
    problems <- c(problems, "`not_confirmed_by_data` is empty")
  }

  if (!is.list(checks_to_run) || !length(checks_to_run)) {
    problems <- c(problems, "`checks_to_run` is empty")
  }

  if (!length(problems)) {
    return(list(ok = TRUE, message = NULL))
  }

  list(
    ok = FALSE,
    message = paste(
      c(
        "Speculative fallback output did not satisfy the tentative-label contract:",
        problems
      ),
      collapse = "; "
    )
  )
}

.mark_speculative_validation <- function(
    validation,
    strict_validation_status = NULL,
    strict_outcome = "placeholder",
    speculative_variant = NULL,
    cluster_score = NA_real_
) {
  issues <- validation$issues %||% .new_cluster_label_issue_table()
  issues <- rbind(
    issues,
    data.frame(
      severity = "warning",
      category = "speculative",
      code = "speculative_fallback_label",
      message = "Tentative label produced by the speculative fallback path after the strict workflow failed to produce an accepted stable label.",
      location = "top_level",
      stringsAsFactors = FALSE
    )
  )

  validation$issues <- issues
  validation$validation_status <- "valid_with_warnings"
  validation$needs_human_review <- TRUE
  validation$is_valid <- TRUE
  validation$label_tier <- "speculative"
  validation$is_speculative <- TRUE
  validation$plot_marker <- "*"
  validation$strict_outcome <- strict_outcome %||% "placeholder"
  validation$strict_validation_status <- strict_validation_status %||% NA_character_
  validation$missing_for_confidence_text <- .speculative_missing_for_confidence_text(
    validation$output %||% list()
  )
  validation <- .attach_cluster_label_difficulty_profile(
    validation = validation,
    cluster_score = cluster_score,
    label_origin = .cluster_label_origin_from_speculative_variant(
      speculative_variant
    )
  )
  class(validation) <- "cluster_label_validation"
  validation
}

# The speculative ladder is allowed to evolve by adding new versioned variants.
# Centralizing the "soft" vs "label-required" grouping avoids scattering manual
# switch statements across the workflow code as the ladder grows.
.cluster_label_origin_from_speculative_variant <- function(speculative_variant) {
  speculative_variant <- .as_scalar_character(speculative_variant)

  if (speculative_variant %in% c(
    "label_soft_v1",
    "speculative_fallback_v3",
    "speculative_fallback_v6",
    "speculative_fallback_v8"
  )) {
    return("speculative_v3")
  }

  if (speculative_variant %in% c(
    "label_broad_v1",
    "speculative_fallback_v4",
    "speculative_fallback_v7",
    "speculative_fallback_v9"
  )) {
    return("speculative_v4")
  }

  "speculative_v3"
}

.run_speculative_fallback_cluster_label <- function(
    evidence,
    provider,
    model,
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
    strict_variant,
    strict_label_mode,
    strict_workflow_steps,
    strict_result,
    strict_validation,
    strict_failure_reason,
    cluster_score,
    strict_outcome,
    verbose,
    cluster_tag,
    log_dir,
    request_fn
) {
  request_fn <- request_fn %||% .ollama_chat_request
  variants <- .default_cluster_label_speculative_variants()
  iteration_log <- list()
  failure_messages <- character(0)

  for (i in seq_along(variants)) {
    speculative_variant <- variants[[i]]

    .label_clusters_log(
      verbose,
      cluster_tag,
      ": speculative rung ",
      i,
      "/",
      length(variants),
      " started with variant `",
      speculative_variant,
      "` (model=",
      model,
      ", num_predict=",
      num_predict,
      ")."
    )

    attempt <- .run_speculative_fallback_single_variant(
      evidence = evidence,
      provider = provider,
      model = model,
      speculative_variant = speculative_variant,
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
      strict_variant = strict_variant,
      strict_label_mode = strict_label_mode,
      strict_workflow_steps = strict_workflow_steps,
      strict_result = strict_result,
      strict_validation = strict_validation,
      strict_failure_reason = strict_failure_reason,
      strict_outcome = strict_outcome,
      cluster_score = cluster_score,
      log_dir = log_dir,
      request_fn = request_fn
    )

    iteration_log <- c(iteration_log, attempt$iteration_log %||% list())

    if (isTRUE(attempt$success)) {
      return(list(
        success = TRUE,
        llm_result = attempt$llm_result,
        validation = attempt$validation,
        num_predict_used = attempt$num_predict_used,
        iteration_log = iteration_log,
        used_variant = speculative_variant
      ))
    }

    failure_messages <- c(
      failure_messages,
      paste0(speculative_variant, ": ", attempt$failure_reason %||% "unknown reason.")
    )
  }

  list(
    success = FALSE,
    failure_reason = paste(failure_messages, collapse = " | "),
    iteration_log = iteration_log
  )
}

.run_speculative_fallback_single_variant <- function(
    evidence,
    provider,
    model,
    speculative_variant,
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
    strict_variant,
    strict_label_mode,
    strict_workflow_steps,
    strict_result,
    strict_validation,
    strict_failure_reason,
    strict_outcome,
    cluster_score,
    log_dir,
    request_fn
) {
  speculative_label_mode <- if (identical(strict_label_mode, "dynamic")) {
    "open"
  } else {
    strict_label_mode
  }

  template <- llm_label_cluster(
    evidence = evidence,
    provider = provider,
    model = model,
    variant = speculative_variant,
    label_mode = speculative_label_mode,
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
    workflow_steps = 1L,
    dry_run = TRUE,
    request_fn = request_fn
  )

  prompt_bundle <- template$prompt
  prompt_bundle$messages <- c(
    prompt_bundle$messages,
    list(
      list(
        role = "user",
        content = .cluster_label_speculative_followup_message(
          strict_result = strict_result,
          strict_validation = strict_validation,
          strict_failure_reason = strict_failure_reason
        )
      )
    )
  )

  endpoint <- paste0(sub("/+$", "", base_url), "/api/chat")
  log_paths <- .init_cluster_label_logs(
    log_dir = log_dir,
    cluster_id = evidence$meta$cluster_id,
    model = model,
    variant = paste0(speculative_variant, "_after_", strict_outcome)
  )

  out <- tryCatch(
    .run_structured_llm_stage(
      evidence = evidence,
      provider = provider,
      model = model,
      variant = speculative_variant,
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
          stage_name = "speculative_fallback"
        )
      },
      stage_name = "speculative_fallback"
    ),
    error = function(e) e
  )

  if (inherits(out, "error")) {
    return(list(
      success = FALSE,
      failure_reason = conditionMessage(out),
      iteration_log = list(
        list(
          iteration = NA_integer_,
          mode = speculative_variant,
          result = "error",
          num_predict = as.integer(num_predict %||% NA_integer_),
          failure_reason = conditionMessage(out)
        )
      )
    ))
  }

  out$workflow_steps <- 1L
  out$workflow <- list(
    strict = list(
      variant = strict_variant,
      label_mode = strict_label_mode,
      workflow_steps = strict_workflow_steps,
      output_status = strict_validation$output_status %||% NULL,
      validation_status = strict_validation$validation_status %||% NULL,
      failure_reason = strict_failure_reason %||% NULL
    )
  )
  out$speculative <- list(
    used = TRUE,
    mode = "soft_label_ladder",
    variant = speculative_variant,
    label_mode = speculative_label_mode,
    label_tier = "speculative",
    plot_marker = "*",
    strict_outcome = strict_outcome,
    strict_variant = strict_variant,
    strict_workflow_steps = strict_workflow_steps,
    strict_validation_status = strict_validation$validation_status %||% NULL,
    strict_failure_reason = strict_failure_reason %||% NULL
  )
  class(out) <- c("cluster_label_result", "list")
  out$output <- .normalize_speculative_fallback_output(out$output)

  validation <- validate_cluster_label(out, evidence)
  if (identical(validation$output_status, "abstain") && isTRUE(validation$is_valid)) {
    return(list(
      success = FALSE,
      failure_reason = "variant returned a valid abstain",
      llm_result = out,
      validation = validation,
      iteration_log = list(
        list(
          iteration = NA_integer_,
          mode = speculative_variant,
          result = "abstain",
          num_predict = as.integer(num_predict %||% NA_integer_),
          validation_status = validation$validation_status %||% NA_character_,
          failure_reason = "variant returned a valid abstain"
        )
      )
    ))
  }

  contract <- .validate_speculative_fallback_contract(validation)
  if (!isTRUE(contract$ok)) {
    return(list(
      success = FALSE,
      failure_reason = contract$message,
      llm_result = out,
      validation = validation,
      iteration_log = list(
        list(
          iteration = NA_integer_,
          mode = speculative_variant,
          result = "rejected",
          num_predict = as.integer(num_predict %||% NA_integer_),
          validation_status = validation$validation_status %||% NA_character_,
          failure_reason = contract$message
        )
      )
    ))
  }

  validation <- .mark_speculative_validation(
    validation = validation,
    strict_validation_status = strict_validation$validation_status %||% NULL,
    strict_outcome = strict_outcome,
    speculative_variant = speculative_variant,
    cluster_score = cluster_score
  )
  out$speculative$missing_for_confidence_text <- validation$missing_for_confidence_text %||%
    NA_character_

  list(
    success = TRUE,
    llm_result = out,
    validation = validation,
    num_predict_used = as.integer(num_predict %||% NA_integer_),
    iteration_log = list(
      list(
        iteration = NA_integer_,
        mode = speculative_variant,
        result = "ok",
        num_predict = as.integer(num_predict %||% NA_integer_),
        validation_status = validation$validation_status %||% NA_character_,
        label_tier = "speculative"
      )
    )
  )
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
