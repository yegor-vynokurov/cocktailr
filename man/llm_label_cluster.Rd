% Manual page for llm_label_cluster
\name{llm_label_cluster}
\alias{llm_label_cluster}
\title{Label one Cocktail cluster with a local LLM via Ollama}
\usage{
llm_label_cluster(
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
  internal_prompt_version = .default_cluster_label_internal_prompt_version(),
  dry_run = FALSE,
  debug = FALSE,
  log_dir = NULL,
  request_fn = NULL
)
}
\arguments{
\item{evidence}{A \code{"cluster_evidence"} object, typically produced by
\code{\link{cluster_evidence}}.}

\item{provider}{Character scalar. Currently only \code{"ollama"} is supported.}

\item{model}{Character scalar naming the local model to use.}

\item{variant}{Character scalar naming the label-stage prompt variant.
The current main public values are \code{"label_primary_v1"},
\code{"label_soft_v1"}, and \code{"label_broad_v1"}. Older versioned
prompt IDs are still accepted as compatibility aliases, but they resolve
to the current public trio.}

\item{label_mode}{Character scalar controlling how freely the model may
choose labels. Use \code{"open"} (default) for unrestricted label
generation, \code{"constrained"} to force labeled answers to come from
the packaged coarse vocabulary or a user override set via
\code{options(cocktailr.cluster_label_vocabulary_path = ...)}, and
\code{"dynamic"} to reuse candidate labels extracted from the draft
stage. Dynamic mode currently requires \code{workflow_steps = 3}.}

\item{base_url}{Base URL of the Ollama server.}

\item{schema_path}{Optional path to a JSON schema file. By default the packaged
schema in \code{inst/schemas/cluster_label_output_schema.json} is used.}

\item{temperature}{Optional numeric override for generation temperature.}

\item{top_p}{Optional numeric override for \code{top_p}.}

\item{seed}{Optional integer override for generation seed.}

\item{num_predict}{Optional integer override for the generation length cap.
By default the packaged prompt catalog uses \code{2400}.}

\item{prompt_budget_chars}{Optional integer character budget for the final
assembled prompt messages. Default \code{10000}. Use \code{NULL} to keep the
full evidence text without prompt-budget trimming.}

\item{keep_alive}{Optional Ollama keep-alive value, for example \code{"5m"}.}

\item{ollama_options}{Optional named list of additional Ollama generation
options passed through under \code{request$options}. Use this for model-specific
controls such as \code{num_ctx}. Explicit top-level arguments such as
\code{temperature}, \code{top_p}, \code{seed}, and \code{num_predict} still take
precedence over the same names inside \code{ollama_options}.}

\item{timeout_sec}{Numeric request timeout in seconds.}

\item{max_retries}{Integer >= 0. Number of repair retries after malformed JSON
or missing required top-level fields.}

\item{workflow_steps}{Integer workflow length. Use \code{1} (default) for the
existing one-step label workflow. Use \code{2} for a gate-then-label
workflow where stage 1 decides \code{label / abstain} and stage 2 is called
only if the gate passes. Use \code{3} for the staged
draft-analysis -> label-selection -> explanation workflow, where the model
first writes a freeform draft, then selects a short label through the public
prompt ladder, and only after that produces the final structured
explanation.}

\item{use_brainstorm}{Logical; if \code{TRUE} (default), run the draft
analysis step before label decision. If \code{FALSE}, the downstream
label-decision and summary / abstain-reason stages use the cluster
evidence directly without a brainstorm pass.}

\item{internal_prompt_version}{Character scalar naming the subdirectory under
\code{inst/prompts/internal_cluster_labeling/} that contains the active
internal service-prompt bundle. Default \code{"v1"}. Copy that folder to
\code{"v2"}, \code{"v3"}, and so on when you want to iterate on the internal
draft / decision / summary prompts without changing the currently active
production set.}

\item{dry_run}{Logical; if \code{TRUE}, return the assembled prompt bundle and
Ollama request payload without making a network request.}

\item{debug}{Logical. If \code{TRUE}, collect per-stage LLM debug logs
(prompts, requests, responses, parsed artifacts, diagnostics). Default
\code{FALSE}. When \code{FALSE}, no LLM stage-log files are written even if
\code{log_dir} is supplied.}

\item{log_dir}{Optional directory for writing request / response artifacts.
When supplied, \code{llm_label_cluster()} creates one timestamped run folder
inside \code{log_dir} and stores prompt, request, response, and parsed-output
files there. When a relative path is used and a local \code{cocktailr} source
checkout can be detected, it is resolved against that package root.}

\item{request_fn}{Optional expert/testing override for the provider request
function.}
}
\value{
If \code{dry_run = TRUE}, returns a \code{"cluster_label_request"} list.

Otherwise returns a \code{"cluster_label_result"} list containing the prompt,
request payload, raw provider response, parsed output object, attempt count,
workflow length, and log paths, including the per-run log folder. For
\code{workflow_steps = 2}, the result also contains a \code{workflow} list
with gate-stage and label-stage details. For \code{workflow_steps = 3}, the
\code{workflow} list contains draft, label-selection, and explanation stages.
}
\description{
Builds a prompt from a \code{\link{cluster_evidence}} object, the packaged JSON
output schema, and a named prompt variant from the packaged prompt catalog,
then calls a local Ollama model to obtain one structured label /
interpretation result. In two-step mode, an internal gate prompt is used
first to decide whether final labeling should proceed. In three-step mode,
label selection is separated from explanation generation so that the accepted
label can be fixed before the final structured explanation is written.
}
\examples{
syn <- generate_synthetic_vegetation_data(
  n_plots_per_community = 4,
  n_transition_plots = 2,
  seed = 42
)
res <- cocktail_cluster(
  vegmatrix = syn$wide_matrix,
  progress = FALSE,
  plot_values = "rel_cover",
  species_cluster_phi = TRUE,
  save_vegmatrix = TRUE
)
ev <- cluster_evidence(res, cluster = "c_1", top_n_phi = 3)

req <- llm_label_cluster(
  evidence = ev,
  model = "gemma4:12b",
  variant = "label_primary_v1",
  dry_run = TRUE
)

names(req)
req$request$model
}
