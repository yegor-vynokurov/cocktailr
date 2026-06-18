% Manual page for llm_label_cluster
\name{llm_label_cluster}
\alias{llm_label_cluster}
\title{Label one Cocktail cluster with a local LLM via Ollama}
\usage{
llm_label_cluster(
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
)
}
\arguments{
\item{evidence}{A \code{"cluster_evidence"} object, typically produced by
\code{\link{cluster_evidence}}.}

\item{provider}{Character scalar. Currently only \code{"ollama"} is supported.}

\item{model}{Character scalar naming the local model to use.}

\item{variant}{Character scalar naming the label-stage prompt variant.
Currently supported label-stage values include
\code{"concise_label_v1"}, \code{"conservative_interpretation_v1"},
\code{"abstain_first_v1"}, and
\code{"strict_abstention_gate_v1"}.}

\item{base_url}{Base URL of the Ollama server.}

\item{schema_path}{Optional path to a JSON schema file. By default the packaged
schema in \code{inst/schemas/cluster_label_output_schema.json} is used.}

\item{temperature}{Optional numeric override for generation temperature.}

\item{top_p}{Optional numeric override for \code{top_p}.}

\item{seed}{Optional integer override for generation seed.}

\item{num_predict}{Optional integer override for the generation length cap.}

\item{keep_alive}{Optional Ollama keep-alive value, for example \code{"5m"}.}

\item{timeout_sec}{Numeric request timeout in seconds.}

\item{max_retries}{Integer >= 0. Number of repair retries after malformed JSON
or missing required top-level fields.}

\item{workflow_steps}{Integer workflow length. Use \code{1} (default) for the
existing one-step label workflow. Use \code{2} for a gate-then-label
workflow where stage 1 decides \code{label / abstain} and stage 2 is called
only if the gate passes.}

\item{dry_run}{Logical; if \code{TRUE}, return the assembled prompt bundle and
Ollama request payload without making a network request.}

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
with gate-stage and label-stage details.
}
\description{
Builds a prompt from a \code{\link{cluster_evidence}} object, the packaged JSON
output schema, and a named prompt variant from the packaged prompt catalog,
then calls a local Ollama model to obtain one structured label /
interpretation result. In two-step mode, an internal gate prompt is used
first to decide whether final labeling should proceed.
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
  variant = "strict_abstention_gate_v1",
  dry_run = TRUE
)

names(req)
req$request$model
}
