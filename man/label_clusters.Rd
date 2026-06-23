\name{label_clusters}
\alias{label_clusters}
\title{Run an end-to-end cluster labeling workflow for one or more Cocktail clusters}
\usage{
label_clusters(
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
  variant = "strict_abstention_gate_v1",
  base_url = getOption("cocktailr.ollama_base_url", "http://localhost:11434"),
  schema_path = NULL,
  temperature = NULL,
  top_p = NULL,
  seed = NULL,
  num_predict = NULL,
  keep_alive = NULL,
  ollama_options = NULL,
  timeout_sec = 600,
  max_retries = 1L,
  workflow_steps = 1L,
  max_iterations = 2L,
  review_dir = file.path("temp", "reports", "cluster_reviews"),
  verbose = TRUE,
  labels_for_imgs = FALSE,
  speculative_fallback_mode = c("off", "after_rejection", "after_nonaccepted"),
  full = FALSE,
  include_front_matter = full,
  write_metadata = full,
  log_dir = NULL,
  request_fn = NULL
)
}
\arguments{
\item{x}{A \code{"cocktail"} object produced by \code{\link{cocktail_cluster}}.}

\item{clusters}{Optional cluster identifiers to process. Accepts numeric IDs
such as \code{c(12, 27)} or labels such as \code{c("c_12", "c_27")}. If
\code{NULL}, clusters are selected automatically from the score-ranked
selection table.}

\item{top_n}{Integer. Used only when \code{clusters = NULL}. Default
\code{10}.}

\item{select_min_phi, select_min_k, select_min_score, select_mode}{Cluster
selection controls forwarded to \code{\link{select_clusters}} when
\code{clusters = NULL}.}

\item{top_n_phi, n_prototype_plots, n_borderline_plots, include_cover}{Evidence
extraction controls forwarded to \code{\link{cluster_evidence}}.}

\item{semantic_layer}{Logical. If \code{TRUE}, try to enrich each cluster's
evidence bundle with indicator-derived ecological axes from the optional
semantic layer built from files under \code{data-raw/external/}. Failures in
this auxiliary step are recorded in the batch summary and the workflow
continues with the plain evidence bundle. Default \code{FALSE}.}

\item{semantic_min_phi}{Optional numeric scalar forwarded to
\code{score_cluster_semantics()}.}

\item{semantic_bootstrap}{Integer bootstrap count for semantic axis
summaries. Default \code{200}.}

\item{semantic_root}{Optional project root override for the semantic layer.
By default the semantic helpers use \code{cocktailr_project_root()}.}

\item{semantic_force_species, semantic_force_reference}{Logical refresh flags
forwarded to \code{score_cluster_semantics()}.}

\item{provider, model, variant, base_url, schema_path, temperature, top_p, seed, num_predict, keep_alive, ollama_options, timeout_sec, max_retries, workflow_steps, log_dir, request_fn}{LLM
controls forwarded to \code{\link{llm_label_cluster}}.}

\item{max_iterations}{Integer workflow budget. Currently allowed values are
\code{1} and \code{2}. Default \code{2}.}

\item{review_dir}{Directory where markdown review cards are written. Default
\code{file.path("temp", "reports", "cluster_reviews")}. When a relative path
is used and a local \code{cocktailr} source checkout can be detected, it is
resolved against that package root.}

\item{verbose}{Logical. If \code{TRUE} (default), print short progress
messages for cluster-level workflow stages, retries, and saved review
artifacts.}

\item{labels_for_imgs}{Logical. If \code{TRUE}, build a plotting-oriented
registry table with \code{\link{cluster_label_registry}} after all review
cards are generated, save it automatically as
\code{"cluster_label_registry.csv"} next to the review cards, and return
it as \code{$label_registry}. Default \code{FALSE}.}

\item{speculative_fallback_mode}{Character scalar controlling whether a
softer speculative fallback ladder may run after the strict workflow fails
to produce an accepted structured label. Default \code{"off"}. Supported
non-default values are \code{"after_rejection"} for the narrow placeholder
branch only, and \code{"after_nonaccepted"} to also continue after a valid
strict abstention.}

\item{full}{Logical. Forwarded to \code{\link{render_cluster_review}}.}

\item{include_front_matter, write_metadata}{Forwarded to
\code{\link{render_cluster_review}}.}
}
\value{
A list of class \code{"cluster_label_batch_result"} with:
\itemize{
  \item \code{summary}: one-row-per-cluster workflow summary
  \item \code{results}: named list with evidence, llm result, validation, and
  review artifact for each cluster
  \item \code{selection}: the resolved cluster table used for the run
  \item \code{label_registry}: optional flat registry table for downstream
  plotting, present when \code{labels_for_imgs = TRUE}
  \item \code{label_registry_file}: optional path to the saved registry CSV,
  present when \code{labels_for_imgs = TRUE}
}
}
\description{
High-level orchestration helper around \code{\link{cluster_evidence}},
\code{\link{llm_label_cluster}}, \code{\link{validate_cluster_label}}, and
\code{\link{render_cluster_review}}.
}
\details{
For each selected cluster, the function builds a deterministic evidence bundle,
requests one structured label / abstention from the local LLM, validates the
structured output, optionally performs one validator-guided repair pass, and
saves a markdown review card.

If no \code{clusters} are supplied, the function selects up to the first
\code{top_n} clusters ranked by the Cocktail selection score from
\code{\link{select_clusters}(return = "table")}.

The workflow is intentionally bounded: at most two top-level iterations are
attempted per cluster. If the first attempt fails with an EOF-like structured
output error, the second attempt automatically doubles \code{num_predict}. If
no valid structured result is available after the bounded retry budget, the
function writes a placeholder review card that records the cluster, model,
prompt provenance, and failure reason.

Relative review-card paths follow the same source-root resolution rule as
\code{\link{render_cluster_review}}.
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

req <- label_clusters(
  x = res,
  clusters = "c_1",
  model = "gemma4:12b",
  variant = "strict_abstention_gate_v1",
  review_dir = file.path("temp", "reports", "cluster_reviews")
)

req$summary
}
