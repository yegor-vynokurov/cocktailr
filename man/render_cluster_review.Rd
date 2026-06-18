% Manual page for render_cluster_review
\name{render_cluster_review}
\alias{render_cluster_review}
\alias{print.cluster_review_artifact}
\title{Render a human-reviewable cluster labeling card}
\usage{
render_cluster_review(
  x,
  evidence,
  validation = NULL,
  file = NULL,
  review_dir = NULL,
  metadata_file = NULL,
  full = FALSE,
  include_front_matter = full,
  write_metadata = full,
  manual_notes = NULL,
  title = NULL
)

\method{print}{cluster_review_artifact}(x, ...)
}
\arguments{
\item{x}{A parsed structured output list, or an object returned by
\code{\link{llm_label_cluster}}.}

\item{evidence}{A \code{"cluster_evidence"} object produced by
\code{\link{cluster_evidence}}.}

\item{validation}{Optional \code{"cluster_label_validation"} object. If
\code{NULL}, validation is computed automatically with
\code{\link{validate_cluster_label}}.}

\item{file}{Optional path to a markdown output file. When a relative path is
used and a local \code{cocktailr} source checkout can be detected, the path is
resolved against that package root.}

\item{review_dir}{Optional root directory for automatically generated review
files. When supplied, the function creates a dataset-aware subdirectory and
saves \code{"<cluster_id>_review.md"} there. When a relative path is used and
a local \code{cocktailr} source checkout can be detected, it is resolved
against that package root.}

\item{metadata_file}{Optional path to a sidecar JSON metadata file. When
omitted and \code{file} is provided, the path is derived automatically.
Relative paths follow the same resolution rule as \code{file}.}

\item{full}{Logical; if \code{FALSE} (default), render a compact final card.
If \code{TRUE}, include review summary, external knowledge, validator
warnings, checks, evidence snapshot, evidence limitations, and manual notes.}

\item{include_front_matter}{Logical; if \code{TRUE} (default when
\code{full = TRUE}), include YAML front matter with machine-readable status
fields.}

\item{write_metadata}{Logical; if \code{TRUE} (default when
\code{full = TRUE}), write the metadata JSON sidecar whenever \code{file},
\code{review_dir}, or \code{metadata_file} is provided.}

\item{manual_notes}{Optional character vector for the manual review section. If
\code{NULL}, a placeholder checklist is inserted.}

\item{title}{Optional markdown title. Defaults to
\code{"Cluster Review: <cluster_id>"}.}

\item{...}{Unused.}
}
\value{
An object of class \code{"cluster_review_artifact"} with the rendered markdown
text, line vector, machine-readable metadata, and any written output paths.
}
\description{
Builds a review card that combines one structured cluster-label output,
its underlying \code{\link{cluster_evidence}} bundle, and the result of
\code{\link{validate_cluster_label}}. By default the card is intentionally
compact; use \code{full = TRUE} for the expanded review bundle.
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
topo <- ev$summaries$species_topological

out <- list(
  schema_version = "0.1.0",
  cluster_id = ev$meta$cluster_id,
  status = "abstain",
  canonical_label = NULL,
  display_label = NULL,
  interpretation_summary = paste(
    "The cluster has some recurring species, but the evidence",
    "is not distinctive enough for a stable ecological label."
  ),
  basis_in_data = list(
    list(
      claim_id = "C1",
      statement = "A recurring species core is present, but not decisive.",
      evidence_ids = c(topo$evidence_id[[1]]),
      support_strength = "weak"
    )
  ),
  key_species = list(
    list(
      species = topo$species[[1]],
      role = "topological",
      evidence_ids = c(topo$evidence_id[[1]])
    )
  ),
  external_knowledge = list(),
  not_confirmed_by_data = list(
    list(
      statement = "A habitat-level label cannot be confirmed.",
      reason = "The evidence bundle does not contain direct habitat metadata."
    )
  ),
  confidence = list(
    score = 0.35,
    rationale = "The cluster has some structure, but not enough for a stable label."
  ),
  checks_to_run = list(
    list(
      check = "Compare with sibling clusters.",
      priority = "high",
      reason = "Contrastive evidence would help decide whether abstention should be lifted."
    )
  ),
  abstain_reason = "Distinctiveness is insufficient for a stable label."
)

val <- validate_cluster_label(out, ev)
review <- render_cluster_review(out, ev, validation = val)
cat(review$markdown)
}
