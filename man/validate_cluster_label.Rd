% Manual page for validate_cluster_label
\name{validate_cluster_label}
\alias{validate_cluster_label}
\alias{print.cluster_label_validation}
\title{Validate a structured cluster label against cluster evidence}
\usage{
validate_cluster_label(x, evidence)

\method{print}{cluster_label_validation}(x, ...)
}
\arguments{
\item{x}{A parsed structured output list, or an object returned by
\code{\link{llm_label_cluster}}.}

\item{evidence}{A \code{"cluster_evidence"} object produced by
\code{\link{cluster_evidence}}.}

\item{...}{Unused.}
}
\value{
An object of class \code{"cluster_label_validation"} with the validated output,
evidence coverage summary, validation status, issue table, and a
\code{needs_human_review} flag.
}
\description{
Runs a conservative validation layer over the structured output produced by
\code{\link{llm_label_cluster}}. The validator checks required fields, output
status consistency, evidence ID integrity, separation between data-derived
claims and external knowledge, and a small set of heuristics for unsupported
ecological overreach.
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
print(val)
}
