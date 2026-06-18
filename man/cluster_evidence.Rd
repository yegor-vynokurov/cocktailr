% Manual page for cluster_evidence
\name{cluster_evidence}
\alias{cluster_evidence}
\title{Build an evidence object for one Cocktail cluster}
\usage{
cluster_evidence(
  x,
  cluster,
  top_n_phi = 10L,
  n_prototype_plots = 5L,
  n_borderline_plots = 5L,
  include_cover = TRUE
)
}
\arguments{
\item{x}{A \code{"cocktail"} object produced by \code{\link{cocktail_cluster}}.}

\item{cluster}{A single cluster identifier, either as a label like \code{"c_37"}
or as an integer node ID such as \code{37}.}

\item{top_n_phi}{Optional integer >= 1. Maximum number of phi-ranked species
to keep in \code{summaries$species_phi}.}

\item{n_prototype_plots}{Optional integer >= 0. Maximum number of prototype
plots to keep.}

\item{n_borderline_plots}{Optional integer >= 0. Maximum number of borderline
plots to keep.}

\item{include_cover}{Logical; if \code{TRUE} and \code{x$vegmatrix} is available,
compute cover summaries for the cluster's topological species.}
}
\value{
A list of class \code{"cluster_evidence"} with top-level components
\code{meta}, \code{context}, \code{summaries}, \code{evidence},
\code{limitations}, and \code{future}.
}
\description{
Collects deterministic, machine-readable evidence for a single Cocktail cluster.
The returned object is intended as the fact layer between a \code{"cocktail"}
object and downstream LLM-based labeling or interpretation.
}
\details{
If the source \code{"cocktail"} object contains dataset provenance
(for example from \code{\link{generate_synthetic_vegetation_data}} or from
\code{cocktail_cluster(dataset_path = ...)}), that information is copied into
\code{meta$dataset}.
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
print(ev)
}
