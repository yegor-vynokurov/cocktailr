\name{label_hclust_leaves}
\alias{label_hclust_leaves}
\title{Replace \code{hclust} leaf labels using saved cluster labels}
\usage{
label_hclust_leaves(
  hc,
  label_registry = "auto",
  x = NULL,
  label_field = "plot_label_short",
  fallback = c("keep", "cluster"),
  warn_missing = TRUE
)
}
\arguments{
\item{hc}{An object of class \code{"hclust"}.}

\item{label_registry}{\code{NULL}, \code{"auto"}, a
\code{cluster_label_registry} table, or a \code{cluster_label_batch_result}
object returned by \code{\link{label_clusters}}. If \code{"auto"}, the helper
tries to load the most relevant saved \code{cluster_label_registry.csv}.}

\item{x}{Optional Cocktail object used only when
\code{label_registry = "auto"} to prefer a dataset-aware saved registry.}

\item{label_field}{Character scalar naming the registry column to use for
replacement. Common choices are \code{"hclust_label_compact"},
\code{"plot_label_short"}, \code{"legend_label"}, and \code{"display_label"}.
Default: \code{"plot_label_short"}.}

\item{fallback}{One of \code{"keep"} or \code{"cluster"}. \code{"keep"}
leaves unmatched or empty replacements unchanged. \code{"cluster"} falls back
to normalized cluster IDs such as \code{"c_12"}.}

\item{warn_missing}{Logical. If \code{TRUE}, warn when some leaves could not be
replaced with a non-empty label from the registry.}
}
\value{
A modified \code{hclust} object. The original leaf labels are preserved in
\code{attr(x, "cocktailr_original_labels")}.
}
\description{
Small helper for base-R \code{hclust} workflows built from Cocktail cluster
IDs, such as \code{cluster_phi_dist() |> hclust() |> plot()}. It rewrites leaf
labels like \code{"c_12"} using a \code{\link{cluster_label_registry}} table or
a batch result from \code{\link{label_clusters}}.
}
\details{
This helper is intended for low-cluster-count overview plots where replacing
the leaf labels themselves is readable. For full Cocktail dendrograms,
\code{\link{cocktail_plot}} remains the recommended path: it keeps stable
numeric IDs on the dendrogram and adds a separate legend/caption with
human-readable labels.
}
\examples{
\dontrun{
D <- cluster_phi_dist(
  x = res,
  clusters = c("c_12", "c_25", "c_35")
)

hc <- hclust(D, method = "average")

run <- label_clusters(
  x = res,
  clusters = c("c_12", "c_25", "c_35"),
  model = "gemma4:12b",
  variant = "label_primary_v1",
  labels_for_imgs = TRUE
)

hc_labeled <- label_hclust_leaves(
  hc,
  label_registry = run$label_registry,
  label_field = "legend_label"
)

plot(hc_labeled, cex = 0.8)
}
}
