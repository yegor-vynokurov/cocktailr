cocktailr
================

- [cocktailr](#cocktailr)
  - [Citation](#citation)
  - [Overview](#overview)
  - [Background](#background)
  - [Installation](#installation)
  - [Synthetic data for cluster labeling](#synthetic-data-for-cluster-labeling)
  - [Typical workflow](#typical-workflow)
    - [Evidence for LLM-assisted labeling (experimental)](#evidence-for-llm-assisted-labeling-experimental)
    - [Long-format input](#long-format-input)
    - [Notes on long-format input](#notes-on-long-format-input)
    - [1) Visualize the dendrogram](#1-visualize-the-dendrogram)
    - [2) Select clusters at a φ cut **or** select strongest clusters by
      score](#2-select-clusters-at-a-φ-cut-or-select-strongest-clusters-by-score)
    - [3) Cluster diagnostics (helper
      functions)](#3-cluster-diagnostics-helper-functions)
    - [4) Diagnostic species for selected
      clusters](#4-diagnostic-species-for-selected-clusters)
    - [5) Distances between clusters (direct plot co-membership
      φ)](#5-distances-between-clusters-direct-plot-co-membership-φ)
    - [6) Visualize grouped clusters on the Cocktail
      dendrogram](#6-visualize-grouped-clusters-on-the-cocktail-dendrogram)
    - [7) Assign plots (relevés) to candidate vegetation
      units](#7-assign-plots-relevés-to-candidate-vegetation-units)
    - [(Optional) Attach assignments to a header data
      frame](#optional-attach-assignments-to-a-header-data-frame)
  - [Function help](#function-help)

# cocktailr

*Cocktail* clustering for diagnostic species-based vegetation
classification.

------------------------------------------------------------------------

## Citation

If you use `cocktailr`, please cite the GitHub repository and the
original Cocktail method papers listed below. A manuscript describing
the package and workflow has been prepared for submission.

## Overview

**cocktailr** provides fast and reproducible *Cocktail* clustering for
vegetation-plot data. It identifies groups of co-occurring species from
**plots × species** tables or **long-format vegetation tables**
(plot–species–value) and supports their use in diagnostic species-based
vegetation classification.

The package implements:

- **Hierarchical Cocktail clustering** of species
  (`cocktail_cluster()`).
- **Dendrogram plotting** with φ heights and optional cluster bands
  (`cocktail_plot()`).
- Extraction of **parent clusters at a φ cut** (`clusters_at_cut()`).
- **Selection of strong clusters** by a combined score
  (`select_clusters()`).
- **Diagnostic species lists** for clusters or combinations of clusters
  (`species_in_clusters()`).
- Listing **plots (relevés) belonging to clusters or combinations of
  clusters** (`releves_in_clusters()`).
- Finding **clusters that contain a given species (or set of species)**
  (`clusters_with_species()`).
- **Distances between clusters** based on direct plot co-membership φ
  (`cluster_phi_dist()`).
- **Assignment of plots (relevés) to candidate vegetation units** using
  cover- and φ-based strategies (`assign_releves()`).

- **Human-readable synthetic vegetation datasets** for testing
  clustering and LLM-based cluster labeling
  (`generate_synthetic_vegetation_data()`).
- **Evidence objects** for LLM-assisted cluster labeling and
  interpretation (`cluster_evidence()`).

------------------------------------------------------------------------

## Background

The *Cocktail* method (Bruelheide 2000, 2016) identifies sets of species
that co-occur more often than expected by chance and merges them
hierarchically according to the **phi coefficient of association**. Each
resulting cluster is characterized by its constituent species and a
threshold (*m*) indicating how many species from that cluster a plot
must contain to belong to it.

For details, see the original works:

- Bruelheide, H. (2000). *A new measure of fidelity and its application
  to defining species groups.* **Journal of Vegetation Science**, 11,
  167–178. <https://doi.org/10.2307/3236796>  
- Bruelheide, H. (2016). *Cocktail clustering – a new hierarchical
  agglomerative algorithm for extracting species groups in vegetation
  databases.* **Journal of Vegetation Science**, 27(6), 1297–1307.
  <https://doi.org/10.1111/jvs.12454>

------------------------------------------------------------------------

## Installation

The development version of `cocktailr` can be installed from GitHub:

``` r
install.packages("remotes")

remotes::install_github("dvynokur/cocktailr")
library(cocktailr)
```

For reproducible analyses based on a tagged release, install a specific
version, for example:

``` r
remotes::install_github("dvynokur/cocktailr@v0.1.0")
```

This command will work after the corresponding GitHub release/tag has
been created.

If you are working from a local development checkout and want to use
newly added functions immediately, either load the package source into
the current R session:

``` r
pkgload::load_all("path/to/cocktailr")
```

or reinstall the local package before calling `library(cocktailr)`
again.

------------------------------------------------------------------------

## Synthetic data for cluster labeling

`generate_synthetic_vegetation_data()` creates a synthetic but
human-readable vegetation dataset with real plant names. It is designed
for testing the Cocktail workflow and for building supervised datasets
for local LLM experiments where the model has to assign human-readable
labels to Cocktail clusters.

The generator always uses four predefined community templates:

- `dry_meadow`
- `wet_meadow`
- `woodland`
- `ruderal_edge`

Each template has its own diagnostic species set. The function then adds
shared generalists, rare/noise species, and optional transition plots
between community pairs. Species names are real, but co-occurrence
probabilities and cover values are artificial.

Useful arguments:

- `n_plots_per_community`: one number for all four communities, or a
  length-4 vector for `dry_meadow`, `wet_meadow`, `woodland`, and
  `ruderal_edge`
- `n_transition_plots`: number of mixed plots split across predefined
  transition pairs
- `seed`: random seed for reproducibility
- `use_underscores`: convert species names such as `Festuca ovina` to
  `Festuca_ovina`
- `keep_absences_in_long`: if `TRUE`, keep zero-valued rows in
  `long_table`; by default only present species are stored

``` r
syn <- generate_synthetic_vegetation_data(
  n_plots_per_community = 30,
  n_transition_plots = 16,
  seed = 42
)
```

The returned list contains:

- `wide_matrix`: plot x species cover matrix, ready for
  `cocktail_cluster()`
- `long_table`: long-format version of the same data with columns
  `plot`, `species`, `value`
- `plot_truth`: gold-standard plot labels, including transition flags
  and source communities
- `species_truth`: expected ecological role of each species plus
  per-community occurrence probabilities
- `community_profiles`: human-readable labels, ecological hints, and
  diagnostic species lists
- `metadata`: generation settings and summary counts

For cluster labeling tasks, a practical workflow is:

1.  Run `cocktail_cluster()` on `syn$wide_matrix` or `syn$long_table`.
2.  Use `species_in_clusters()` to extract diagnostic species for
    selected clusters.
3.  Feed those species lists to your local LLM and ask it for a
    community label or short description.
4.  Compare the predicted labels against `syn$community_profiles`, and
    use `syn$plot_truth` / `syn$species_truth` as ground truth for
    evaluation.

If you want CSV exports like the examples shipped with the package:

``` r
wide_df <- data.frame(
  plot = rownames(syn$wide_matrix),
  syn$wide_matrix,
  check.names = FALSE
)

write.csv(wide_df, "synthetic_vegetation_wide.csv", row.names = FALSE)
write.csv(syn$long_table, "synthetic_vegetation_long.csv", row.names = FALSE)
write.csv(syn$plot_truth, "synthetic_plot_truth.csv", row.names = FALSE)
write.csv(syn$species_truth, "synthetic_species_truth.csv", row.names = FALSE)
write.csv(syn$community_profiles, "synthetic_community_profiles.csv", row.names = FALSE)
```

The package also ships with example exports produced by this function in
`inst/extdata`. After installation, you can load them with:

``` r
extdir <- system.file("extdata", package = "cocktailr")

veg_long <- read.csv(file.path(extdir, "synthetic_vegetation_long.csv"))
plot_truth <- read.csv(file.path(extdir, "synthetic_plot_truth.csv"))
species_truth <- read.csv(file.path(extdir, "synthetic_species_truth.csv"))
community_profiles <- read.csv(file.path(extdir, "synthetic_community_profiles.csv"))
```

------------------------------------------------------------------------

## Typical workflow

`cocktail_cluster()` supports both **wide** (plots × species) and
**long** (plot–species–value) input formats. The main example below uses
a wide matrix; a compact long-format example is provided afterward.

A small end-to-end example on a toy **plots × species** matrix, showing:

1.  Cocktail clustering  
2.  Dendrogram plotting  
3.  Selection of clusters at a φ cut **or** selection of strongest
    clusters by score  
4.  Cluster inspection utilities  
5.  Species lists  
6.  Distances between clusters  
7.  Plot assignment to candidate vegetation units

If you are copying commands into a fresh R session, run the whole chunk
below: it first creates the toy matrix `vm`, then builds the Cocktail
object `res`.

``` r
library(cocktailr)

# Toy plots × species matrix with percentage cover
vm <- matrix(
  c(
    60,50,40,30,  5, 0,10, 0,
    55,45,35,25, 10, 5, 0, 0,
    50,40,30,20,  5,10, 0, 5,
    45,35,25,15,  0, 5, 5, 0,
    10, 5, 0, 0, 60,50,40,30,
     5,10, 0, 0, 55,45,35,25,
     0, 5,10, 0, 50,40,30,20,
     0, 0, 5,10, 45,35,25,15
  ),
  nrow = 8, byrow = TRUE,
  dimnames = list(
    paste0("plot", 1:8),
    paste0("sp",   1:8)
  )
)

# 1) Cocktail clustering, keeping relative cover and species-cluster phi
res <- cocktail_cluster(
  vegmatrix           = vm,
  progress            = FALSE,
  plot_values         = "rel_cover",
  species_cluster_phi = TRUE,
  save_vegmatrix      = TRUE   # default; needed later by assign_releves()
)

names(res)
#> [1] "Cluster.species"     "Cluster.info"        "Plot.cluster"       
#> [4] "Cluster.merged"      "Cluster.height"      "Species.cluster.phi"
#> [7] "species"             "plots"               "vegmatrix"
```

`assign_releves()` reads the vegetation matrix from `res$vegmatrix`, so
keep `save_vegmatrix = TRUE` (default) if you plan to use assignment
later.

For very large datasets, you can reduce memory usage by setting
`save_vegmatrix = FALSE` in `cocktail_cluster()`. In that case, however,
`assign_releves()` will not work unless you recompute the Cocktail
object with `save_vegmatrix = TRUE`.

### Evidence for LLM-assisted labeling (experimental)

`cluster_evidence()` does not call an LLM. Instead, it collects a
deterministic fact bundle for one cluster: merge metrics, topological
species, φ-ranked species (if available), member plots,
prototype/borderline plots, optional cover summaries, topology, and
explicit limitations.

This object is intended as the handoff layer between `cocktailr` and a
later `llm_label_cluster()`-style workflow, but it is also useful on its
own for inspection and debugging.

This example assumes that `res` already exists from the toy workflow
chunk above. In a fresh session, run the `vm <- matrix(...)` and
`res <- cocktail_cluster(...)` example first.

``` r
ev <- cluster_evidence(
  x                  = res,
  cluster            = "c_1",
  top_n_phi          = 5,
  n_prototype_plots  = 3,
  n_borderline_plots = 2
)

names(ev)
#> [1] "meta"        "context"     "summaries"   "evidence"    "limitations"
#> [6] "future"
ev$context$cluster_metrics
#> $h
#> [1] 0.745356
#>
#> $k
#> [1] 2
#>
#> $m
#> [1] 2
#>
#> $evidence_ids
#>    h    k    m
#> "E1" "E2" "E3"
ev$summaries$species_phi
#>   species       phi evidence_id
#> 1     sp4 1.0000000          E9
#> 2     sp3 0.7453560         E10
#> 3     sp1 0.1490712         E11
ev$summaries$plots_prototype[, c("plot", "support_score")]
#>    plot support_score
#> 1 plot1     0.7863248
#> 2 plot2     0.7659864
#> 3 plot3     0.7276786
head(names(ev$evidence$items))
#> [1] "E1" "E2" "E3" "E4" "E5" "E6"

print(ev)
#> Cluster evidence for c_1
#>   h = 0.745
#>   k = 2
#>   m = 2
#>   plot_values = rel_cover
#>   parent = c_6
#>   topological species (2): sp3, sp4
#>   phi species (3): sp4=1.000, sp3=0.745, sp1=0.149
#>   member plots = 5
#>   evidence records = 21
```

What to inspect first:

- `ev$context$cluster_metrics` for `h`, `k`, and `m`
- `ev$summaries$species_topological` for the cluster-constituting
  species
- `ev$summaries$species_phi` for ranked fidelity evidence when
  `Species.cluster.phi` is available
- `ev$summaries$plots_prototype` for the strongest example plots
- `ev$limitations` for missing components or explicit caution notes
- `ev$evidence$items` for machine-addressable fact records (`E1`, `E2`,
  ...)

If the Cocktail object was built with `species_cluster_phi = FALSE`, the
φ summary is omitted and the limitation is recorded explicitly. If it
was built with `save_vegmatrix = FALSE`, cover summaries are omitted and
prototype/borderline support falls back to `Plot.cluster` values only.

### Long-format input

`cocktail_cluster()` also supports **long-format** vegetation tables
when `input_format = "long"`.

The default long-format column names are:

- `plot` — plot / relevé ID
- `species` — species name
- `value` — numeric cover / abundance value

Below, we convert the same toy matrix `vm` to long format and run
`cocktail_cluster()`.

``` r
library(dplyr)
library(tidyr)
library(tibble)

# Convert the toy wide matrix to long format
vm_long <- as.data.frame(vm) %>%
  rownames_to_column(var = "plot") %>%
  pivot_longer(
    cols = -plot,
    names_to = "species",
    values_to = "value"
  )

vm_long
#> # A tibble: 64 × 3
#>    plot  species value
#>    <chr> <chr>   <dbl>
#>  1 plot1 sp1        60
#>  2 plot1 sp2        50
#>  3 plot1 sp3        40
#>  4 plot1 sp4        30
#>  5 plot1 sp5         5
#>  6 plot1 sp6         0
#>  7 plot1 sp7        10
#>  8 plot1 sp8         0
#>  9 plot2 sp1        55
#> 10 plot2 sp2        45
#> # ℹ 54 more rows

# Run Cocktail clustering on long-format input
res_long <- cocktail_cluster(
  vegmatrix    = vm_long,
  input_format = "long",
  progress     = FALSE
)

# Same output structure as for wide input
names(res_long)
#> [1] "Cluster.species"     "Cluster.info"        "Plot.cluster"       
#> [4] "Cluster.merged"      "Cluster.height"      "Species.cluster.phi"
#> [7] "species"             "plots"               "vegmatrix"
```

The returned Cocktail object also stores the internally used, aligned
vegetation matrix in `res_long$vegmatrix` (as a sparse `dgCMatrix`) by
default (`save_vegmatrix = TRUE`).

When using long-format input, always set `input_format = "long"`
explicitly.

If your long-format table uses different column names, provide a mapping
via `long = list(...)`:

``` r
res_long2 <- cocktail_cluster(
  vegmatrix    = my_long_table,
  input_format = "long",
  long = list(
    plot    = "PlotObservationID",
    species = "Harmonized_name_wfo",
    value   = "Relative_cover"
  ),
  progress     = FALSE
)
```

### Notes on long-format input

- Duplicate plot–species rows are aggregated using `sum()` and capped at
  100.
- Empty plots (all zero after parsing and aggregation) are dropped
  automatically.
- The function issues warnings when duplicates are detected or empty
  plots are removed.

------------------------------------------------------------------------

### 1) Visualize the dendrogram

``` r
cocktail_plot(
  x              = res,
  file           = NULL,       # plot to device
  phi_cut        = 0.25,
  label_clusters = TRUE,
  cex_species    = 0.9
)
```

<img src="man/figures/README-typical-plot-1.png" width="100%" />

------------------------------------------------------------------------

### 2) Select clusters at a φ cut **or** select strongest clusters by score

#### Option A: Parent clusters at a φ cut

``` r
phi_cut <- 0.25

parent_labels <- clusters_at_cut(
  x         = res,
  phi_cut   = phi_cut,
  as_labels = TRUE
)

parent_labels
#> [1] "c_1" "c_2" "c_4"
```

#### Option B: Strong clusters by score (merge φ × log(k) × log(m))

``` r
strong_labels <- select_clusters(
  x         = res,
  min_phi   = 0.20,
  min_k     = 1,
  min_score = 0.3,
  mode      = "strict",
  return    = "labels"
)

strong_labels
#> [1] "c_4" "c_1" "c_2"
```

(If you want the full score table:)

``` r
strong_table <- select_clusters(
  x         = res,
  min_phi   = 0.20,
  min_k     = 1,
  min_score = 0.3,
  mode      = "strict",
  return    = "table"
)

strong_table
#>   cluster         h k m     score
#> 3     c_4 0.4879500 3 3 0.5889308
#> 1     c_1 0.7453560 2 2 0.3581085
#> 2     c_2 0.6546537 2 2 0.3145303
```

------------------------------------------------------------------------

### 3) Cluster diagnostics (helper functions)

These two helpers allow quick inspection of clusters and combinations of
clusters.

#### (a) List plots belonging to clusters or combinations of clusters

``` r
# Example: plots belonging to the first and third parent clusters
releves_A <- releves_in_clusters(res, clusters = parent_labels[1])
releves_B <- releves_in_clusters(res, clusters = parent_labels[3])

releves_A
#> $c_1
#> [1] "plot1" "plot2" "plot3" "plot4" "plot8"
releves_B
#> $c_4
#> [1] "plot3" "plot5" "plot6" "plot7" "plot8"
```

Combinations of clusters (list input): plots belonging to *any cluster
in the set*:

``` r
cluster_set_example <- list(
  set1 = parent_labels[1:2]
)

releves_combined <- releves_in_clusters(res, clusters = cluster_set_example)
releves_combined
#> $g_1_2
#> [1] "plot1" "plot2" "plot3" "plot4" "plot5" "plot6" "plot8"
```

#### (b) Find clusters that contain a given species (or set of species)

``` r
# Clusters that contain sp1 (default: only clusters with merge phi >= 0 are returned)
clusters_with_species(res, species = "sp1")
#> [1] "c_2" "c_6"

# Clusters that contain both sp1 and sp2,
# restricted to clusters with merge phi >= 0.2
cl <- clusters_with_species(
  res,
  species = c("sp1", "sp2"),
  match   = "all",
  min_phi = 0.2
)

cl
#> [1] "c_2"

# Highlight clusters that contain both sp1 and sp2
cocktail_plot(
  x              = res,
  file           = NULL,
  clusters       = cl,
  label_clusters = TRUE,
  cex_species    = 0.9
)
```

<img src="man/figures/README-typical-clusters-with-species-1.png" width="100%" />

------------------------------------------------------------------------

### 4) Diagnostic species for selected clusters

Cluster-constituting species sets:

``` r
diag_sp_member <- species_in_clusters(
  x      = res,
  labels = parent_labels
)

diag_sp_member
#> $c_1
#> [1] "sp3" "sp4"
#> 
#> $c_2
#> [1] "sp1" "sp2"
#> 
#> $c_4
#> [1] "sp5" "sp6" "sp8"
```

With φ-based filtering/ranking (uses `Species.cluster.phi`):

``` r
diag_sp_phi <- species_in_clusters(
  x                   = res,
  labels              = parent_labels,
  species_cluster_phi = TRUE,
  min_phi             = 0.20
)

diag_sp_phi
#> $c_1
#>   species      phi
#> 1     sp4 1.000000
#> 2     sp3 0.745356
#> 
#> $c_2
#>   species       phi
#> 1     sp1 1.0000000
#> 2     sp2 0.6546537
#> 
#> $c_4
#>   species     phi
#> 1     sp8 1.00000
#> 2     sp5 0.48795
#> 3     sp6 0.48795
```

------------------------------------------------------------------------

### 5) Distances between clusters (direct plot co-membership φ)

`cluster_phi_dist()` computes distances between clusters using the φ
coefficient between their **binary plot-membership vectors** (membership
= `Plot.cluster > 0`). Distance is always: `d(A,B) = 1 - phi(A,B)`.

We must specify which clusters to compare (e.g. selected strong
clusters):

``` r
clusters_for_dist <- select_clusters(
  x         = res,
  min_score = 0,
  mode      = "strict",
  return    = "labels"
)


D <- cluster_phi_dist(
  x        = res,
  clusters = clusters_for_dist
)

D
#>           c_1       c_2
#> c_2 0.8509288          
#> c_4 1.6000000 1.4472136
```

Hierarchical clustering of clusters:

``` r
hc_clusters <- hclust(D, method = "average")

plot(hc_clusters, main = "Cluster dendrogram (co-membership phi distance)", cex = 0.7)
```

<img src="man/figures/README-typical-phi-dist-hclust-1.png" width="100%" />

``` r

grp_clusters <- cutree(hc_clusters, k = 2)
table(grp_clusters)
#> grp_clusters
#> 1 2 
#> 2 1

# Cluster combinations: group -> vector of cluster labels ("c_12", ...)
cluster_groups <- split(names(grp_clusters), grp_clusters)
cluster_groups
#> $`1`
#> [1] "c_1" "c_2"
#> 
#> $`2`
#> [1] "c_4"
```

------------------------------------------------------------------------

### 6) Visualize grouped clusters on the Cocktail dendrogram

You can pass combinations of clusters (`cluster_groups`) to
`cocktail_plot()`:

``` r
cocktail_plot(
  x              = res,
  clusters       = cluster_groups,
  label_clusters = TRUE,
  cex_species    = 0.9
)
```

<img src="man/figures/README-typical-cocktail-groups-1.png" width="100%" />

------------------------------------------------------------------------

### 7) Assign plots (relevés) to candidate vegetation units

`assign_releves()` assigns each plot to one of the provided candidate
vegetation units using the vegetation matrix stored inside the Cocktail
object (`x$vegmatrix`). This matrix is created by `cocktail_cluster()`
when `save_vegmatrix = TRUE` (default). If the Cocktail object was
created with `save_vegmatrix = FALSE`, `assign_releves()` will stop with
an error.

Strategies:

- `"count"` – number of candidate species present
- `"cover"` – summed cover of candidate species
- `"phi"` – sum of φ weights over candidate species
- `"phi_cover"` – sum of cover × φ over candidate species

Candidate species logic:

- Candidate species are the cluster-constituting species from
  `Cluster.species`.
- For `"phi"` and `"phi_cover"`:
  - `min_phi = NULL` (default): candidate species are the
    cluster-constituting species.
  - `min_phi = 0.2` (example): candidate species are taken from the
    **full fidelity profiles** (`Species.cluster.phi`), using species
    with φ ≥ `min_phi`.

Membership restriction:

- `plot_membership = TRUE` (default): only candidate vegetation units
  for which the plot is already a Cocktail member compete.
- `plot_membership = FALSE`: all candidate vegetation units compete for
  all plots.

Example: assign plots to the parent clusters at `phi_cut = 0.25`.  
The assignment `"+"` appears when a plot has an unresolved tie (two or
more candidate vegetation units share the same best score):

``` r
assign_count <- assign_releves(
  x               = res,
  strategy        = "count",
  clusters        = parent_labels,
  plot_membership = TRUE,
  min_group_size  = 1L
)

table(assign_count)
#> assign_count
#>   + u_4 
#>   3   5
```

Example: assign to candidate vegetation units represented by cluster
combinations defined from cluster-distance grouping:

``` r
assign_combined <- assign_releves(
  x               = res,
  strategy        = "phi_cover",
  clusters        = cluster_groups,
  plot_membership = TRUE,
  min_phi         = 0.20,
  min_group_size  = 1L
)

table(assign_combined)
#> assign_combined
#> u_1_2   u_4 
#>     4     4
```

The returned object is a named character vector (names = plot IDs). Each
value is:

- a candidate vegetation unit label such as `"u_5"` for a unit defined
  by a single cluster, or `"u_5_12"` for a unit represented by several
  clusters;
- `"+"` when there is an unresolved tie between candidate vegetation
  units;
- `"-"` for candidate vegetation units that were collapsed because they
  contain fewer plots than `min_group_size`;
- `NA` when no candidate vegetation unit wins for that plot (all scores
  are 0 or no eligible unit exists).

------------------------------------------------------------------------

### (Optional) Attach assignments to a header data frame

The following example creates a simple header table with plot IDs and
adds multiple assignment strategies as new columns:

``` r
library(dplyr)

# Example header table with plot IDs matching the names of rel_assigned
hea <- data.frame(
  releve_number = rownames(vm)
)

strategies <- c("count", "cover", "phi", "phi_cover")

hea2 <- hea

# Choose one candidate-unit definition:
# parent_labels <- clusters_at_cut(res, phi_cut = 0.25, as_labels = TRUE)

for (s in strategies) {
  rel_assigned <- assign_releves(
    x               = res,
    strategy        = s,
    clusters        = parent_labels,
    plot_membership = TRUE,
    min_phi         = 0.20,
    min_group_size  = 2
  )

  # Align by releve_number using the names of rel_assigned
  idx <- match(as.character(hea2$releve_number), names(rel_assigned))

  colname <- paste0("unit_", s)
  hea2[[colname]] <- rel_assigned[idx]
}

dplyr::glimpse(hea2)
#> Rows: 8
#> Columns: 5
#> $ releve_number  <chr> "plot1", "plot2", "plot3", "plot4", "plot5", "plot6", "…
#> $ unit_count     <chr> "+", "+", "u_4", "+", "u_4", "u_4", "u_4", "u_4"
#> $ unit_cover     <chr> "u_2", "u_2", "u_2", "u_2", "u_4", "u_4", "u_4", "u_4"
#> $ unit_phi       <chr> "u_1", "u_1", "u_4", "u_1", "u_4", "u_4", "u_4", "u_4"
#> $ unit_phi_cover <chr> "u_2", "u_2", "u_2", "u_2", "u_4", "u_4", "u_4", "u_4"
```

------------------------------------------------------------------------

## Function help

See function help for details:

- `?cocktail_cluster` – build the Cocktail tree, optionally with
  species–cluster phi
- `?cocktail_plot` – draw dendrograms (PDF/PNG or current device)
- `?clusters_at_cut` – parent clusters at a phi cut
- `?select_clusters` – select strong clusters by score
- `?species_in_clusters` – diagnostic species for clusters or
  combinations of clusters
- `?releves_in_clusters` – list plots belonging to clusters or
  combinations of clusters
- `?clusters_with_species` – find clusters containing species
- `?cluster_phi_dist` – distances between clusters (direct co-membership
  φ)
- `?assign_releves` – assign plots to candidate vegetation units using
  covers and φ

- `?generate_synthetic_vegetation_data` - create synthetic vegetation
  datasets and matching truth tables for labeling experiments
- `?cluster_evidence` - collect deterministic evidence for one cluster
  before LLM labeling or interpretation

------------------------------------------------------------------------

© 2026 Denys Vynokurov & Helge Bruelheide. Licensed under MIT.
