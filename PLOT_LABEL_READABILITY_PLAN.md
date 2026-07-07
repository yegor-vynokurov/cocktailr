# Plan: Improve Plot Label Readability

## Context

Current image output shows two separate readability problems:

1. `cocktail_plot()` puts the human-readable cluster legend in the top margin,
   which becomes cramped and hard to scan on dense pages.
2. `cluster_hclust_plot()` writes long leaf labels directly onto the dendrogram,
   so labels collide and become unreadable when many clusters are shown.

The concrete examples are:

- `temp/reports/forest_steppe_single_model_labeling/2026-07-03_145147/plots/phi4-mini_latest_full_cocktail_plot_page01.png`
- `temp/reports/forest_steppe_single_model_labeling/2026-07-03_145147/plots/phi4-mini_latest_cluster_hclust_labels.png`

## Main idea

Treat the two plot types differently:

- For `cocktail_plot()`, keep numeric cluster IDs on the dendrogram and move the
  label legend into a dedicated bottom area, ideally in 2 columns.
- For `cluster_hclust_plot()`, keep direct leaf replacement, but generate a
  dedicated compact label field in the registry that:
  - starts with the cluster number,
  - is shorter than `legend_label`,
  - is safer for dense dendrogram leaves.

This avoids overloading one label field for every use case.

## Step-by-step plan

### Step 1. Audit current plotting and label flow

Goal:

- confirm where each label string is created and where it is drawn.

Files to inspect / use:

- `R/cocktail_plot.R`
- `R/cocktail_plot_label_utils.R`
- `R/cluster_hclust_plot.R`
- `R/label_hclust_leaves.R`
- `R/cluster_label_registry.R`

Expected result:

- a precise map of:
  - where top legend lines are assembled for `cocktail_plot()`,
  - where `hclust` leaf labels are selected,
  - which registry fields are safe to change and which are shared by other code.

### Step 2. Add a dedicated compact `hclust` label field to the registry

Goal:

- avoid using raw `plot_label_short` or full `legend_label` for dense `hclust`
  plots.

Idea:

- extend `cluster_label_registry()` with a new field such as
  `hclust_label_compact` or `plot_label_hclust`.
- format it as:
  - `"c_288: Open places with moderate sunlight"`
  - not just `"Open places with moderate sunlight"`
- apply controlled truncation there, for example:
  - prefix always preserved,
  - label text trimmed to a configurable character budget,
  - optional `...` suffix.

Why this is safer:

- `plot_label_short` is currently used more broadly and may be expected to stay
  human-friendly without forced numeric prefixes.
- a dedicated field lets `hclust` become readable without changing other plot
  conventions.

Files:

- `R/cluster_label_registry.R`
- `tests/testthat/test-cluster-label-registry.R`

Acceptance checks:

- every `hclust` label begins with `c_<id>:`
- abstained clusters still get a reasonable compact fallback
- speculative markers are preserved if needed

### Step 3. Switch `cluster_hclust_plot()` to the compact field by default

Goal:

- make one-call `cluster_hclust_plot()` readable without extra user work.

Idea:

- change the default `label_field` in `cluster_hclust_plot()` from
  `"plot_label_short"` to the new compact `hclust` field.
- keep `label_hclust_leaves()` flexible, so advanced users can still request
  `legend_label`, `display_label`, or another registry column manually.

Files:

- `R/cluster_hclust_plot.R`
- `R/label_hclust_leaves.R`
- `man/cluster_hclust_plot.Rd`
- `man/label_hclust_leaves.Rd`
- `tests/testthat/test-cocktail-plot.R`

Acceptance checks:

- `cluster_hclust_plot()` output labels start with cluster numbers by default
- low-level manual override still works
- existing explicit calls with `label_field = ...` keep working

### Step 4. Increase `cluster_hclust_plot()` height and expose denser-label defaults

Goal:

- give leaf labels more vertical room before changing typography too much.

Idea:

- increase default `height_in` for `cluster_hclust_plot()`
  from `7` to something like `9` or `10`.
- consider slightly smaller default text or recommend `cex` scaling in docs,
  but only after adding compact labels.

Files:

- `R/cluster_hclust_plot.R`
- `tests/testthat/test-cocktail-plot.R`

Acceptance checks:

- saved PNG/PDF outputs are visibly taller
- labels remain readable for the same cluster selection that currently fails

### Step 5. Move the `cocktail_plot()` legend from the top margin to a bottom panel

Goal:

- stop using stacked `mtext()` lines in the top margin for page-level cluster
  label legend content.

Idea:

- replace the current top-margin caption approach with a two-region layout:
  - upper region: dendrogram,
  - lower region: legend/caption block.
- likely implementation routes:
  - `layout()` with plot + caption rows, or
  - `par(fig = ...)` with a dedicated text-only region.

Preferred direction:

- reserve a bottom strip for legend text, because it is more stable than
  stretching top margins and makes 2-column layout easier.

Files:

- `R/cocktail_plot.R`
- `R/cocktail_plot_label_utils.R`
- `tests/testthat/test-cocktail-plot.R`

Acceptance checks:

- the dendrogram title remains readable
- the legend no longer overlaps the plot or consumes extreme top margin
- multi-page output still works

### Step 6. Render the `cocktail_plot()` legend in 2 columns

Goal:

- improve scanability when there are many labeled clusters on one page.

Idea:

- extend `.cocktail_plot_legend_lines()` or add a new formatter that returns a
  structured legend table instead of a flat text vector.
- draw entries in two columns in the bottom panel:
  - left column and right column balanced by row count,
  - review root line possibly kept as a separate header line above both columns.

Possible rules:

- if there are 1-4 entries, keep one column
- if there are 5+ entries, use 2 columns
- keep the speculative `*` note as a final full-width line

Files:

- `R/cocktail_plot_label_utils.R`
- `R/cocktail_plot.R`
- `tests/testthat/test-cocktail-plot.R`

Acceptance checks:

- legend entries are easier to scan than the current stacked lines
- the common review folder line still appears
- truncation / overflow is controlled for long legend text

### Step 7. Add regression coverage for label-prefix and compact-label behavior

Goal:

- make sure the readability fixes stay stable.

Tests to add or update:

- `cluster_label_registry()` produces the new compact `hclust` field
- compact `hclust` labels always begin with cluster number
- `cluster_hclust_plot()` uses the compact field by default
- `label_hclust_leaves()` still supports manual `label_field` overrides
- `cocktail_plot()` legend formatting produces one-column and two-column layouts
- page-level legend behavior remains stable with speculative labels and
  abstained clusters

Files:

- `tests/testthat/test-cluster-label-registry.R`
- `tests/testthat/test-cocktail-plot.R`

### Step 8. Re-render the real report artifacts and compare before/after

Goal:

- validate the changes on the same real outputs that exposed the problem.

Artifacts to regenerate:

- `phi4-mini_latest_cluster_hclust_labels.png`
- `phi4-mini_latest_full_cocktail_plot_page01.png`

Comparison checklist:

- `cluster_hclust_plot()` labels now start with cluster number
- leaf labels are shorter and more readable
- `cocktail_plot()` legend is visually separated from the dendrogram
- bottom legend remains readable even with many entries
- page height feels sufficient without wasting too much whitespace

## Recommended implementation order

1. Add the compact `hclust` registry field
2. Switch `cluster_hclust_plot()` default to it
3. Increase `cluster_hclust_plot()` height
4. Rework `cocktail_plot()` legend placement to a bottom panel
5. Add 2-column legend layout
6. Update tests
7. Re-render the real images and tune defaults

## Safe default choices

If I were implementing this now, I would start with these concrete defaults:

- new registry field:
  `hclust_label_compact = "c_<id>: <trimmed short label>"`
- `cluster_hclust_plot(height_in = 9 or 10)` by default
- `cluster_hclust_plot(label_field = "hclust_label_compact")` by default
- `cocktail_plot()` bottom legend panel with:
  - 1 column for short lists,
  - 2 columns for denser pages,
  - separate full-width note for speculative markers

## Important compatibility note

The safest path is to add a new registry field rather than rewriting
`plot_label_short` globally. That keeps existing downstream code stable while
allowing `hclust` readability to improve immediately.
