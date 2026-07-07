# Step 1 Audit: Plotting and Label Flow

This note records the current label flow for the two plot paths named in
`PLOT_LABEL_READABILITY_PLAN.md`.

## `cocktail_plot()`: where legend lines are assembled and drawn

Current behavior is split into two layers:

1. The dendrogram itself always draws numeric cluster IDs.
2. Human-readable labels are added later as page-level caption lines in the top
   margin.

Concrete flow:

- `R/cocktail_plot.R:229-242`
  `.page_caption_lines()` takes the cluster IDs actually drawn on the current
  page, subsets the registry, and delegates text assembly to
  `.cocktail_plot_legend_lines()`.
- `R/cocktail_plot_label_utils.R:155-170`
  `.cocktail_plot_registry_page_subset()` converts numeric page IDs back to
  `c_<id>` registry keys and keeps only rows present in the registry.
- `R/cocktail_plot_label_utils.R:228-271`
  `.cocktail_plot_legend_lines()` builds the top legend content. It currently:
  - adds a `"Label reviews: .../"` header line,
  - appends one line per visible cluster using `legend_label` plus the review
    filename,
  - appends an overflow line when `max_entries` is exceeded,
  - appends a speculative-marker note when needed.
- `R/cocktail_plot.R:244-267`
  `.draw_page_header()` renders the page title and each legend line with
  `graphics::mtext(..., side = 3)`, so the legend lives in the top margin.
- `R/cocktail_plot.R:519-521`
  When both `label_registry` and `label_clusters` are active, the plot reserves
  extra top margin with `par(mar = c(8, 5, 8, 1), ...)`.
- `R/cocktail_plot.R:670,740,773,844,911`
  `draw_page()` accumulates `page_label_ids` from the cluster IDs that were
  actually drawn on the page.
- `R/cocktail_plot.R:680,749,782,854,920`
  The dendrogram annotations themselves are still numeric IDs, drawn with
  `graphics::text(..., labels = <integer cluster id>)`.
- `R/cocktail_plot.R:931-932,948-949`
  After `draw_page()` finishes, `cocktail_plot()` calls `.draw_page_header()`,
  which is where the human-readable legend finally appears.

Conclusion:

- The top legend problem is isolated to the page-header path, not to the
  dendrogram label geometry.
- Moving the legend to a bottom panel can happen without changing the numeric
  ID logic inside `draw_page()`.

## `cluster_hclust_plot()`: where leaf labels are selected and drawn

Current behavior is the opposite of `cocktail_plot()`:

1. The leaf text is replaced directly on the `hclust` object.
2. Whatever ends up in `hc$labels` is what base R draws on the dendrogram.

Concrete flow:

- `R/cluster_hclust_plot.R:75-94`
  `cluster_hclust_plot()` defaults to `label_leaves = TRUE`,
  `label_registry = "auto"`, and `label_field = "plot_label_short"`.
- `R/cluster_hclust_plot.R:150-164`
  After building `dist` and `hclust`, the wrapper resolves the registry and
  passes control to `label_hclust_leaves()`.
- `R/label_hclust_leaves.R:77-108`
  `label_hclust_leaves()`:
  - reads current leaf labels,
  - normalizes the source labels back to `c_<id>` keys,
  - matches them against `reg$cluster`,
  - replaces each matched leaf with `reg[[label_field]]` when the value is
    non-empty.
- `R/label_hclust_leaves.R:113-129`
  If replacement fails, fallback stays configurable:
  - `"keep"` leaves the original label unchanged,
  - `"cluster"` falls back to normalized `c_<id>` labels.
- `R/label_hclust_leaves.R:132-134`
  The modified strings are written back into `hc$labels`, while original labels
  are preserved in `attr(hc, "cocktailr_original_labels")`.
- `R/cluster_hclust_plot.R:219-245`
  `.draw_cluster_hclust_plot()` simply calls
  `graphics::plot(stats::as.dendrogram(hc), ...)`, so the final leaf text is
  whatever `label_hclust_leaves()` placed into `hc$labels`.

Conclusion:

- The dense-leaf readability problem is controlled almost entirely by the
  registry field chosen through `label_field`.
- A dedicated compact field is the cleanest way to improve default output
  without changing the mechanics of `label_hclust_leaves()`.

## Registry fields: what is created where, and what is safe to change

Registry construction:

- `R/cluster_label_registry.R:157-228`
  `.empty_cluster_label_registry()` defines the persisted registry schema.
- `R/cluster_label_registry.R:232-339`
  `.cluster_label_registry_row()` computes one row per cluster.
- `R/cluster_label_registry.R:304-319`
  The two current plot-facing fields are created here:
  - `plot_label_short`
  - `legend_label`
- `R/cluster_label_registry.R:455-480`
  `.cluster_label_registry_legend_label()` formats `legend_label` as
  `c_<id>: <text>` and also handles abstained / placeholder fallbacks.

Current shared contracts:

- `plot_label_short`
  - Produced as a generic short human-readable label in
    `R/cluster_label_registry.R:304-311`.
  - Used as the default `label_field` in both
    `R/cluster_hclust_plot.R:86` and `R/label_hclust_leaves.R:65`.
  - Asserted directly in tests:
    `tests/testthat/test-cluster-label-registry.R:224-248`,
    `tests/testthat/test-cluster-label-registry.R:315-320`,
    `tests/testthat/test-cluster-label-registry.R:421-428`,
    `tests/testthat/test-cocktail-plot.R:353`,
    `tests/testthat/test-cocktail-plot.R:396-403`,
    `tests/testthat/test-cocktail-plot.R:506`.
- `legend_label`
  - Produced centrally by
    `R/cluster_label_registry.R:312-319` and
    `R/cluster_label_registry.R:455-480`.
  - Required by `cocktail_plot()` registry normalization in
    `R/cocktail_plot_label_utils.R:22-30`.
  - Rendered by the page legend assembler in
    `R/cocktail_plot_label_utils.R:246-255`.
  - Also supported as a manual `label_hclust_leaves()` override, with tests in
    `tests/testthat/test-cocktail-plot.R:386-393`.

Safe vs risky change surface:

- Safe to add a new registry column:
  - `label_hclust_leaves()` already accepts any `label_field` present in the
    registry (`R/label_hclust_leaves.R:85-91`).
  - `.read_cluster_label_registry_file()` reads the CSV generically and does not
    enforce an exact fixed column set (`R/cluster_label_registry.R:140-154`).
  - `cocktail_plot()` only requires `cluster`, `legend_label`, and
    `review_file` for its legend path (`R/cocktail_plot_label_utils.R:22-30`).
- Risky to repurpose `plot_label_short` globally:
  - It is the current default for `cluster_hclust_plot()`.
  - Tests already treat it as a general human-friendly label, not a special
    dense-dendrogram format.
- Risky to repurpose `legend_label` globally:
  - It feeds the page legend in `cocktail_plot()`.
  - It remains a supported manual override for direct leaf replacement.

## Step 2 implications

The audit supports the direction already proposed in the plan:

- Add a new registry field rather than changing `plot_label_short` semantics.
- Keep `legend_label` focused on legend/caption use.
- Switch `cluster_hclust_plot()` defaults later by changing only the default
  `label_field`, not the label-replacement algorithm.
