# LLM Evidence Translator Plan

## Goal

Rework the model-facing cluster evidence so that:

- raw evidence IDs like `[E7]` do not appear in LLM prompts;
- `Topological species` is replaced with plain ecological wording;
- each species is rendered together with numeric ecological values and fixed dictionary-based explanations;
- the prompt does not add habitat interpretation beyond what the numbers say;
- the translation layer is user-editable through a simple dictionary file in the repo.

This plan is intentionally conservative. It translates scientific shorthand into fixed English phrases, but it does not add habitat guesses such as `steppe`, `dry meadow`, `alpine grassland`, or similar.

## Confirmed Current-State Facts

- In the current pipeline, values in square brackets after species are evidence IDs such as `[E7]`, `[E78]`, not species counts and not ordinal numbering.
- In the current `c_475` sample, the prompt contains lines like:
  - `Metrics: h=0.371, k=71, m=6`
  - `Topological species: Achillea nobilis [E7]; ...`
- `k=71` means the cluster core contains 71 species.
- `m=6` means a plot must contain at least 6 cluster species to count as a member of the cluster.
- `Plot membership: n=71` means the cluster has 71 member plots.
- Species-level ecological values are already available from the existing EIVE/Tichy lookup layer.
- Species-level cover and frequency are already available from the existing cover summary layer.

## Design Rules

- Keep review/debug output and LLM output separate.
- Keep evidence IDs in review/debug output, but remove them from model-facing prompt text.
- Do not present a raw species list first and a separate enrichment block later.
- Render each included species as one enriched line.
- Every numeric quantity in the model-facing prompt must declare either:
  - a denominator;
  - an explicit numeric scale;
  - or both.
- Do not append habitat interpretation sentences such as:
  - `This is a strong signal of steppe habitat.`
  - `This points to dry calcareous grassland.`
- Use only fixed dictionary phrases tied to numeric ranges.
- Make the dictionary editable by the user without changing R code.

Allowed prompt patterns:

- `45 of 71 member plots (63%)`
- `7.82/10`
- `71 out of 535 recorded species in the dataset (13.3%)`
- `4.18/100 on the original percentage-cover scale`

Forbidden bare numeric patterns:

- `freq=0.63`
- `mean_cover=4.18`
- `k=71` without a denominator
- `n=71` without a denominator
- `0.34` without a scale label

## Proposed New Files

- `inst/extdata/llm_axis_dictionary_v1.csv`
  - user-editable dictionary for axis bins;
  - one row per axis and numeric interval;
  - contains the exact English phrase to inject into prompts.
- `inst/extdata/llm_metric_dictionary_v1.csv`
  - optional fixed phrasing for cluster metrics;
  - can start small if we only need `h`, `k`, `m`, and `n`.
- `R/cluster_evidence_quantity_context.R`
  - helper for denominators, percentages, and scale labels;
  - computes dataset totals such as total species and total plots;
  - computes relative shares such as cluster species share and cluster plot share.
- `R/cluster_evidence_llm_render.R`
  - helper functions that read the dictionary and render model-facing evidence text.

If we want to minimize new files, the new render helpers can live in `R/cluster_evidence.R`, but a dedicated file will be easier to maintain.

## Step-by-Step Implementation Plan

### Step 1. Split debug evidence from LLM evidence

Create a separate model-facing serializer instead of reusing the current debug-style text snapshot.

Target:

- current debug/review renderer keeps `[E...]`;
- new LLM renderer removes evidence IDs entirely.

Implementation area:

- `R/cluster_evidence.R`
- `R/render_cluster_review.R`
- new `R/cluster_evidence_llm_render.R`

Exact English replacements in prompts:

- replace `Topological species:` with:
  - `Plants that regularly occur in this cluster:`
- replace `Phi-ranked species:` with:
  - `Species with the strongest cluster association:`
- replace `Plot membership: n=71` with:
  - `This cluster contains 71 member plots.`

### Step 2. Create the dictionary-translator

Create a versioned, user-editable dictionary file that maps numeric ranges to fixed English phrases.

Required property:

- the dictionary must not infer habitat type;
- it must only decode the numeric value itself.

Recommended file format:

`inst/extdata/llm_axis_dictionary_v1.csv`

Recommended columns:

- `axis`
- `lower`
- `upper`
- `label`
- `phrase`

Example row meaning:

- `l,8,9,very_bright,prefers very bright places`

Render rule:

- lookup by axis;
- pick the row where `lower <= value < upper`;
- for the final bin, include the upper bound `10.0`.

### Step 3. Create a quantity-context helper

Create a small service helper that attaches denominators and scales to every numeric value before prompt rendering.

Required outputs:

- `dataset_species_total`
- `dataset_plot_total`
- `dataset_cluster_total`
- `cluster_species_share_pct = 100 * k / dataset_species_total`
- `cluster_plot_share_pct = 100 * n / dataset_plot_total`
- `species_freq_count`
- `species_freq_pct`
- `cover_scale_type`
- `cover_scale_label`
- `cover_scale_bounds`

Rules:

- no model-facing count is allowed without a denominator;
- no model-facing mean value is allowed without a scale label or a normalized reference;
- `k` must be rendered relative to all species in the input table;
- `n` must be rendered relative to all plots in the input table.

Exact English template for cluster-size context:

```text
Dataset context: {dataset_species_total} recorded species, {dataset_plot_total} plots, {dataset_cluster_total} evaluated clusters.
- k={k}: the cluster core contains {k} species out of {dataset_species_total} recorded species in the dataset ({cluster_species_share_pct}% of all recorded species).
- n={n}: this cluster contains {n} member plots out of {dataset_plot_total} plots in the dataset ({cluster_plot_share_pct}% of all plots).
```

### Step 4. Attach cover-scale metadata and normalized cover context

The current bare `mean_cover` value is not self-explanatory across datasets.

We need one helper that decides how cover values can be rendered safely:

- if the source scale is explicitly percentage cover, render raw mean cover with `/100`;
- if the source scale is ordinal or otherwise not self-explanatory, do not show bare raw cover alone;
- add a normalized comparison metric with an explicit denominator.

Recommended additional derived metric:

- `mean_plot_cover_share_pct`
  - mean of `species_cover / total_plot_cover` across all member plots;
  - expressed as a percentage;
  - denominator is explicit: total cover of the plot.

Recommended metadata fields:

- `cover_scale_type`
  - `percentage_cover`
  - `ordinal_numeric_cover`
  - `numeric_unknown_scale`
- `cover_scale_label`
  - exact human-readable label for prompt text
- `cover_scale_min`
- `cover_scale_max`

Exact English templates for cover context:

If the dataset uses percentage cover:

```text
Mean cover {mean_cover}/100 on the original percentage-cover scale, averaged across all {n_member_plots} member plots including zeros where the species is absent.
```

If the dataset scale is not self-explanatory:

```text
Mean share of total plot cover {mean_plot_cover_share_pct}% across all {n_member_plots} member plots.
```

Optional review/debug-only supplement:

```text
Raw mean cover on the original input scale: {mean_cover}.
```

### Step 5. Create a fixed phrase template for each species line

Each species line should be assembled programmatically from frequency, cover, and per-axis dictionary lookup.

Exact English template:

```text
{species}: occurs in {freq_count} of {n_member_plots} member plots ({freq_pct}%). {cover_sentence} Light {l}/10 ({l_phrase}). Moisture {m}/10 ({m_phrase}). Nutrients {n}/10 ({n_phrase}). Reaction {r}/10 ({r_phrase}). Temperature {t}/10 ({t_phrase}). Salinity {s}/10 ({s_phrase}).
```

Rules:

- omit an axis if its value is missing;
- do not render bare `mean_cover` without scale context;
- do not replace an axis with a habitat guess;
- do not append a final summary sentence like `This indicates steppe habitat`.

### Step 6. Create a fixed phrase template for cluster metrics

Cluster metrics should also be translated into plain English, but without overclaiming.

Exact English template:

```text
How to read these cluster metrics:
- h={h}: merge-phi value for this cluster. Lower values mean a looser shared species signal; higher values mean a tighter shared species signal.
- k={k}: the cluster core contains {k} species out of {dataset_species_total} recorded species in the dataset ({cluster_species_share_pct}% of all recorded species).
- m={m}: a plot must contain at least {m} of these {k} species to count as a member of the cluster.
- n={n}: this cluster contains {n} member plots out of {dataset_plot_total} plots in the dataset ({cluster_plot_share_pct}% of all plots).
```

Notes:

- `k` and `n` must stay separate because they refer to different things.
- `h` wording should stay definitional unless we later decide to add a user-editable `h` band dictionary.

### Step 7. Select which species to include in the prompt

We cannot safely dump all species with six axes each into every prompt, especially for large clusters.

Add a deterministic retain policy, for example:

- include top phi-ranked species;
- include dominant species by `mean_cover`;
- include frequent species by `freq_in_member_plots`;
- deduplicate;
- trim to the prompt budget.

The exact selection policy should be fixed in code, not improvised by the model.

### Step 8. Remove raw scientific shorthand from model-facing headings

Replace technical or ambiguous headings with plain wording.

Exact English headings:

- `How to read these cluster metrics:`
- `Plants that regularly occur in this cluster:`
- `Species with the strongest cluster association:`
- `Ecological axis summary for the cluster:`

Avoid these headings in the model-facing prompt:

- `Topological species`
- `Semantic axes`
- `Cover summary`

These names can remain in internal/debug outputs if needed.

### Step 9. Add an editable axis dictionary with 10 bins per 0-10 scale

The current project scale is effectively `0-10`, so the first version of the dictionary should define 10 bins:

- `0.0-0.99`
- `1.0-1.99`
- `2.0-2.99`
- `3.0-3.99`
- `4.0-4.99`
- `5.0-5.99`
- `6.0-6.99`
- `7.0-7.99`
- `8.0-8.99`
- `9.0-10.0`

This gives users a stable place to edit wording later without touching code.

### Step 10. Keep cluster-level ecological summaries numeric and literal

The cluster-level ecological block should also stay literal and dictionary-driven.

Exact English template:

```text
Ecological axis summary for the cluster:
- Light {l}/10 ({l_phrase}).
- Moisture {m}/10 ({m_phrase}).
- Nutrients {n}/10 ({n_phrase}).
- Reaction {r}/10 ({r_phrase}).
- Temperature {t}/10 ({t_phrase}).
- Salinity {s}/10 ({s_phrase}).
```

Do not add a sentence like:

- `Overall this is an open dry steppe-like habitat.`

### Step 11. Preserve evidence IDs only in review/debug artifacts

The review markdown and evidence registry can still keep IDs for provenance and auditing.

Expected behavior:

- review/debug:
  - `Festuca valesiaca (mean_cover=4.18, freq=0.63) [E99]`
- LLM prompt:
  - `Festuca valesiaca: occurs in 45 of 71 member plots (63%). Mean cover 4.18/100 on the original percentage-cover scale, averaged across all 71 member plots including zeros where the species is absent. ...`

### Step 12. Add tests

Add tests for:

- evidence IDs are absent from model-facing text;
- axis phrases come from dictionary lookup, not hardcoded habitat guesses;
- missing axis values are omitted cleanly;
- counts always include denominators;
- `k` is rendered relative to all species in the dataset;
- `n` is rendered relative to all plots in the dataset;
- raw cover values are never shown without an explicit scale label;
- `k` and `n` are rendered as different concepts;
- the same cluster renders deterministically on repeated runs;
- the dictionary can be edited without changing the renderer contract.

## Initial Dictionary Proposal

This section defines the initial fixed English phrases for the first user-editable dictionary version.

Range convention:

- use `lower <= value < upper`;
- for the last row, include `10.0`.

Scale direction:

- higher `Light` means more light-demanding;
- higher `Moisture` means wetter;
- higher `Nutrients` means richer;
- higher `Reaction` means more base-rich and less acidic;
- higher `Temperature` means warmer;
- higher `Salinity` means saltier.

### Light

| Range | Exact English phrase |
| --- | --- |
| 0.0-0.99 | prefers very deep shade |
| 1.0-1.99 | prefers deep shade |
| 2.0-2.99 | prefers strong shade |
| 3.0-3.99 | prefers shade |
| 4.0-4.99 | prefers semi-shaded places |
| 5.0-5.99 | prefers partly open places |
| 6.0-6.99 | prefers open places |
| 7.0-7.99 | prefers bright places |
| 8.0-8.99 | prefers very bright places |
| 9.0-10.0 | prefers fully open, full-light places |

### Moisture

| Range | Exact English phrase |
| --- | --- |
| 0.0-0.99 | prefers extremely dry conditions |
| 1.0-1.99 | prefers very dry conditions |
| 2.0-2.99 | prefers dry conditions |
| 3.0-3.99 | prefers rather dry conditions |
| 4.0-4.99 | prefers slightly dry conditions |
| 5.0-5.99 | prefers intermediate moisture |
| 6.0-6.99 | prefers slightly moist conditions |
| 7.0-7.99 | prefers moist conditions |
| 8.0-8.99 | prefers very moist conditions |
| 9.0-10.0 | prefers wet conditions |

### Nutrients

| Range | Exact English phrase |
| --- | --- |
| 0.0-0.99 | prefers extremely nutrient-poor soils |
| 1.0-1.99 | prefers very nutrient-poor soils |
| 2.0-2.99 | prefers nutrient-poor soils |
| 3.0-3.99 | prefers rather nutrient-poor soils |
| 4.0-4.99 | prefers moderately nutrient-poor soils |
| 5.0-5.99 | prefers intermediate nutrient levels |
| 6.0-6.99 | prefers moderately nutrient-rich soils |
| 7.0-7.99 | prefers nutrient-rich soils |
| 8.0-8.99 | prefers very nutrient-rich soils |
| 9.0-10.0 | prefers extremely nutrient-rich soils |

### Reaction

| Range | Exact English phrase |
| --- | --- |
| 0.0-0.99 | prefers extremely acidic soils |
| 1.0-1.99 | prefers very acidic soils |
| 2.0-2.99 | prefers acidic soils |
| 3.0-3.99 | prefers rather acidic soils |
| 4.0-4.99 | prefers slightly acidic to near-neutral soils |
| 5.0-5.99 | prefers near-neutral soils |
| 6.0-6.99 | prefers slightly base-rich soils |
| 7.0-7.99 | prefers base-rich soils |
| 8.0-8.99 | prefers very base-rich soils |
| 9.0-10.0 | prefers extremely base-rich soils |

### Temperature

| Range | Exact English phrase |
| --- | --- |
| 0.0-0.99 | typical of extremely cold conditions |
| 1.0-1.99 | typical of very cold conditions |
| 2.0-2.99 | typical of cold conditions |
| 3.0-3.99 | typical of cool conditions |
| 4.0-4.99 | typical of slightly cool conditions |
| 5.0-5.99 | typical of temperate conditions |
| 6.0-6.99 | typical of slightly warm conditions |
| 7.0-7.99 | typical of warm conditions |
| 8.0-8.99 | typical of very warm conditions |
| 9.0-10.0 | typical of extremely warm conditions |

### Salinity

| Range | Exact English phrase |
| --- | --- |
| 0.0-0.99 | shows almost no salinity preference |
| 1.0-1.99 | tolerates very low salinity |
| 2.0-2.99 | tolerates low salinity |
| 3.0-3.99 | tolerates slightly saline conditions |
| 4.0-4.99 | tolerates mildly saline conditions |
| 5.0-5.99 | tolerates moderately saline conditions |
| 6.0-6.99 | tolerates clearly saline conditions |
| 7.0-7.99 | tolerates high salinity |
| 8.0-8.99 | tolerates very high salinity |
| 9.0-10.0 | tolerates extreme salinity |

## Worked Example for `c_475`

This is how the evidence should look in the model-facing prompt after translation.

```text
Cluster: c_475

Dataset context: {dataset_species_total} recorded species, {dataset_plot_total} plots, {dataset_cluster_total} evaluated clusters.

How to read these cluster metrics:
- h=0.371: merge-phi value for this cluster. Lower values mean a looser shared species signal; higher values mean a tighter shared species signal.
- k=71: the cluster core contains 71 species out of {dataset_species_total} recorded species in the dataset ({cluster_species_share_pct}% of all recorded species).
- m=6: a plot must contain at least 6 of these 71 species to count as a member of the cluster.
- n=71: this cluster contains 71 member plots out of {dataset_plot_total} plots in the dataset ({cluster_plot_share_pct}% of all plots).

Plants that regularly occur in this cluster:
- Festuca valesiaca: occurs in 45 of 71 member plots (63%). Mean cover 4.18/100 on the original percentage-cover scale, averaged across all 71 member plots including zeros where the species is absent. Light 7.82/10 (prefers bright places). Moisture 2.25/10 (prefers dry conditions). Nutrients 2.25/10 (prefers nutrient-poor soils). Reaction 7.04/10 (prefers base-rich soils). Temperature 5.23/10 (typical of temperate conditions).
- Clinopodium acinos: occurs in 32 of 71 member plots (45%). Mean cover 0.34/100 on the original percentage-cover scale, averaged across all 71 member plots including zeros where the species is absent. Light 8.68/10 (prefers very bright places). Moisture 2.24/10 (prefers dry conditions). Nutrients 1.54/10 (prefers very nutrient-poor soils). Reaction 6.92/10 (prefers slightly base-rich soils). Temperature 4.66/10 (typical of slightly cool conditions). Salinity 0.00/10 (shows almost no salinity preference).
- Koeleria macrantha: occurs in 34 of 71 member plots (48%). Mean cover 0.35/100 on the original percentage-cover scale, averaged across all 71 member plots including zeros where the species is absent. Light 7.70/10 (prefers bright places). Moisture 2.94/10 (prefers dry conditions). Nutrients 2.14/10 (prefers nutrient-poor soils). Reaction 7.58/10 (prefers base-rich soils). Temperature 4.57/10 (typical of slightly cool conditions). Salinity 0.00/10 (shows almost no salinity preference).
- Arenaria serpyllifolia: occurs in 41 of 71 member plots (58%). Mean cover 0.40/100 on the original percentage-cover scale, averaged across all 71 member plots including zeros where the species is absent. Light 8.15/10 (prefers very bright places). Moisture 3.10/10 (prefers rather dry conditions). Nutrients 4.48/10 (prefers moderately nutrient-poor soils). Reaction 6.42/10 (prefers slightly base-rich soils). Temperature 4.22/10 (typical of slightly cool conditions). Salinity 0.11/10 (shows almost no salinity preference).

Ecological axis summary for the cluster:
- Light 8.04/10 (prefers very bright places).
- Moisture 2.76/10 (prefers dry conditions).
- Nutrients 3.23/10 (prefers rather nutrient-poor soils).
- Reaction 6.98/10 (prefers slightly base-rich soils).
- Temperature 4.95/10 (typical of slightly cool conditions).
- Salinity 0.07/10 (shows almost no salinity preference).
```

Important:

- this example stays literal;
- placeholders such as `{dataset_species_total}` and `{cluster_species_share_pct}` will be filled by code from the actual dataset;
- the `x/100` cover phrasing in this example assumes a dataset whose numeric cover values are on a percentage-cover scale;
- it does not say `steppe`, `dry grassland`, `calcareous grassland`, or any other habitat name;
- it only states the numeric evidence plus fixed dictionary phrases.

## Acceptance Criteria

- A user can open one dictionary file and change the English phrase for any numeric bin.
- The same cluster always produces the same translated prompt text.
- The model-facing prompt contains no evidence IDs like `[E7]`.
- The model-facing prompt contains no separate raw topological-species block.
- No count appears without a denominator or percentage context.
- No raw cover value appears without an explicit scale label.
- Each included species line contains only numeric facts and fixed dictionary phrases.
- No habitat guess is introduced by the translation layer.

## Recommended Implementation Order

1. Add the dictionary CSV file.
2. Add the quantity-context helper for denominators and percentages.
3. Add cover-scale metadata and normalized cover helper.
4. Add lookup helpers for axis phrases.
5. Add model-facing metric renderer.
6. Add model-facing species-line renderer.
7. Add a dedicated LLM evidence serializer.
8. Wire the serializer into the model-facing prompt path only.
9. Keep review/debug render unchanged.
10. Add tests.
11. Run a smoke example on `c_475`.
