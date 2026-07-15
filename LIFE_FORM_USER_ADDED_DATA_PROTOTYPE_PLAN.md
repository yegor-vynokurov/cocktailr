# Plan: Life-Form Prototype Via `user_added_data`

## Goal

Build a fast prototype that injects life-form context into cluster labeling
through the existing `user_added_data` pathway, then compare four labeling
variants on the real dataset:

`D:/documents/coctrailr/cocktailr/data-raw/external/forest_steppe_chytry_2021`

The four run variants are:

1. Plain cluster evidence only.
2. Current ecological enrichment only.
3. Life-form `user_added_data` only.
4. Life-form `user_added_data` plus ecological enrichment.

## Tracking Legend

- `todo` = not started
- `doing` = in progress
- `done` = completed
- `blocked` = waiting on a decision, fix, or external resource

## Status Board

| ID | Stage | Status | Main output |
| --- | --- | --- | --- |
| P0 | Freeze scope, assumptions, and run matrix | `todo` | This plan updated and accepted |
| P1 | Reproduce clustering and cluster selection on the target dataset | `todo` | `res_*.rds`, `selected_clusters.csv` |
| P2 | Inspect `Life_form.xlsx` and define the prototype life-form dictionary | `todo` | Dictionary file and mapping notes |
| P3 | Design cluster-specific life-form text format for `user_added_data` | `todo` | Template for per-cluster `.txt` notes |
| P4 | Implement life-form lookup and text generator | `todo` | Prototype helper code |
| P5 | Implement per-cluster labeling wrapper for `user_added_data` | `todo` | Wrapper or driver script |
| P6 | Add automated tests for parsing, rendering, and prompt injection | `todo` | Test file(s) green locally |
| P7 | Run smoke comparison on one cluster for all four variants | `todo` | Smoke run artifacts |
| P8 | Run full comparison on selected clusters for all four variants | `todo` | Four run folders with summaries and reviews |
| P9 | Compare outputs and document findings | `todo` | Comparison markdown and summary table |
| P10 | Decide whether to promote the prototype into first-class evidence enrichment | `todo` | Go / no-go note |

## Key Constraints And Assumptions

- `cluster_evidence()` already accepts raw `user_added_data` as a file, a
  directory, or an in-memory object.
- `label_clusters()` forwards one `user_added_data` input to each cluster in a
  run. For this prototype, that means cluster-specific life-form notes should
  be injected by labeling one cluster at a time through a wrapper loop rather
  than by one batch call with one shared directory.
- The current raw `user_added_data` character budget is small:
  `1000` characters total. The prototype text must stay compact.
- The life-form dataset is categorical and multi-label, not numeric. It should
  be treated as structural context, not as habitat proof.
- The prototype should reuse the same dataset, clustering result, and selected
  cluster list across all four variants so the comparison stays fair.

## Proposed Output Layout

Use a dedicated report root for this experiment family:

```text
temp/reports/forest_steppe_life_form_prototype/<timestamp>/
  input/
  clustering/
  prototype/
    dictionary/
    life_form_user_added/
  runs/
    plain/
    semantic_only/
    life_forms_only/
    life_forms_plus_semantic/
  evaluation/
  session/
```

## Run Matrix

| Arm | Slug | `semantic_layer` | `user_added_data` | Purpose |
| --- | --- | --- | --- | --- |
| A | `plain` | `FALSE` | `NULL` | Baseline prompt with plain cluster evidence only |
| B | `semantic_only` | `TRUE` | `NULL` | Current ecological enrichment only |
| C | `life_forms_only` | `FALSE` | per-cluster life-form note | Isolate life-form effect |
| D | `life_forms_plus_semantic` | `TRUE` | per-cluster life-form note | Test complementarity |

All other run settings should stay fixed across arms unless a hard blocker
appears:

- same model
- same dataset
- same cluster selection
- same `variant`
- same `label_mode`
- same `use_brainstorm`
- same prompt budget
- same timeout and Ollama options

## Deliverables

- One prototype life-form dictionary file.
- One prototype helper that matches species to life-form rows.
- One prototype helper that writes compact cluster-specific `.txt` notes for
  `user_added_data`.
- One wrapper that runs labeling per cluster so each cluster gets its own
  life-form note.
- Smoke and full run artifacts for all four arms.
- One comparison document with findings, risks, and recommendation.

## Dictionary Design For The Prototype

Start with a compact human-readable mapping from raw life-form flags to short
phrases that a small LLM can use safely.

Initial phrasing target:

| Raw flag | Prototype phrase |
| --- | --- |
| `Tree` | usually a tree; contributes the upper woody layer |
| `Shrub` | usually a shrub; contributes the shrub layer |
| `Phanerophyte` | woody perennial with buds well above the ground |
| `Chamaephyte` | low woody or subwoody plant with buds close to the ground |
| `Semi-shrub` | partly woody plant, often between shrub and herb structure |
| `Dwarf shrub` | low woody shrub, usually close to the ground layer |
| `Hemicryptophyte` | perennial herb with buds at or near the soil surface; part of the herb layer |
| `Geophyte` | perennial herb with underground storage organs; part of the herb layer |
| `Therophyte` | annual herb surviving unfavorable periods as seed; often short-lived or disturbance-tolerant |
| `Hydrophyte` | aquatic plant form |
| `Epiphyte` | plant growing on other plants rather than rooted in soil |
| `Woody liana` | woody climber |
| `Herbaceous liana` | non-woody climber |

Rules for the dictionary:

- Keep each phrase literal and structural.
- Avoid habitat claims in the dictionary itself.
- Allow species to carry multiple forms when the table marks several `1`s.
- Prefer short two-clause sentences over raw jargon.

## Cluster-Level Text Format For `user_added_data`

Generate one compact `.txt` note per cluster. Target size:

- preferred: `500-900` characters
- hard limit: `<= 1000` characters

Recommended structure:

1. One cluster-level structural summary.
2. One short contrast sentence from diagnostic species.
3. Three to six species lines for the strongest or most informative taxa.
4. One caution line that this is structural context only.

Prototype template:

```text
Life-form context for <cluster_id>:
- Recurrent structure: <short summary from frequent species>.
- Diagnostic structure: <short summary from phi-weighted species>.
- <species_1>: <short life-form phrase>.
- <species_2>: <short life-form phrase>.
- <species_3>: <short life-form phrase>.
- Use this as structural context only, not as direct habitat proof.
```

## Recommended Implementation Approach

### P1. Reproduce The Shared Baseline Inputs

- Follow `LABELING_STEP_BY_STEP.md` for the target dataset.
- Rebuild or confirm the clustering object and selected cluster list.
- Save one shared `res_*.rds`.
- Save one shared `selected_clusters.csv`.
- Freeze the exact model and run settings that will be reused across all arms.

Exit condition:

- One common cluster selection exists and is accepted as the comparison base.

### P2. Build The Prototype Dictionary

- Inspect `data-raw/external/life_forms/Life_form.xlsx`.
- Confirm the taxon column and the binary life-form columns.
- Create a prototype dictionary file.
- Recommended location:
  `inst/extdata/life_form_dictionary_v1.csv`
- Include at least:
  `raw_flag`, `display_label`, `phrase`, `priority`

Exit condition:

- One reusable dictionary exists and covers the life-form flags found in the
  workbook.

### P3. Build Species Lookup And Matching

- Reuse the matching philosophy already used by the current semantic layer:
  exact name, alias correction, conservative fallback.
- If possible, reuse existing species alias machinery rather than inventing a
  separate matcher.
- Record unmatched species explicitly for debugging.

Exit condition:

- For a chosen smoke cluster, the helper returns matched life-form rows for the
  key species and a short unmatched list for the rest.

### P4. Build Cluster-Level Life-Form Summaries

- Aggregate frequent species life forms into one cluster-level structural line.
- Aggregate diagnostic species life forms into one second line.
- Select three to six species for short per-species notes.
- Prefer frequent plus phi-diagnostic species over a long exhaustive list.
- Keep output deterministic.

Recommended summaries:

- `recurrent_structure_summary`
- `diagnostic_structure_summary`
- `species_life_form_notes`
- `unmatched_species`

Exit condition:

- One deterministic `.txt` note is produced for a cluster and fits the raw
  `user_added_data` budget.

### P5. Implement Prototype Injection Through `user_added_data`

- Because `label_clusters()` uses one shared `user_added_data` input per run,
  implement a per-cluster wrapper for prototype arms `C` and `D`.
- For each cluster:
  generate the cluster-specific `.txt` note, then call `label_clusters()` for
  that cluster only with the matching `user_added_data`.
- Collect outputs into the same report layout style as ordinary runs.

Recommended prototype artifacts:

- helper code under `R/` or a dedicated prototype script under the project root
- generated `.txt` notes under
  `temp/reports/.../prototype/life_form_user_added/`

Exit condition:

- Arms `C` and `D` can run end-to-end without manually editing per-cluster
  files between calls.

### P6. Add Automated Tests

Minimum automated coverage:

- workbook parser reads the life-form table structure correctly
- matching works for exact names and multi-form species
- dictionary rendering is deterministic
- generated life-form note stays within the character limit
- `user_added_data` content appears in the assembled prompt or prompt logs
- combined run path does not break `semantic_layer = TRUE`

Recommended test themes:

- one tree species
- one shrub species
- one hemicryptophyte
- one geophyte
- one species with multiple active forms
- one unmatched species

Exit condition:

- Tests pass locally for the prototype pieces that do not require a real model
  call.

### P7. Smoke Comparison

Run one-cluster smoke tests for all four arms before the full comparison.

Smoke objectives:

- confirm the arm toggles are wired correctly
- inspect the prompt text for the life-form block
- confirm the life-form note is not truncated unexpectedly
- check whether the small model uses the added structural context sensibly
- verify that semantic enrichment and life-form notes can coexist

Recommended smoke scope:

- first selected cluster
- optionally one woody cluster and one herb-dominated cluster if the first
  result is ambiguous

Exit condition:

- All four smoke runs finish and their review cards and prompt logs are usable.

### P8. Full Comparison

Run the four arms on the same selected cluster list.

For each arm, save:

- `.rds` run object
- summary `.csv`
- review cards
- model logs
- prototype-generated life-form note files when relevant

Exit condition:

- All four arms complete or fail with documented reasons.

### P9. Comparison And Evaluation

Compare the four arms on both quantitative and qualitative criteria.

Quantitative comparison:

- number of successful runs
- number of abstains
- number of accepted labels
- number of duplicated canonical labels
- label diversity across clusters
- number of runs marked for human review
- average prompt size if available from logs

Qualitative comparison:

- does the label better reflect vegetation structure
- do explanations stay evidence-first
- does the model overclaim habitat less or more
- do nearby clusters receive more differentiated labels
- are woody, shrub, and herb-layer distinctions clearer
- do life-form notes help without dominating the prompt

Recommended manual review subset:

- at least 10 clusters
- include easy and ambiguous cases
- include clusters with strong woody signal and clusters with mixed signal

Exit condition:

- One comparison note exists with a recommendation for or against promotion.

### P10. Promotion Decision

Possible outcomes:

- keep as a prototype only
- promote into a first-class enrichment path beside `semantic_layer`
- merge life-form logic into the future semantic evidence layer

Decision criteria:

- measurable labeling improvement
- acceptable prompt cost
- no strong increase in hallucinated habitat claims
- manageable maintenance burden

## Comparison Table Template

Use this table during execution:

| Arm | Run status | Labeled | Abstain | Human review | Duplicate labels | Notes |
| --- | --- | --- | --- | --- | --- | --- |
| A plain | `todo` |  |  |  |  |  |
| B semantic_only | `todo` |  |  |  |  |  |
| C life_forms_only | `todo` |  |  |  |  |  |
| D life_forms_plus_semantic | `todo` |  |  |  |  |  |

## Step Checklist

- [ ] P0 scope frozen
- [ ] P1 shared baseline inputs prepared
- [ ] P2 prototype dictionary created
- [ ] P3 cluster text template finalized
- [ ] P4 life-form generator implemented
- [ ] P5 per-cluster wrapper implemented
- [ ] P6 automated tests passing
- [ ] P7 smoke comparison complete
- [ ] P8 full comparison complete
- [ ] P9 evaluation note written
- [ ] P10 promotion decision recorded

## Execution Log

| Date | Step | Status | Action taken | Output path | Notes / blockers |
| --- | --- | --- | --- | --- | --- |
|  | P0 | `todo` |  |  |  |
|  | P1 | `todo` |  |  |  |
|  | P2 | `todo` |  |  |  |
|  | P3 | `todo` |  |  |  |
|  | P4 | `todo` |  |  |  |
|  | P5 | `todo` |  |  |  |
|  | P6 | `todo` |  |  |  |
|  | P7 | `todo` |  |  |  |
|  | P8 | `todo` |  |  |  |
|  | P9 | `todo` |  |  |  |
|  | P10 | `todo` |  |  |  |

