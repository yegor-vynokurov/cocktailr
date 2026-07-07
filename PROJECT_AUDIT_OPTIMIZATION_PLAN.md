# Project Audit And Optimization Plan

## Purpose

This plan is for auditing and optimizing the current `cocktailr` branch while
respecting one core constraint:

- our LLM/labeling/reporting work should remain an optional extension;
- the original project should stay as intact as possible;
- existing upstream-style functions should be reused as-is whenever that is
  practical;
- changes inside old source files are allowed only at small integration seams.

The plan is written so work can be paused, resumed, and checked off step by
step.

## Working Rules

- Prefer wrappers, adapters, and helper layers over editing original
  implementation.
- A change in an original file is acceptable if it is a narrow seam:
  argument passthrough, provenance capture, optional hook, or doc index entry.
- If a feature needs substantial new behavior inside an old function, do not
  keep expanding that function. Move the behavior into a new helper or a new
  wrapper and keep the old function change minimal.
- If a new feature can exist entirely in new files, it should.
- Comments should explain non-obvious logic, not narrate every line.
- Documentation must reflect the actual current behavior, not intended
  behavior.
- Tests must prove optionality:
  old workflows still work without LLM features turned on.

## Important Packaging Decision

Keep `R/` flat. Do not introduce custom grouping subfolders inside `R/`.

Why:

- In normal R package practice, source files are kept directly under `R/`.
- The official R manual discusses file paths relative to the `R` subdirectory
  and mentions OS-specific subdirectories, not custom feature folders.
- A flat `R/` plus consistent filename prefixes is the safer and more idiomatic
  option for package tooling, review, and future upstream sync.

Reference:

- CRAN, *Writing R Extensions*:
  https://cran.r-project.org/doc/manuals/r-devel/R-exts.html

Practical consequence:

- if we need clearer grouping, we should rename files with consistent prefixes;
- do not rely on `R/llm/`, `R/plot/`, `R/internal/` style folder trees.

## Baseline Snapshot On July 7, 2026

- Branch: `labels_to_imgs`
- Old source snapshot is stored in `temp/R_old/`
- `temp/R_old/` contains 10 original source files
- current `R/` contains 29 source files
- compared with `temp/R_old/`, there are 19 new source files in `R/`

New files compared with `R_old`:

- `R/cluster_evidence.R`
- `R/cluster_evidence_llm_dictionary.R`
- `R/cluster_evidence_llm_render.R`
- `R/cluster_evidence_quantity_context.R`
- `R/cluster_evidence_utils.R`
- `R/cluster_hclust_plot.R`
- `R/cluster_label_display_utils.R`
- `R/cluster_label_io_utils.R`
- `R/cluster_label_prompt_utils.R`
- `R/cluster_label_registry.R`
- `R/cluster_label_utils.R`
- `R/cocktail_plot_label_utils.R`
- `R/generate_synthetic_vegetation_data.R`
- `R/label_clusters.R`
- `R/label_hclust_leaves.R`
- `R/llm_label_cluster.R`
- `R/render_cluster_review.R`
- `R/semantic_layer_indicators.R`
- `R/validate_cluster_label.R`

Original files that currently differ substantively from `temp/R_old`:

- `R/cocktail_cluster.R`
- `R/cocktail_plot.R`
- `R/cocktailr-package.R`

Original files that appear effectively unchanged relative to `temp/R_old`:

- `R/assign_releves.R`
- `R/cluster_phi_dist.R`
- `R/clusters_at_cut.R`
- `R/clusters_with_species.R`
- `R/releves_in_clusters.R`
- `R/select_clusters.R`
- `R/species_in_clusters.R`

Current reproducible test status:

- `tests/testthat/test-render-cluster-review.R` passes
- `tests/testthat/test-cocktail-plot.R` passes
- `devtools::test(filter = 'cocktail-plot')` passes
- `tests/testthat/test-label-clusters.R` still has 3 reproducible failures in
  the validator-repair scenario around lines 253 and 315-320

Important note:

- earlier reports about `cocktail-plot` auto-load/speculative-label failures
  are not reproduced by the current direct test runs on July 7, 2026;
- this means the audit must include a reproducibility check, not just blind
  fixing.

## Status Legend

- `[ ]` not started
- `[~]` in progress
- `[x]` completed
- `[!]` blocked / needs decision

## Progress Checklist

- [ ] Step 1. Freeze baseline and create an audit ledger
- [ ] Step 2. Classify all code by ownership and allowed change level
- [ ] Step 3. Audit every modification inside original source files
- [ ] Step 4. Decide and apply file naming strategy for extension code
- [ ] Step 5. Find duplicated logic, stale branches, and dead assets
- [ ] Step 6. Audit public API surface and optionality guarantees
- [ ] Step 7. Reproduce and fix validator-repair test failures
- [ ] Step 8. Reproduce or close historical `cocktail-plot` failure claims
- [ ] Step 9. Refactor plotting/labeling seams to minimize core edits
- [ ] Step 10. Audit and tighten documentation and examples
- [ ] Step 11. Audit inline comments and internal developer notes
- [ ] Step 12. Final verification, cleanup, and upstream-minimality review

## Step 1. Freeze Baseline And Create An Audit Ledger

Goal:

- make future cleanup decisions against a fixed baseline, not memory.

Actions:

1. Record the current branch, current commit, and dirty worktree state.
2. Save a machine-readable inventory of:
   - files in `R/`
   - files in `temp/R_old/`
   - files in `tests/testthat/`
   - files in `man/`
3. Create an audit ledger file in `temp/` or `DEV_NOTES.md` with:
   - date
   - command used
   - result summary
   - open questions
4. Save diff summaries against `temp/R_old/` for original files.

Artifacts:

- one audit ledger note
- one inventory snapshot
- one diff summary for original files

Exit criteria:

- we can answer, from files not memory, what changed and when.

## Step 2. Classify All Code By Ownership And Allowed Change Level

Goal:

- separate true upstream code from extension code and mixed seam code.

Classification buckets:

- `upstream_core`: original project logic we should avoid modifying
- `extension_public`: new exported optional APIs
- `extension_internal`: helpers used only by extension features
- `seam_layer`: tiny compatibility hooks bridging upstream core and extension
- `docs_only`: files changed only for docs/indexing

Actions:

1. Build a one-row-per-file matrix for all files in `R/`.
2. Mark whether each file is:
   - from `R_old`
   - new
   - exported
   - test-covered
   - safe to rename
3. For each original file, assign a target rule:
   - `freeze`
   - `keep seam only`
   - `candidate for rollback + wrapper`
4. For each new file, assign a domain:
   - evidence
   - llm pipeline
   - plotting labels
   - semantic layer
   - validation/review

Artifacts:

- architecture matrix in markdown or CSV

Exit criteria:

- every source file has an owner category and a refactor rule.

## Step 3. Audit Every Modification Inside Original Source Files

Goal:

- reduce changes in old files to the minimum justified seam.

Current priority files:

- `R/cocktail_cluster.R`
- `R/cocktail_plot.R`
- `R/cocktailr-package.R`

Actions:

1. Review each diff against `temp/R_old/` line by line.
2. For each added behavior, classify it:
   - strictly necessary seam
   - optional convenience
   - extension logic that should move out
3. Make a keep/remove/move decision for every block.
4. Apply this rule aggressively:
   - if a block can live in a helper without changing public behavior,
     move it out;
   - if a block introduces extension-specific branching into old code,
     prefer wrapper extraction.
5. For `R/cocktail_cluster.R`, confirm whether dataset provenance is a
   justified seam or should move to a post-processing helper.
6. For `R/cocktail_plot.R`, confirm whether label-registry rendering should
   remain as a small seam or become a dedicated wrapper such as
   `cocktail_plot_labeled()`.
7. For `R/cocktailr-package.R`, keep only index-level doc changes if useful.

Decision rule:

- more than one non-trivial extension branch inside an old function is a sign
  we should extract a wrapper.

Artifacts:

- per-file keep/remove/move decisions
- follow-up task list for extraction

Exit criteria:

- every old-file modification is defended as either necessary or scheduled for
  removal/extraction.

## Step 4. Decide And Apply File Naming Strategy For Extension Code

Goal:

- make extension code visually separable in a flat `R/` directory.

Recommendation:

- do not force a blanket `llm_` prefix on everything;
- instead use domain prefixes because not all added code is strictly LLM code.

Preferred filename families:

- `cluster_evidence_*` for deterministic evidence-building layers
- `cluster_label_*` for label formatting/registry/prompt plumbing
- `llm_*` only for direct model-calling workflows
- `render_*` / `validate_*` only if tightly scoped and easy to find
- `semantic_*` for enrichment/scoring layers
- `*_plot_*` only for plotting-specific helpers

Actions:

1. Mark current filenames that are:
   - already good
   - ambiguous
   - too generic
2. Decide whether to rename:
   - `label_clusters.R`
   - `render_cluster_review.R`
   - `validate_cluster_label.R`
   - `generate_synthetic_vegetation_data.R`
   - any other ambiguous file
3. Rename only when it improves ownership clarity materially.
4. Keep public function names stable unless there is a strong reason to change.

Artifacts:

- filename decision table
- optional rename batch plan

Exit criteria:

- a reviewer can open `R/` and immediately understand which files are original
  and which belong to the extension layer.

## Step 5. Find Duplicated Logic, Stale Branches, And Dead Assets

Goal:

- shrink maintenance burden and remove legacy leftovers from iterative work.

Audit zones:

- `R/llm_label_cluster.R`
- `R/label_clusters.R`
- `R/render_cluster_review.R`
- `R/cluster_label_prompt_utils.R`
- `R/cluster_label_registry.R`
- `R/cluster_label_utils.R`
- `inst/prompts/internal_cluster_labeling/`
- `tests/testthat/`

Actions:

1. Search for repeated helper patterns:
   - label summary fallback assembly
   - path resolution
   - review metadata assembly
   - prompt version routing
   - label registry formatting
2. Search for dead branches:
   - legacy prompt versions no longer reachable
   - skipped explanation-stage remnants
   - fallback branches that no longer trigger
   - stale prompt filenames and README examples
3. Search for duplicated tests that assert the same behavior at different
   levels.
4. Remove or consolidate only after equivalent coverage exists elsewhere.

Artifacts:

- duplication report
- dead code report
- safe-delete candidate list

Exit criteria:

- we have a concrete list of code/assets to merge, move, or delete.

## Step 6. Audit Public API Surface And Optionality Guarantees

Goal:

- ensure the package still behaves like the original project unless optional
  features are explicitly used.

Public APIs to review:

- `cocktail_cluster()`
- `cocktail_plot()`
- `cluster_hclust_plot()`
- `label_hclust_leaves()`
- `cluster_evidence()`
- `llm_label_cluster()`
- `label_clusters()`
- `cluster_label_registry()`
- `render_cluster_review()`
- `validate_cluster_label()`

Actions:

1. List all exported functions added since `R_old`.
2. For each exported function, define:
   - whether it is optional
   - whether it depends on LLM infrastructure
   - whether it can be safely ignored by upstream users
3. For original exports that gained new arguments, verify:
   - defaults preserve old behavior
   - no LLM code runs unless explicitly requested
   - output structure changes are documented and backward-safe
4. Add regression tests for “extension off” behavior where missing.

Artifacts:

- API audit table
- missing optionality-test list

Exit criteria:

- we can state clearly that the package still supports the original workflow
  without extension features.

## Step 7. Reproduce And Fix Validator-Repair Test Failures

Goal:

- resolve the currently reproducible `validator repair` failures in
  `tests/testthat/test-label-clusters.R`.

Known failing area on July 7, 2026:

- `tests/testthat/test-label-clusters.R`
- test: `label_clusters uses text-only validator repair without falling back to JSON prompts`
- failing expectations around:
  - `state$saw_validator_repair`
  - `res$summary$repair_used[[1]]`
  - `res$results$c_1$llm_result$repair$source`

Actions:

1. Reproduce the failure with the smallest possible command.
2. Trace the control flow through:
   - `label_clusters()`
   - validator rejection path
   - repair request construction
   - repair result attachment
3. Identify whether the issue is:
   - the repair branch is no longer entered
   - the test fixture no longer triggers the branch
   - repair metadata is no longer attached
   - prompt-version migration changed behavior
4. Decide whether code or test is wrong.
5. Fix the behavior or update the test only after confirming the intended
   contract.
6. Add a smaller, lower-level regression test if the existing test is too
   brittle.

Artifacts:

- root-cause note
- code fix or test correction
- stable regression test

Exit criteria:

- the validator-repair contract is explicit and tested.

## Step 8. Reproduce Or Close Historical `cocktail-plot` Failure Claims

Goal:

- resolve the discrepancy between earlier failure reports and current passing
  runs.

Known current state on July 7, 2026:

- `tests/testthat/test-cocktail-plot.R` passes
- `devtools::test(filter = 'cocktail-plot')` passes

Historical claims to verify:

- auto-loaded registry failures
- speculative-label handling failures
- hclust-label auto-load failures

Actions:

1. Re-run all relevant commands in a clean R session:
   - `testthat::test_file('tests/testthat/test-cocktail-plot.R')`
   - `devtools::test(filter = 'cocktail-plot')`
   - full `devtools::test()` if needed
2. If failures reappear only in some modes, inspect:
   - state leakage across tests
   - temp-file reuse
   - registry auto-discovery order
   - tests depending on worktree contents
3. If failures do not reproduce, document them as resolved or stale claims.
4. If failures reproduce, fix root causes before changing expectations.

Artifacts:

- reproducibility note
- command matrix with pass/fail status
- fix list if needed

Exit criteria:

- there is one trusted answer to “does cocktail-plot currently fail?”

## Step 9. Refactor Plotting/Labeling Seams To Minimize Core Edits

Goal:

- move extension complexity out of original plotting code where possible.

Primary targets:

- `R/cocktail_plot.R`
- `R/cocktail_plot_label_utils.R`
- `R/cluster_hclust_plot.R`
- `R/label_hclust_leaves.R`
- `R/cluster_label_registry.R`

Actions:

1. Identify which plotting features truly need core hooks:
   - new optional argument
   - post-plot annotation
   - output path resolution
2. Move formatting/layout logic into helper files, not into old plot code.
3. If `cocktail_plot()` still carries too much extension behavior, introduce a
   wrapper approach:
   - keep `cocktail_plot()` mostly upstream-like
   - move labeled/report-aware orchestration into a new function
4. Apply the same rule to any future hclust-label enhancements.

Artifacts:

- seam reduction diff
- before/after map of plotting responsibilities

Exit criteria:

- original plotting files contain only small optional hooks, not extension
  subsystems.

## Step 10. Audit And Tighten Documentation And Examples

Goal:

- align docs with real package behavior and make optional features easy to use
  without confusion.

Documentation targets:

- roxygen comments in `R/`
- generated `.Rd` files in `man/`
- `README.md`
- `README.Rmd`
- `LABELING_STEP_BY_STEP.md`
- `LABELING_STEP_BY_STEP_RU_phi4_mini.md`
- prompt READMEs in `inst/prompts/`
- any plan/dev note that now serves as user-facing evidence

Actions:

1. Check every exported function for:
   - stale arguments
   - missing return-value docs
   - unclear optionality notes
   - examples that no longer run
2. Add short architecture notes where users need them:
   - what is optional
   - where outputs are written
   - how to inspect review artifacts
   - what files are auto-discovered
3. Make sure docs explain provenance additions such as dataset metadata and
   label registry files.
4. Regenerate docs and verify no accidental export drift.

Artifacts:

- updated roxygen
- regenerated `man/*.Rd`
- corrected step-by-step guides

Exit criteria:

- user docs, Rd docs, and actual behavior match.

## Step 11. Audit Inline Comments And Internal Developer Notes

Goal:

- keep code understandable without turning files into comment noise.

Actions:

1. Review large new files for missing explanation around non-obvious logic:
   - staged workflow assembly
   - fallback ladders
   - registry public/private label derivation
   - plotting layout math
2. Remove comments that merely restate code.
3. Add short comments ahead of blocks that future maintainers would struggle to
   infer quickly.
4. Consolidate scattered rationale into one or two developer-facing notes if a
   topic keeps recurring.

Artifacts:

- targeted comment cleanup
- concise developer notes where justified

Exit criteria:

- hard parts are explained once, clearly, and in the right place.

## Step 12. Final Verification, Cleanup, And Upstream-Minimality Review

Goal:

- end with a cleaner extension layer, passing tests, and a clear statement of
  what still touches upstream code.

Actions:

1. Run:
   - `devtools::document()`
   - targeted `testthat::test_file(...)`
   - `devtools::test()`
   - optional `devtools::check()` if time permits
2. Re-compare current `R/` against `temp/R_old/`.
3. Confirm the final list of original files still intentionally changed.
4. Delete dead prompts, stale helpers, and temporary audit scripts.
5. Write a short summary:
   - what stayed in original files
   - what moved out
   - what remains as technical debt

Artifacts:

- final verification note
- final upstream-minimality summary

Exit criteria:

- we can explain exactly why each remaining core-file edit exists.

## Suggested Execution Order

Recommended order for real work:

1. Step 1
2. Step 2
3. Step 3
4. Step 7
5. Step 8
6. Step 9
7. Step 5
8. Step 6
9. Step 10
10. Step 11
11. Step 12

Why this order:

- first freeze the facts;
- then reduce uncertainty around ownership and test failures;
- only then do larger cleanup/refactor work;
- documentation comes after behavior is stable.

## Decision Notes To Revisit During Audit

- whether `cocktail_plot()` should remain directly label-aware or gain a wrapper
  such as `cocktail_plot_labeled()`
- whether dataset provenance belongs in `cocktail_cluster()` or in a wrapper
  around it
- whether `render_cluster_review()` and `validate_cluster_label()` should be
  renamed for clearer extension ownership
- whether all new user-facing exports are justified as exports
- whether some new helpers should be merged or made internal-only

## Resume Template

When resuming after interruption, update this section:

- Last completed step:
- Current branch:
- Current commit:
- Current failing tests:
- Next concrete action:
- Open decision needing confirmation:
