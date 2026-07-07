# Internal Cluster Labeling Prompts

This directory stores versioned internal service-prompt bundles for the
staged cluster-labeling workflow.

## Folder layout

- `v1/` is the current default production bundle.
- `v2/`, `v3/`, and later folders should be full copies of a previous
  version when you want to iterate safely.
- Switch the active bundle from R with
  `internal_prompt_version = "v2"` in `llm_label_cluster()` or
  `label_clusters()`.

## Active runtime flow

The current fixed pipeline uses these prompt roles:

1. `user_draft_analysis_v1.md`
2. One label-decision rung from:
   `user_label_decision_primary_v2.md`,
   `user_label_decision_soft_v2.md`,
   `user_label_decision_broad_v2.md`
3. One terminal explanation rung:
   `user_label_summary_pass_v2.md` for labeled output or
   `user_abstain_reason_pass_v2.md` for abstentions

The public prompt catalog in `../cluster_labeling/catalog.json` maps the
public `label_primary_v1` / `label_soft_v1` / `label_broad_v1` variants to
these internal service prompts.

## Files in each version folder

- `user_draft_analysis_v1.md`: brainstorm pass that externalizes
  interpretations, conflicts, and candidate labels.
- `user_label_decision_primary_v2.md`: strict label-only decision rung that
  prefers abstention over a weak label.
- `user_label_decision_soft_v2.md`: softer fallback rung that allows a broad
  directional label.
- `user_label_decision_broad_v2.md`: broadest fallback rung that still tries
  to avoid abstention when any safe orientation label is available.
- `user_label_summary_pass_v2.md`: explains a label that was already fixed by
  the decision rung.
- `user_abstain_reason_pass_v2.md`: explains an abstain result that was
  already fixed by the decision rung.
- `user_explanation_pass_v1.md`: compatibility asset for the older
  explanation-style flow and review provenance.
- `user_gate_abstain_examples_v1.md`: compatibility asset for the older
  gate-then-label flow.

## How to create a new version

1. Copy `v1/` to `v2/`.
2. Edit only the copied files inside `v2/`.
3. Run the workflow with `internal_prompt_version = "v2"`.
4. Promote the new bundle by changing the runtime argument after review.

This keeps the current production prompts reproducible while making prompt
experiments easy to diff and roll back.
