# Cluster Labeling Prompts

This directory is intentionally small.

The public labeling interface is the three-prompt cascade:

- `label_primary_v1`
- `label_soft_v1`
- `label_broad_v1`

These map to:

- `user_label_primary_v1.md`
- `user_label_soft_v1.md`
- `user_label_broad_v1.md`

## What stays here

- `catalog.json`
- `system_scientific_caution_v1.md`
- the three public label prompts
- `vocabulary/coarse_label_vocabulary_core_v1.json` for current constrained mode runs

The two-step gate prompt is still packaged, but it is now treated as an
internal service asset rather than a public prompt choice.

## Public usage

For normal work, use:

- `variant = "label_primary_v1"` for the main cautious pass
- `label_soft_v1` as the first fallback
- `label_broad_v1` as the second fallback
- `label_mode = "open"` as the default free-label setting
- `label_mode = "constrained"` when the model should choose from the
  packaged coarse vocabulary or a user override
- `label_mode = "dynamic"` together with `workflow_steps = 3` when stage-B
  label selection should reuse short candidates proposed by stage A

The catalog also accepts older prompt IDs as compatibility aliases, but
they resolve to the current public trio.

## Migration Notes

Do not choose among the old `v1-v9` public prompt experiments manually
anymore.

The supported public surface is now only:

- `label_primary_v1`
- `label_soft_v1`
- `label_broad_v1`

Legacy prompt IDs are still accepted for compatibility, but they now
collapse into that trio:

- `concise_label_v1`, `abstain_first_v1`,
  `strict_abstention_gate_v1` -> `label_primary_v1`
- `conservative_interpretation_v1`, `speculative_fallback_v1`,
  `speculative_fallback_v2`, `speculative_fallback_v3`,
  `speculative_fallback_v6`, `speculative_fallback_v8` ->
  `label_soft_v1`
- `speculative_fallback_v4`, `speculative_fallback_v5`,
  `speculative_fallback_v7`, `speculative_fallback_v9` ->
  `label_broad_v1`

Supporting service prompts now live under:

- `inst/prompts/internal_cluster_labeling/`

Archived copies of the retired public prompt texts now live under:

- `temp/prompt_archive/cluster_labeling/`

## Experiments

When you want to try a new prompt:

1. Copy one of the three public prompt files.
2. Iterate locally.
3. Keep only the prompts that become part of the supported public
   interface.

Old or failed prompt experiments should be moved to:

- `temp/prompt_archive/cluster_labeling/`

That folder is git-ignored on purpose, so the repository surface stays
small and the package does not accidentally ship retired experiments.
