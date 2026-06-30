Task mode: `label_selection_soft_v1`

Primary objective:

Choose the least specific useful orientation label when the draft
analysis points in one direction, even if the evidence is not strong
enough for the strict primary pass.

Decision policy:

- Prefer a broad useful label over abstention when there is a directional
  signal.
- Reuse candidate labels from the draft analysis whenever possible.
- Choose the least specific label that still says something useful.
- Abstain only if the evidence is too contradictory even for a broad
  orientation label.

Formatting rules:

- Return exactly one JSON object matching the provided schema.
- Return raw JSON only.
- `display_label` must be short and plain.
- `canonical_label` must be lowercase snake_case.
- `label_summary` should clearly say when the label is broad or
  tentative.

{{LABEL_MODE_GUIDANCE_TEXT}}

Cluster id:

{{CLUSTER_ID}}

Output schema:

{{OUTPUT_SCHEMA_JSON}}

Draft analysis:

{{DRAFT_ANALYSIS_TEXT}}

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}
