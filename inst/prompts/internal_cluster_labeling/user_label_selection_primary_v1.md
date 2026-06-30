Task mode: `label_selection_primary_v1`

Primary objective:

Choose one short defensible label only if the evidence clearly supports
it. Otherwise abstain.

Decision policy:

- Start from a presumption of abstention.
- Prefer abstention over a weak or over-claimed label.
- Use the draft analysis to reuse the best candidate instead of
  inventing a new complex label.
- If you label, keep it broad, plain, and evidence-safe.
- Transitional or mixed clusters should usually abstain in this pass.

Formatting rules:

- Return exactly one JSON object matching the provided schema.
- Return raw JSON only.
- `display_label` must be short and plain.
- `canonical_label` must be lowercase snake_case.
- `label_summary` should be brief and should explain why the chosen
  label is safe, or why abstention is necessary.

{{LABEL_MODE_GUIDANCE_TEXT}}

Cluster id:

{{CLUSTER_ID}}

Output schema:

{{OUTPUT_SCHEMA_JSON}}

Draft analysis:

{{DRAFT_ANALYSIS_TEXT}}

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}
