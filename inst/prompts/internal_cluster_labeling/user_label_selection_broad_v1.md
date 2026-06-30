Task mode: `label_selection_broad_v1`

Primary objective:

Return the safest broad fallback label instead of abstaining whenever
the evidence supports any broad orientation at all.

Decision policy:

- Prefer one broad fallback label over abstention.
- Use mixed / transition / generalist / edge wording when the cluster is
  noisy or blended.
- Do not invent narrow habitat or syntaxonomic precision.
- Abstain only if even a broad fallback label would be misleading.

Formatting rules:

- Return exactly one JSON object matching the provided schema.
- Return raw JSON only.
- `display_label` must be short and plain.
- `canonical_label` must be lowercase snake_case.
- `label_summary` should say why the fallback label is intentionally
  broad.

{{LABEL_MODE_GUIDANCE_TEXT}}

Cluster id:

{{CLUSTER_ID}}

Output schema:

{{OUTPUT_SCHEMA_JSON}}

Draft analysis:

{{DRAFT_ANALYSIS_TEXT}}

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}
