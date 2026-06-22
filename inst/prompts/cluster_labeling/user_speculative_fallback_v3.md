BEST-EFFORT ORIENTATION PASS

The strict cluster-labeling workflow did not produce an accepted stable label
for cluster `{{CLUSTER_ID}}`.

In this pass, prefer a broad useful orientation label over abstention whenever
the evidence points in any coherent direction.

Return one raw JSON object that matches this schema exactly:

```json
{{OUTPUT_SCHEMA_JSON}}
```

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}

Rules:

1. Stay anchored in the evidence bundle.
2. If there is any coherent signal at all, prefer `status = "labeled"`.
3. Use broad labels rather than narrow ecological claims.
4. Abstain only if the evidence is so contradictory that you cannot even
   propose a broad pattern.
5. When uncertain, say so in `interpretation_summary` and
   `not_confirmed_by_data`, not by inventing precision.
6. Keep `basis_in_data` evidence-backed and keep `external_knowledge` empty
   unless clearly separated.
7. Do not add markdown, commentary, or code fences.

Schema-critical rules:

- if `status = "labeled"`, `canonical_label` must be lowercase snake_case
- if `status = "labeled"`, `display_label` must be a short human-readable
  phrase, not snake_case
- if `status = "labeled"`, `abstain_reason` must be `null`
- if `status = "abstain"`, `canonical_label` and `display_label` must be
  `null`, and `abstain_reason` must be non-empty

Broad label styles that are acceptable here:

- "woodland-like assemblage"
- "wet graminoid-herb assemblage"
- "dry forb-grass assemblage"
- "mixed meadow assemblage"
- "transition edge assemblage"
- "ruderal-like assemblage"

If several stories are possible, choose the least specific one that is still
useful.

Formatting example for a labeled answer:

- `canonical_label = "mixed_meadow_assemblage"`
- `display_label = "Mixed Meadow Assemblage"`

Return JSON only.
