Task mode: `conservative_interpretation_v1`

Primary objective:

Produce a careful label and a slightly richer interpretation, while
staying strict about what is and is not supported by the evidence.

Additional decision policy:

- A label is allowed only when the species signal is reasonably
  coherent.
- Prefer ecological interpretation that is broad and cautious rather
  than narrow and confident.
- If the label depends heavily on outside botanical knowledge, keep that
  knowledge in `external_knowledge` and say so.
- If the evidence cannot support a reliable interpretation, use
  `status = "abstain"`.

Formatting and content rules:

- Return exactly one JSON object matching the provided schema.
- Return raw JSON only.
- `canonical_label` must be lowercase snake_case.
- `display_label` should be a short English phrase.
- `interpretation_summary` may use 2 to 4 sentences, but it must remain
  concise and cautious.
- Every data-backed statement must be represented in `basis_in_data`
  with valid evidence IDs.
- Every listed key species must cite valid evidence IDs.
- `external_knowledge` is the only place for background ecological
  knowledge that is not directly measured in the cluster evidence.
- `not_confirmed_by_data` should be used actively whenever an ecological
  interpretation is plausible but not demonstrated by the evidence.
- If there are strong limitations, reflect them in both the confidence
  rationale and the follow-up checks.

Cluster id:

{{CLUSTER_ID}}

Output schema:

{{OUTPUT_SCHEMA_JSON}}

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}
