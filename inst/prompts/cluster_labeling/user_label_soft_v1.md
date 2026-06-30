Task mode: `label_soft_v1`

Primary objective:

Produce the least specific useful orientation label that still stays
anchored in the provided evidence.

Decision policy:

- If there is any coherent signal at all, prefer a broad
  `status = "labeled"` answer over abstention.
- Use broad compositional, structural, or physiognomic wording rather
  than narrow habitat claims.
- If several stories are possible, choose the least specific one that is
  still useful.
- Abstain only if the evidence is so contradictory that you cannot even
  propose a broad pattern.

Formatting and content rules:

- Return exactly one JSON object matching the provided schema.
- Return raw JSON only.
- `canonical_label` must be lowercase snake_case when present.
- `display_label` should be short and plain when present.
- `interpretation_summary` should explicitly say when the answer is
  broad, tentative, or limited by evidence.
- Keep `basis_in_data` evidence-backed and keep `external_knowledge`
  empty unless clearly separated.
- Do not invent precision to avoid abstention.
- If `status = "abstain"`, keep `canonical_label` and `display_label`
  null.

{{LABEL_MODE_GUIDANCE_TEXT}}

Broad label styles that are acceptable here:

- "woodland-like assemblage"
- "wet graminoid-herb assemblage"
- "dry forb-grass assemblage"
- "mixed meadow assemblage"
- "transition edge assemblage"
- "ruderal-like assemblage"

Cluster id:

{{CLUSTER_ID}}

Output schema:

{{OUTPUT_SCHEMA_JSON}}

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}
