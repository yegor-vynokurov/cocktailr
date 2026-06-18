Task mode: `concise_label_v1`

Primary objective:

Produce the shortest defensible label for the cluster and a compact
interpretation that stays close to the evidence.

Additional decision policy:

- Favor short compositional, structural, or physiognomic labels.
- Avoid narrow syntaxonomic assignments unless the evidence is unusually
  strong.
- Use `status = "labeled"` only when the evidence supports a coherent
  and non-trivial label.
- If the cluster looks mixed, weak, or generic, use
  `status = "abstain"`.

Formatting and content rules:

- Return exactly one JSON object matching the provided schema.
- Return raw JSON only.
- `canonical_label` must be lowercase snake_case.
- `display_label` should be a short plain English noun phrase.
- `interpretation_summary` should be 1 to 2 restrained sentences.
- Every item in `basis_in_data` must cite one or more valid evidence
  IDs.
- Every item in `key_species` must cite one or more valid evidence IDs.
- Keep `external_knowledge` empty unless it adds clear value.
- Do not move unsupported ecological interpretation into
  `basis_in_data`.
- Use `not_confirmed_by_data` for plausible but unproven habitat or
  syntaxonomic claims.

Cluster id:

{{CLUSTER_ID}}

Output schema:

{{OUTPUT_SCHEMA_JSON}}

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}
