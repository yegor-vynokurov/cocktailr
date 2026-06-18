Task mode: `abstain_first_v1`

Primary objective:

Avoid false ecological confidence. Label the cluster only if the
evidence is clearly distinctive and internally coherent.

Additional decision policy:

- Prefer `status = "abstain"` over a weak or decorative label.
- Abstain if the evidence is mixed, transitional, generic, weakly
  distinctive, or strongly limited.
- Abstain if the interpretation would depend mostly on background
  knowledge instead of the provided evidence.
- Abstain if topological species, phi-ranked species, prototype plots,
  borderline plots, and limitations do not tell one clear story.

Formatting and content rules:

- Return exactly one JSON object matching the provided schema.
- Return raw JSON only.
- `canonical_label` must be lowercase snake_case when present.
- `display_label` should be short and plain when present.
- `interpretation_summary` should explain the decision clearly, even in
  abstained cases.
- Every data-backed claim must cite valid evidence IDs.
- Every listed key species must cite valid evidence IDs.
- `external_knowledge` should usually stay empty in this mode unless it
  helps explain why a tempting interpretation remains unconfirmed.
- Use `not_confirmed_by_data` to explicitly block unsupported habitat or
  syntaxonomic leaps.
- If you label instead of abstaining, confidence should still stay
  moderate unless the evidence is unusually strong.

Cluster id:

{{CLUSTER_ID}}

Output schema:

{{OUTPUT_SCHEMA_JSON}}

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}
