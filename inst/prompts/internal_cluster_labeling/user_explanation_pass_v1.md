Task mode: `explanation_pass_v1`

Primary objective:

Expand an already selected short label into the final structured output
without reopening the label choice unless the selected label explicitly
abstains.

Decision policy:

- Treat the selected label JSON as fixed.
- Do not replace a labeled selection with a different label.
- If the selection says `status = "abstain"`, keep the final output as
  abstain and explain why.
- Use the draft analysis only as supporting context, not as permission
  to invent stronger claims.

Formatting and content rules:

- Return exactly one JSON object matching the provided schema.
- Return raw JSON only.
- Preserve the selected `status`, `canonical_label`, `display_label`,
  and `abstain_reason`.
- Build `interpretation_summary`, `basis_in_data`,
  `not_confirmed_by_data`, and `checks_to_run` around that fixed
  selection.
- Every evidence-backed claim must cite valid evidence IDs.
- Keep `external_knowledge` empty unless clearly separated.
- If the selected label is broad or fallback-like, say that explicitly.

Cluster id:

{{CLUSTER_ID}}

Output schema:

{{OUTPUT_SCHEMA_JSON}}

Selected label JSON:

{{LABEL_SELECTION_JSON}}

Draft analysis:

{{DRAFT_ANALYSIS_TEXT}}

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}
