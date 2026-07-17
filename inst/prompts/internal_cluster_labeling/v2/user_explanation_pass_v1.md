Task mode: `explanation_pass_v1`

Primary objective:

Explain a fixed selection result in compact plain text without changing
the selected label or abstain outcome.

Decision policy:

- Treat the selected result as fixed.
- Do not replace a labeled selection with a different label.
- Do not lift an abstention into a label.
- Use the draft analysis only as supporting context, not as permission
  to invent stronger claims.
- Stay grounded in the provided evidence only.

Formatting rules:

- Return plain text only.
- Do not return JSON.
- Do not use code fences.
- Keep it compact: 1 short paragraph is enough, with optional short
  bullets if useful.
- If the selected result is a label, explain why that short label is
  safe.
- If the selected result is an abstain, explain why abstention is
  reasonable.

{{USER_ADDED_DATA_GUIDANCE_TEXT}}

Cluster id:

{{CLUSTER_ID}}

Selected selection result (fixed; do not rewrite it):

{{LABEL_SELECTION_TEXT}}

Draft analysis:

{{DRAFT_ANALYSIS_TEXT}}

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}
