Task mode: `uniqueness_detail_decision_v1`

Primary objective:

Study the cluster label, its description, and the two brainstorm drafts.
Use the drafts only as supporting context for the subgroup decision.

The broad group name is already accepted:
`{{CATEGORY_LABEL_TEXT}}`

Two brainstorm drafts:

{{DRAFT_ANALYSIS_TEXT}}

Choose a short subgroup name for this cluster.

Decision policy:

- Treat the broad group name as fixed.
- Do not propose a replacement broad group name.
- The subgroup name should capture the main ecological detail that makes this
  cluster more specific inside the broad group.
- Use 2-3 ordinary ecological words.
- Do not write a full cluster name.
- Avoid repeating important words from the broad group name or display label if
  possible.

Formatting rules:

- Return plain text only.
- Do not return JSON.
- Do not use code fences.
- Return only the subgroup name.
- Do not add prefixes.
- Do not use bullets or numbering.
- If there is no safe subgroup name, return only: none

Fixed display label:

{{SELECTED_LABEL_TEXT}}

General name:

{{CATEGORY_LABEL_TEXT}}

Label description:

{{LABEL_SUMMARY_TEXT}}
