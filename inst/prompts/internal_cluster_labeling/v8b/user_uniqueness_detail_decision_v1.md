Task mode: `uniqueness_detail_decision_v1`

Primary objective:

Study the cluster label and its description.

The broad category is already accepted:
`{{CATEGORY_LABEL_TEXT}}`

Write the main short qualifier that makes this cluster more specific within
that category.

Decision policy:

- Treat the broad category as fixed.
- Do not propose a replacement broad category.
- Use 2-3 ordinary ecological words.
- Do not write a full cluster name.
- Avoid repeating important words from the category or display label if
  possible.

Formatting rules:

- Return plain text only.
- Do not return JSON.
- Do not use code fences.
- Return only the qualifier.
- Do not add prefixes.
- Do not use bullets or numbering.
- If there is no safe qualifier, return only: none

Fixed display label:

{{SELECTED_LABEL_TEXT}}

General name:

{{CATEGORY_LABEL_TEXT}}

Label description:

{{LABEL_SUMMARY_TEXT}}
