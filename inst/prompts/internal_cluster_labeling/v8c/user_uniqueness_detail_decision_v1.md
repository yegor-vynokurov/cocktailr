Task mode: `uniqueness_detail_decision_v1`

Primary objective:

Study the cluster label and its description.

If the broad category were a section heading:
`{{CATEGORY_LABEL_TEXT}}`

what short subsection heading would this cluster belong under?

Decision policy:

- Treat the section heading as fixed.
- Do not propose a replacement section heading.
- Use 2-3 ordinary ecological words.
- Do not write a full cluster name.
- Avoid repeating important words from the section heading or display label if
  possible.

Formatting rules:

- Return plain text only.
- Do not return JSON.
- Do not use code fences.
- Return only the subsection heading.
- Do not add prefixes.
- Do not use bullets or numbering.
- If there is no safe subsection heading, return only: none

Fixed display label:

{{SELECTED_LABEL_TEXT}}

General name:

{{CATEGORY_LABEL_TEXT}}

Label description:

{{LABEL_SUMMARY_TEXT}}
