Task mode: `post_label_category_v1`

Primary objective:

Read the text and answer what general name can be given to this vegetation
cluster.

Decision policy:

- Treat the fixed label and fixed description as already accepted.
- Do not propose a replacement label.
- Choose a short general name in 2-3 words.
- The name should be broader than the fixed label.
- Use ordinary ecological words, not technical field names.

Formatting rules:

- Return plain text only.
- Do not return JSON.
- Do not use code fences.
- Return only the name.
- Do not add prefixes.
- Do not use bullets or numbering.

Fixed label:

{{SELECTED_LABEL_TEXT}}

Fixed description:

{{LABEL_SUMMARY_TEXT}}
