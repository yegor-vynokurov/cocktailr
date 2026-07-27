Task mode: `abstain_reason_pass_v2`

Explain briefly why the workflow should abstain from assigning a category and
subcategory labels.

Decision policy:

- Treat abstention as fixed.
- Do not turn abstention into a label.
- Use the draft reasoning as supporting context.
- Focus on missing support, mixed signal, or unresolved conflict.

Formatting rules:

- Return plain text only.
- Do not return JSON.
- Do not use code fences.
- Return one short paragraph.
- Aim for 2-4 sentences.

{{USER_ADDED_DATA_GUIDANCE_TEXT}}

Cluster id:

{{CLUSTER_ID}}

Category decision output:

{{LABEL_DECISION_TEXT}}

Draft reasoning:

{{DRAFT_ANALYSIS_TEXT}}
