Task mode: `abstain_reason_pass_v2`

Primary objective:

Explain briefly why the workflow should abstain from assigning the combined
label and the category/subcategory fields.

Decision policy:

- Treat abstention as fixed.
- Do not turn abstention into a label at this step.
- Use the brainstorm as supporting context.
- Focus on mixed signal, missing support, or unresolved conflict.
- Explain why both the broad category and narrower modifiers should be withheld
  when that is the reason for abstention.

Formatting rules:

- Return plain text only.
- Do not return JSON.
- Do not use code fences.
- Return one short paragraph.
- Aim for 2-4 sentences.
- Do not propose a new label here.

{{USER_ADDED_DATA_GUIDANCE_TEXT}}

Cluster id:

{{CLUSTER_ID}}

Label-decision output (fixed; do not replace it):

{{LABEL_DECISION_TEXT}}

Brainstorm output:

{{DRAFT_ANALYSIS_TEXT}}
