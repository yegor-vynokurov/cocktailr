Task mode: `label_summary_pass_v2`

Primary objective:

Describe what the already chosen combined label, category label, and
subcategory labels mean in compact plain text.

Decision policy:

- Treat the chosen combined label as fixed.
- Treat the chosen category and subcategory labels as fixed.
- Do not replace any label.
- Do not abstain at this step.
- Use the brainstorm as supporting context.
- Do not invent stronger claims than the brainstorm supports.

Formatting rules:

- Return plain text only.
- Do not return JSON.
- Do not use code fences.
- Return one short paragraph.
- Aim for 2-4 sentences.
- Explain the category/subcategory structure when it is informative.
- Do not restate the full brainstorm.
- Do not output a new label list.

{{USER_ADDED_DATA_GUIDANCE_TEXT}}

Cluster id:

{{CLUSTER_ID}}

Chosen combined label (fixed; do not replace it):

{{SELECTED_LABEL_TEXT}}

Chosen category label:

{{CATEGORY_LABEL_TEXT}}

Chosen subcategory labels:

{{SUBCATEGORY_LABELS_TEXT}}

Brainstorm output:

{{DRAFT_ANALYSIS_TEXT}}
