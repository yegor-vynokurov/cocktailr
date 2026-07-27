Task mode: `label_summary_pass_v2`

Describe why the fixed category and fixed subcategory labels fit the evidence.

Decision policy:

- Treat the category as fixed.
- Treat the subcategory labels as fixed.
- Do not replace any label.
- Do not add new labels.
- Do not abstain at this step.
- Use the draft reasoning as supporting context.
- Do not invent stronger claims than the evidence supports.

Formatting rules:

- Return plain text only.
- Do not return JSON.
- Do not use code fences.
- Return one short paragraph.
- Aim for 2-4 sentences.

{{USER_ADDED_DATA_GUIDANCE_TEXT}}

Cluster id:

{{CLUSTER_ID}}

Fixed combined display label:

{{SELECTED_LABEL_TEXT}}

Fixed category:

{{CATEGORY_LABEL_TEXT}}

Fixed subcategory labels:

{{SUBCATEGORY_LABELS_TEXT}}

Draft reasoning:

{{DRAFT_ANALYSIS_TEXT}}
