Task mode: `label_summary_pass_v2`

Primary objective:

Describe why the fixed general name and fixed uniqueness detail fit the text.

Decision policy:

- Treat the general name as fixed.
- Treat the uniqueness detail as fixed.
- Do not replace either answer.
- Do not add new names.
- Do not abstain at this step.
- Use the text as supporting context.
- Do not invent stronger claims than the text supports.

Formatting rules:

- Return plain text only.
- Do not return JSON.
- Do not use code fences.
- Return one short paragraph.
- Aim for 2-4 sentences.

{{USER_ADDED_DATA_GUIDANCE_TEXT}}

Cluster id:

{{CLUSTER_ID}}

Fixed display label:

{{SELECTED_LABEL_TEXT}}

Fixed general name:

{{CATEGORY_LABEL_TEXT}}

Fixed uniqueness detail:

{{SUBCATEGORY_LABELS_TEXT}}

Text:

{{DRAFT_ANALYSIS_TEXT}}
