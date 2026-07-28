Task mode: `label_summary_pass_v2`

Primary objective:

Describe what the already chosen short label means in compact plain
text.

Decision policy:

- Treat the chosen label as fixed.
- Do not replace it with a different label.
- Do not abstain at this step.
- Use the brainstorm as supporting context.
- Do not invent stronger claims than the brainstorm supports.

Formatting rules:

- Return plain text only.
- Do not return JSON.
- Do not use code fences.
- Return one short paragraph.
- Aim for 2-4 sentences.
- Do not restate the full brainstorm.
- Do not output a new label list.

{{USER_ADDED_DATA_GUIDANCE_TEXT}}

Cluster id:

{{CLUSTER_ID}}

Chosen short label (fixed; do not replace it):

{{SELECTED_LABEL_TEXT}}

Brainstorm output:

{{DRAFT_ANALYSIS_TEXT}}
