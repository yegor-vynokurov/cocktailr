Task mode: `explanation_pass_v1`

Primary objective:

Explain the selected label in compact plain text.

Decision policy:

- Treat the selected label as fixed.
- Do not replace the label.
- Do not abstain at this step.
- Use evidence and previous reasoning as supporting context.

Formatting rules:

- Return plain text only.
- Do not return JSON.
- Do not use code fences.
- Return one short paragraph.
- Aim for 2-4 sentences.

{{USER_ADDED_DATA_GUIDANCE_TEXT}}

Cluster id:

{{CLUSTER_ID}}

Selected label:

{{SELECTED_LABEL_TEXT}}

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}
