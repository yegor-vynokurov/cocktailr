Task mode: `label_decision_primary_v2`

Primary objective:

Choose one short defensible label only if the evidence and brainstorm
clearly support it. Otherwise abstain.

Decision policy:

- Start from a presumption of abstention.
- Prefer abstention over a weak or over-claimed label.
- Reuse candidate labels from the brainstorm when possible.
- If you label, keep it broad, plain, and evidence-safe.
- Transitional or mixed clusters should usually abstain in this pass.

Formatting rules:

- Return plain text only.
- Do not return JSON.
- Do not use code fences.
- Return exactly one short answer.
- Target label length: 4–8 words, with about 6 words preferred.
- Do not write a sentence.
- Do not explain the label.
- If a candidate label is longer than 8 words, compress it to the shortest safe ecological label.
- Prefer habitat, structure, moisture, soil, light, or disturbance words over long species lists.
- Avoid species names in the label unless one diagnostic species is essential.
- Do not include more than one species name in the label.
- Avoid phrases like “with ... core” unless they are essential for a safe label.
- Do not return multiple fields.
- Do not return `canonical_label`, `display_label`, `label_summary`, or
  `abstain_reason`.
- Do not use words `cluster` or `clusters` in the label. 
- Return only the short label text in 4–8 words, with about 6 words preferred.
- If you abstain, return only: `ABSTAIN`

{{LABEL_MODE_GUIDANCE_TEXT}}

{{USER_ADDED_DATA_GUIDANCE_TEXT}}

Cluster id:

{{CLUSTER_ID}}

Brainstorm output:

{{DRAFT_ANALYSIS_TEXT}}

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}
