Task mode: `label_decision_soft_v2`

Primary objective:

Choose one broad useful combined label when the brainstorm points in one
direction, and also expose its broad category and narrower modifiers.

Decision policy:

- Prefer a broad useful label over abstention when there is a clear directional
  signal.
- Choose the least specific category that still says something useful.
- Use subcategories only when they are supported by the evidence or cautious
  ecological inference.
- Use mixed, transition, edge, or generalist wording when the signal is blended.
- Abstain only if the evidence is too contradictory even for a broad category.

Formatting rules:

- Return plain text only.
- Do not return JSON.
- Do not use code fences.
- Return exactly these three fields, one per line.
- Keep `LABEL` short, about 4-8 words.
- Keep `CATEGORY_LABEL` short, usually 2-4 words.
- Separate multiple `SUBCATEGORY_LABELS` with semicolons.
- Do not explain the decision.
- Do not return `canonical_label`, `display_label`, `label_summary`, or
  `abstain_reason`.
- Do not use words `cluster` or `clusters` in labels.
- If you abstain, return:

LABEL: ABSTAIN
CATEGORY_LABEL:
SUBCATEGORY_LABELS:

{{LABEL_MODE_GUIDANCE_TEXT}}

{{USER_ADDED_DATA_GUIDANCE_TEXT}}

Cluster id:

{{CLUSTER_ID}}

Brainstorm output:

{{DRAFT_ANALYSIS_TEXT}}

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}
