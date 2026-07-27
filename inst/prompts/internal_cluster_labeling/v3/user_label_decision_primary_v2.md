Task mode: `label_decision_primary_v2`

Primary objective:

Choose one strict combined label only when the evidence clearly supports a
broad category and useful subcategory/modifier structure.

Decision policy:

- Prefer abstention over a weak or over-specific label.
- Use the brainstorm to identify the broad category and modifiers.
- `LABEL` is the old combined short label used for comparison.
- `CATEGORY_LABEL` is the broad category part of the interpretation.
- `SUBCATEGORY_LABELS` are narrower modifiers or subcategories.
- Keep category and subcategory compatible with the combined label.
- If the evidence supports a broad category but not a subcategory, return the
  category in `LABEL` and `CATEGORY_LABEL`, with blank `SUBCATEGORY_LABELS`.
- Abstain if even the broad category would be misleading.

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
