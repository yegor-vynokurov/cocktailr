Task mode: `label_decision_broad_v2`

Primary objective:

Return the safest broad fallback combined label whenever the evidence supports
any broad category, and expose that category plus any safe modifiers.

Decision policy:

- Prefer one broad fallback label over abstention.
- Do not invent narrow habitat or syntaxonomic precision.
- Use category labels that remain useful across mixed or noisy clusters.
- Use subcategories sparingly; leave them blank if they would be decorative.
- Abstain only if even a broad category would be misleading.

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
