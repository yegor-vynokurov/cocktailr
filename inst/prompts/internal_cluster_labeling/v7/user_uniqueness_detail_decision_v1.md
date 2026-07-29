Task mode: `uniqueness_detail_decision_v1`

Primary objective:

Read the text and decide how this vegetation cluster could differ from another
similar vegetation cluster with the same fixed general name:
`{{CATEGORY_LABEL_TEXT}}`.

Decision policy:

- Treat the general name as already accepted.
- Do not propose a replacement general name.
- Write the difference very briefly, in 2-3 words.
- Use ordinary ecological words.
- DRY means do not repeat yourself.
- Do not repeat meaningful words from the fixed display label.
- Do not repeat meaningful words from the fixed general name where possible.
- Write only the compact differentiating detail, not a replacement name or a
  mini-description.

Few-shot examples:

- Bakery fixed display label: crusty sourdough bread
- Bakery fixed general name: sourdough bread
- Good difference: extra crusty
- Bad difference: crusty sourdough bread

- Car fixed display label: compact electric city car
- Car fixed general name: electric car
- Good difference: compact city
- Bad difference: electric city car

Formatting rules:

- Return plain text only.
- Do not return JSON.
- Do not use code fences.
- Return only the short difference.
- Do not add prefixes.
- Do not use bullets or numbering.
- If there is no safe difference, return only: none

General name:

{{CATEGORY_LABEL_TEXT}}

Text:

{{DRAFT_ANALYSIS_TEXT}}
