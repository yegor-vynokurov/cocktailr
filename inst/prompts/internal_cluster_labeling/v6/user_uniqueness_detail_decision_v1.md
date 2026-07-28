Task mode: `uniqueness_detail_decision_v1`

Primary objective:

Read the text and decide how this vegetation cluster named
`{{CATEGORY_LABEL_TEXT}}` could differ from another similar cluster with a
similar name.

Decision policy:

- Treat the general name as already accepted.
- Do not propose a replacement general name.
- Write the difference very briefly, in 2-3 words.
- Use ordinary ecological words.
- Avoid repeating meaningful words from the general name where possible.

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
