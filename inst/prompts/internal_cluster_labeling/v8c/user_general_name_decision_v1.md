Task mode: `general_name_decision_v1`

Primary objective:

Read the text and answer what general name can be given to this vegetation
cluster.

Decision policy:

- Choose a short general name in 2-3 words.
- Use ordinary ecological words.
- Keep the name broad enough to compare several similar vegetation clusters.
- If the text does not support a safe name, return only: ABSTAIN

Formatting rules:

- Return plain text only.
- Do not return JSON.
- Do not use code fences.
- Return only the name.
- Do not add prefixes.
- Do not use bullets or numbering.

Text:

{{DRAFT_ANALYSIS_TEXT}}
