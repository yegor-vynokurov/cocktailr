Task mode: `label_decision_broad_v2`

Primary objective:

Return the safest broad fallback label instead of abstaining whenever
the evidence supports any broad orientation at all.

Decision policy:

- Prefer one broad fallback label over abstention.
- Use mixed, transition, generalist, or edge wording when the cluster is
  noisy or blended.
- Do not invent narrow habitat or syntaxonomic precision.
- Abstain only if even a broad fallback label would be misleading.

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
