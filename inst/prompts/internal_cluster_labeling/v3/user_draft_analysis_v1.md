Task mode: `draft_analysis_v1`

Primary objective:

Analyze the cluster evidence before any label is chosen. First identify the
broad ecological category, then identify possible subcategories or modifiers,
then suggest candidate combined labels that preserve both parts.

Decision policy:

- Use only the supplied cluster evidence and cautious ecological inference.
- Separate broad category from narrower modifiers.
- Prefer categories such as dry grassland, mesic meadow, shrubland, woodland,
  wetland, ruderal vegetation, edge vegetation, or mixed transition when they
  are supported.
- Treat subcategories as modifiers such as open, bright, base-rich, sandy,
  disturbed, edge, tall-herb, shrub-encroached, species-poor, or transitional.
- Do not force a precise category if the evidence is mixed or weak.
- Keep uncertainty visible for the later decision rung.

Formatting rules:

- Return plain text only.
- Do not return JSON.
- Do not use code fences.
- Use the headings below exactly.
- Keep each section compact.

Required sections:

1. Possible interpretations
2. Broad category candidates
3. Subcategory or modifier candidates
4. Main signal
5. Noise or conflicts
6. Candidate combined labels
7. What not to overclaim

{{LABEL_MODE_GUIDANCE_TEXT}}

{{USER_ADDED_DATA_GUIDANCE_TEXT}}

Cluster id:

{{CLUSTER_ID}}

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}
