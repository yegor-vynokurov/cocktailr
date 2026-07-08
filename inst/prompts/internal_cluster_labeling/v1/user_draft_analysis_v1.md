Task mode: `draft_analysis_v1`

Primary objective:

Read the cluster evidence and externalize intermediate reasoning before
any final label is chosen.

Output rules:

- Return plain text only.
- Do not return JSON.
- Do not use code fences.
- Use short sections with compact bullets.
- Stay grounded in the provided evidence only.
- Do not invent hidden habitat, soil, regional, or syntaxonomic facts.

Please include all of the following:

1. `Possible interpretations`
   Give 3-7 plausible broad readings of the cluster.
2. `Main signal`
   Say which broad story looks strongest right now.
3. `Noise or conflicts`
   Say what looks mixed, weak, transitional, or noisy.
4. `Candidate labels`
   Propose several short label ideas, including at least one very broad
   fallback.
5. `What not to overclaim`
   Note tempting but unsupported interpretations.

{{USER_ADDED_DATA_GUIDANCE_TEXT}}

Cluster id:

{{CLUSTER_ID}}

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}
