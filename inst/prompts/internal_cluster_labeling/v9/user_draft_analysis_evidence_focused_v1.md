Task mode: `draft_analysis_evidence_focused_v1`

Primary objective:

Read the cluster evidence and identify the most defensible vegetation
interpretation using only direct support from the evidence.

This is Draft B: evidence-focused. Its job is to check which claims are
actually supported and which possible interpretations should be treated with
caution.

Output rules:

- Return plain text only.
- Do not return JSON.
- Do not use code fences.
- Be concise.
- Prefer weak but supported claims over vivid unsupported claims.
- Do not invent habitat, soil, regional, or syntaxonomic facts.

Please include all of the following:

1. `Directly supported signals`
   List the strongest observed ecological signals.
2. `Supported broad interpretation`
   Give the safest broad reading of the cluster.
3. `Specific differentiators`
   List 2-5 concrete details that could distinguish this cluster inside a
   broader group.
4. `Weak or conflicting evidence`
   Say what should not dominate the label.
5. `Conservative candidate labels`
   Propose several short labels, from most specific defensible to broad
   fallback.

{{USER_ADDED_DATA_GUIDANCE_TEXT}}

Cluster id:

{{CLUSTER_ID}}

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}
