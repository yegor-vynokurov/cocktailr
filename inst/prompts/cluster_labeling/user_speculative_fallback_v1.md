SPECULATIVE FALLBACK PASS

The strict cluster-labeling workflow did not produce an accepted stable label for cluster `{{CLUSTER_ID}}`.

Your job in this fallback pass is not to pretend certainty. Your job is to provide the most cautious useful hypothesis that is still anchored in the evidence, if such a hypothesis is possible.

You must still return one raw JSON object that matches this schema exactly:

```json
{{OUTPUT_SCHEMA_JSON}}
```

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}

Instructions for this fallback pass:

1. Use the evidence first. Do not invent habitats, regions, soils, or ecology that are not supported by the evidence bundle.
2. If there is some directional evidence, you may return `status = "labeled"` with a short tentative label.
3. Prefer compositional or structural wording over strong habitat claims.
4. The tentative label should remain useful for a human reader, but it must sound cautious.
5. If you return `status = "labeled"`, set `confidence.score` to `0`.
6. If you return `status = "labeled"`, explicitly fill `not_confirmed_by_data` with what is missing for confidence.
7. Use `checks_to_run` to suggest how the tentative label could be verified or rejected.
8. If there is no directional evidence at all, return `status = "abstain"` rather than guessing.
9. Do not add markdown, commentary, or code fences.

Preferred style for tentative labels:

- "woodland-like assemblage"
- "possible wet meadow edge"
- "ruderal grassland hypothesis"
- "mesic herb-rich woodland pattern"

Avoid overconfident labels like:

- "alluvial floodplain meadow"
- "saline steppe vegetation"
- "confirmed ruderal clearing"

Additional constraints for `status = "labeled"` in this fallback pass:

- `display_label` should be short and human-readable
- `canonical_label` must stay lowercase snake_case
- `interpretation_summary` must explain both the weak signal and the limitation
- `basis_in_data` must remain evidence-backed
- `external_knowledge` should stay empty unless clearly separated and genuinely needed

Return JSON only.
