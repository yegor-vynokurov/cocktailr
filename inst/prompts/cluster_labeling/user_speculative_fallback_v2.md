SPECULATIVE FALLBACK PASS

The strict cluster-labeling workflow did not produce an accepted stable label
for cluster `{{CLUSTER_ID}}`.

Your job in this fallback pass is not to pretend certainty. Your job is to
provide the most cautious useful hypothesis that is still anchored in the
evidence, if such a hypothesis is possible.

You must still return one raw JSON object that matches this schema exactly:

```json
{{OUTPUT_SCHEMA_JSON}}
```

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}

Instructions for this fallback pass:

1. Use the evidence first. Do not invent habitats, regions, soils, or ecology
   that are not supported by the evidence bundle.
2. If there is some directional evidence, you may return `status = "labeled"`
   with a short tentative label.
3. Prefer compositional, structural, physiognomic, or broad moisture/light
   wording over strong habitat claims.
4. If you return `status = "labeled"`, make `interpretation_summary` explain
   both the directional signal and the main limitation.
5. If you return `status = "labeled"`, explicitly fill
   `not_confirmed_by_data` with what is still missing.
6. Use `checks_to_run` to suggest how the tentative label could be verified or
   rejected.
7. If there is no directional evidence at all, return `status = "abstain"`
   rather than guessing.
8. Do not add markdown, commentary, or code fences.

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
- if `status = "labeled"`, then `canonical_label` and `display_label` must both
  be non-null and `abstain_reason` must be `null`
- if `status = "abstain"`, then `canonical_label` and `display_label` must be
  `null` and `abstain_reason` must be a non-empty string
- `basis_in_data` must remain evidence-backed
- `external_knowledge` should stay empty unless clearly separated and genuinely
  needed

Return JSON only.
