EMERGENCY LABEL RESCUE PASS

The strict cluster-labeling workflow did not produce an accepted stable label
for cluster `{{CLUSTER_ID}}`.

This pass must not leave the cluster unlabeled. Return the broadest useful
label you can justify from the evidence, even if it is only heuristic.

Return one raw JSON object that matches this schema exactly:

```json
{{OUTPUT_SCHEMA_JSON}}
```

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}

Hard rules:

1. Return `status = "labeled"`.
2. If the evidence is clear, choose the best broad label.
3. If the evidence is mixed, choose the least specific mixed or transition
   label.
4. If the evidence is very weak or noisy, still return a generic orientation
   label and explain the uncertainty in `interpretation_summary` and
   `not_confirmed_by_data`.
5. Keep `basis_in_data` and `key_species` evidence-backed.
6. Keep `external_knowledge` empty unless clearly separated and truly needed.
7. Do not add markdown, commentary, or code fences.

Schema-critical rules for this rescue pass:

- `canonical_label` must be lowercase snake_case and non-null
- `display_label` must be human-readable plain English and non-null
- `display_label` must not be snake_case
- `abstain_reason` must be `null`
- Keep the broad rescue label in `canonical_label`, and convert it into a
  readable phrase for `display_label`

Preferred generic fallback labels:

- mixed_generalist_assemblage
- transition_edge_assemblage
- mixed_woodland_pattern
- wet_graminoid_herb_assemblage
- dry_forb_grass_assemblage
- ruderal_mixed_assemblage
- chaotic_plant_assemblage

If you are unsure, choose the least specific label from the list above that
still matches the evidence direction.

Example formatting:

- `canonical_label = "mixed_generalist_assemblage"`
- `display_label = "Mixed Generalist Assemblage"`

Return JSON only.
