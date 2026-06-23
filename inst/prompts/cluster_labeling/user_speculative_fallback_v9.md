PHI4-ORIENTED CONSTRAINED LABEL-REQUIRED RESCUE PASS

The strict cluster-labeling workflow did not produce an accepted stable label
for cluster `{{CLUSTER_ID}}`.

This pass must return the safest useful label from the explicit coarse
vocabulary below. Do not invent a new label.

Return one raw JSON object that matches this schema exactly:

```json
{{OUTPUT_SCHEMA_JSON}}
```

Allowed coarse label vocabulary:

{{COARSE_LABEL_VOCABULARY_TEXT}}

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}

Decision rule:

1. Pick the strongest broad direction from the evidence.
2. Select one label from the allowed vocabulary.
3. If the evidence is mixed, prefer `transition_edge_assemblage`,
   `mixed_herbaceous_assemblage`, or `mixed_generalist_assemblage`.
4. If all other coarse labels are too strong, use `chaotic_plant_assemblage`
   instead of abstaining.

Rules:

1. Prefer `status = "labeled"`.
2. In ordinary cases, do not abstain.
3. If you label the cluster, `canonical_label` and `display_label` must match
   one pair from the allowed vocabulary exactly.
4. Do not invent a new label.
5. Keep `basis_in_data` factual and short.
6. Keep `external_knowledge` empty.
7. Keep `key_species` short and concrete.
8. Keep `interpretation_summary` short and explicitly cautious when the signal
   is weak or mixed.
9. Add at least one statement in `not_confirmed_by_data` saying what is still
   uncertain about the chosen coarse label.
10. If no directional family is strong enough, prefer
    `mixed_generalist_assemblage` or `chaotic_plant_assemblage` over abstention.
11. Do not add markdown, commentary, or code fences.

Schema-critical rules:

- if `status = "labeled"`, `canonical_label` must be non-null lowercase
  snake_case
- if `status = "labeled"`, `display_label` must be non-null short plain
  English, not snake_case
- if `status = "labeled"`, `abstain_reason` must be `null`
- if `status = "abstain"`, `canonical_label` and `display_label` must be
  `null`, and `abstain_reason` must be non-empty

Return JSON only.
