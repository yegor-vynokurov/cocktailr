PHI4-ORIENTED CONSTRAINED SOFT RESCUE PASS

The strict cluster-labeling workflow did not produce an accepted stable label
for cluster `{{CLUSTER_ID}}`.

This pass is constrained: if you label the cluster, you must choose from the
explicit coarse vocabulary below. Do not invent a new label.

Return one raw JSON object that matches this schema exactly:

```json
{{OUTPUT_SCHEMA_JSON}}
```

Allowed coarse label vocabulary:

{{COARSE_LABEL_VOCABULARY_TEXT}}

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}

Decision rule:

1. Read the topological species first.
2. Read the first 5 phi-ranked species.
3. Choose the single best-fitting label from the allowed vocabulary.
4. If one coarse label is reasonably defensible, return `status = "labeled"`.
5. Use `status = "abstain"` only if none of the listed labels can be defended
   even broadly.

Rules:

1. If you label the cluster, `canonical_label` and `display_label` must match
   one pair from the allowed vocabulary exactly.
2. Do not invent a new `canonical_label`.
3. Prefer a broad directional label over a fine habitat guess.
4. If woodland-like, wet-meadow-like, or dry-meadow-like signals are visible,
   do not switch to `mixed_generalist_assemblage` too early.
5. Use `transition_edge_assemblage` when two broad directions clearly overlap.
6. Use `mixed_generalist_assemblage` when the cluster is real but no stronger
   family is safer.
7. Keep `basis_in_data` factual and short.
8. Keep `external_knowledge` empty unless truly needed.
9. Keep `interpretation_summary` to 1 or 2 short sentences.
10. If you choose any ecological orientation label, add one statement in
    `not_confirmed_by_data` saying that this is only a coarse orientation from
    the species evidence, not a confirmed habitat assignment.
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
