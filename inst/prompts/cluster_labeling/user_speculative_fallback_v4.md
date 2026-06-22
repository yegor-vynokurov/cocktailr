LABEL-REQUIRED FALLBACK PASS

The strict cluster-labeling workflow did not produce an accepted stable label
for cluster `{{CLUSTER_ID}}`.

In this pass, your job is to return the least specific defensible label rather
than abstaining.

Return one raw JSON object that matches this schema exactly:

```json
{{OUTPUT_SCHEMA_JSON}}
```

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}

Rules:

1. Prefer `status = "labeled"` whenever the evidence supports any broad
   orientation at all.
2. Use broad compositional or physiognomic wording instead of narrow habitat
   claims.
3. If the cluster is mixed, transitional, or weak, still return a label such
   as a mixed, transition, generalist, or edge assemblage.
4. Use `not_confirmed_by_data` to explain why the label remains broad.
5. `interpretation_summary` should explicitly say that this is a broad
   best-effort label when evidence is weak.
6. Keep `basis_in_data` and `key_species` tied to valid evidence IDs.
7. Do not add markdown, commentary, or code fences.

Schema-critical rules:

- prefer `status = "labeled"`
- if `status = "labeled"`, `canonical_label` must be non-null lowercase
  snake_case
- if `status = "labeled"`, `display_label` must be non-null short plain
  English, not snake_case
- if `status = "labeled"`, `abstain_reason` must be `null`
- if `status = "abstain"`, `canonical_label` and `display_label` must be
  `null`, and `abstain_reason` must be non-empty

You may use broad families such as:

- woodland_like_assemblage
- wet_meadow_like_assemblage
- dry_meadow_like_assemblage
- mixed_herbaceous_assemblage
- transition_edge_assemblage
- generalist_assemblage
- ruderal_like_assemblage

Return JSON only.
