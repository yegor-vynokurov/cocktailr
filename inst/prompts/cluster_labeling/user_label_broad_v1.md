Task mode: `label_broad_v1`

Primary objective:

Return the least specific defensible label rather than abstaining
whenever the evidence supports any broad orientation at all.

Decision policy:

- Prefer `status = "labeled"` whenever the evidence supports any broad
  compositional or physiognomic story.
- Use broad wording instead of narrow habitat, syntaxonomic, or hidden
  environmental claims.
- If the cluster is mixed, transitional, weak, or noisy, still return a
  broad mixed / transition / generalist / edge label when possible.
- Use `not_confirmed_by_data` to explain why the label remains broad.
- Abstain only if even a broad fallback label would be misleading.

Formatting and content rules:

- Return exactly one JSON object matching the provided schema.
- Return raw JSON only.
- `canonical_label` must be lowercase snake_case when present.
- `display_label` should be short and plain when present.
- `interpretation_summary` should explicitly say when this is a broad
  best-effort label.
- Keep `basis_in_data` and `key_species` tied to valid evidence IDs.
- Do not add markdown, commentary, or code fences.

{{LABEL_MODE_GUIDANCE_TEXT}}

You may use broad families such as:

- woodland_like_assemblage
- wet_meadow_like_assemblage
- dry_meadow_like_assemblage
- mixed_herbaceous_assemblage
- transition_edge_assemblage
- generalist_assemblage
- ruderal_like_assemblage

Cluster id:

{{CLUSTER_ID}}

Output schema:

{{OUTPUT_SCHEMA_JSON}}

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}
