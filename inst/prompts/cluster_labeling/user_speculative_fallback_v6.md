PHI4-ORIENTED SOFT RESCUE PASS

The strict cluster-labeling workflow did not produce an accepted stable label
for cluster `{{CLUSTER_ID}}`.

This pass is tuned for a small local model. Prefer one broad orientation label
whenever the evidence shows any directional pattern at all.

Return one raw JSON object that matches this schema exactly:

```json
{{OUTPUT_SCHEMA_JSON}}
```

Cluster evidence:

{{CLUSTER_EVIDENCE_TEXT}}

Work in this order:

1. Look at the topological species.
2. Look at the first 5 phi-ranked species.
3. Ask which single broad direction those species support best.
4. If one direction is visible, return `status = "labeled"`.
5. Abstain only if even the broadest orientation would be misleading.

Strong directional hints commonly seen in the current benchmark:

- tree + shade flora such as `Fagus`, `Quercus`, `Mercurialis`,
  `Dryopteris`, `Galium odoratum`, `Anemone`, `Hedera`
  -> usually `woodland_like_assemblage`
- wet graminoids / marsh-edge herbs such as `Carex acuta`,
  `Iris pseudacorus`, `Filipendula ulmaria`, `Caltha palustris`,
  `Juncus effusus`, `Mentha aquatica`, `Deschampsia cespitosa`,
  `Lychnis flos-cuculi`
  -> usually `wet_meadow_like_assemblage`
- dry grass-forb taxa such as `Festuca ovina`, `Thymus serpyllum`,
  `Galium verum`, `Lotus corniculatus`, `Plantago lanceolata`,
  `Salvia pratensis`, `Bromus erectus`, `Achillea millefolium`
  -> usually `dry_meadow_like_assemblage`
- if two directions are clearly mixed
  -> prefer `transition_edge_assemblage` or `mixed_generalist_assemblage`

Preferred canonical labels in this pass:

- `woodland_like_assemblage`
- `wet_meadow_like_assemblage`
- `dry_meadow_like_assemblage`
- `transition_edge_assemblage`
- `mixed_generalist_assemblage`
- `mixed_herbaceous_assemblage`
- `ruderal_like_assemblage`

Rules:

1. Stay anchored in the evidence bundle.
2. If topological species and the first phi-ranked species point in the same
   broad direction, do not call the cluster "mixed" and do not abstain.
3. Keep `basis_in_data` factual:
   species presence, phi ranking, prototype consistency, cover pattern.
4. Do not put habitat claims into `basis_in_data`.
5. Keep `external_knowledge` empty unless clearly separated and truly needed.
6. Keep `key_species` short and concrete: usually 2 to 4 items.
7. Keep `basis_in_data` short and concrete: usually 2 to 4 claims.
8. Keep `interpretation_summary` to 1 or 2 short sentences.
9. If the label contains words such as `woodland`, `meadow`, `wet`, `dry`,
   `ruderal`, or `edge`, include one matching statement in
   `not_confirmed_by_data` saying that this is only a coarse orientation from
   species composition, not a confirmed habitat assignment.
10. Do not add markdown, commentary, or code fences.

Schema-critical rules:

- if `status = "labeled"`, `canonical_label` must be lowercase snake_case
- if `status = "labeled"`, `display_label` must be a short human-readable
  phrase, not snake_case
- if `status = "labeled"`, `abstain_reason` must be `null`
- if `status = "abstain"`, `canonical_label` and `display_label` must be
  `null`, and `abstain_reason` must be non-empty

Return JSON only.
